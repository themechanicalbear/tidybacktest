imp_vol <- function() {
  # https://www.cboe.com/micro/vix/vixwhite.pdf
symbol <- "SPY"
min_day <- 1440
min_year <- min_day * 365 # 525600
min_month <- min_year / 12 # 43800
rates <- risk_free_rate()

option_data <- readRDS(paste0(here::here(), "/data/options/", symbol, ".RDS")) %>%
  dplyr::mutate(mid = (ask + bid) / 2) %>%
  dplyr::filter(dte >= 23) %>%
  dplyr::left_join(rates, by = c("quotedate" = "date"))

puts <- option_data %>%
  dplyr::filter(type == "put") %>%
  dplyr::mutate(put_mid_price = mid) %>%
  dplyr::group_by(quotedate, dte) %>%
  dplyr::arrange(desc(strike)) %>%
  dplyr::mutate(lead_1 = ifelse(is.na(lead(bid, 1)), 0, lead(bid, 1)),
                lead_2 = ifelse(is.na(lead(bid, 2)), 0, lead(bid, 2))) %>%
  dplyr::filter(bid > 0.00 |
                  lead_1 > 0.00 |
                  lead_2 > 0.00) %>%
  dplyr::ungroup() %>%
  dplyr::select(c(symbol, quotedate, close, expiration, strike, dte, wk_4_coupon, wk_13_coupon, put_mid_price))

calls <- option_data %>%
  dplyr::filter(type == "call") %>%
  dplyr::mutate(call_mid_price = mid) %>%
  dplyr::group_by(quotedate, dte) %>%
  dplyr::arrange(strike) %>%
  dplyr::mutate(lead_1 = ifelse(is.na(lead(bid, 1)), 0, lead(bid, 1)),
                lead_2 = ifelse(is.na(lead(bid, 2)), 0, lead(bid, 2))) %>%
  dplyr::filter(bid > 0.00 |
                  lead_1 > 0.00 |
                  lead_2 > 0.00) %>%
  dplyr::ungroup() %>%
  dplyr::select(c(symbol, quotedate, close, expiration, strike, dte, wk_4_coupon, wk_13_coupon, call_mid_price))

option_data <- puts %>%
  dplyr::left_join(calls, by = Reduce(intersect, list(names(puts), names(calls)))) %>%
  dplyr::group_by(quotedate) %>%
  dplyr::mutate(near_dte = min(dte)) %>%
  dplyr::mutate(near_dte_diff = dte - near_dte) %>%
  dplyr::filter(dte == near_dte |
                  near_dte_diff > 6) %>%
  dplyr::mutate(dte_rank = dense_rank(near_dte_diff)) %>%
  dplyr::filter(dte_rank <= 2) %>%
  dplyr::ungroup()

summary_output <- data.frame()

vol_calc <- function(dt) {
  sub_data <- filter(option_data, quotedate == dt)
  if (nrow(sub_data) > 0) {
    sub_data <- dplyr::mutate(sub_data, next_dte = max(dte))

    F_level_near <- sub_data %>%
      dplyr::filter(dte_rank == 1) %>%
      dplyr::mutate(p_c_abs = abs(call_mid_price - put_mid_price)) %>%
      dplyr::filter(p_c_abs == min(p_c_abs, na.rm = TRUE))

    F_level_next <- sub_data %>%
      dplyr::filter(dte_rank == 2) %>%
      dplyr::mutate(p_c_abs = abs(call_mid_price - put_mid_price)) %>%
      dplyr::filter(p_c_abs == min(p_c_abs, na.rm = TRUE))

    if (nrow(F_level_near) > 0 & nrow(F_level_next) > 0) {
      sub_data <- sub_data %>%
        dplyr::filter(dplyr::row_number() == 1) %>%
        dplyr::mutate(r1 = wk_4_coupon / 100,
                      r2 = wk_13_coupon / 100,
                      t1 = near_dte / 365,
                      t2 = next_dte / 365,
                      k1 = F_level_near[[1, "strike"]],
                      m1 = F_level_near[[1, "call_mid_price"]] - F_level_near[[1, "put_mid_price"]],
                      f1 = k1 + exp(r1 * t1) * m1,
                      k2 = F_level_next[[1, "strike"]],
                      m2 = F_level_next[[1, "call_mid_price"]] - F_level_next[[1, "put_mid_price"]],
                      f2 = k2 + exp(r2 * t2) * m2) %>%
        dplyr::select(symbol, quotedate, r1, r2, t1, t2, k1, k2, m1, m2, f1, f2)

      summary_output <- rbind(summary_output, sub_data)
      assign("summary_output", summary_output, envir = .GlobalEnv)
    }
  }
}

system.time(purrr::map(unique(option_data$quotedate), vol_calc))

option_data <- option_data %>%
  dplyr::left_join(summary_output, by = c("symbol", "quotedate")) %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::group_by(quotedate, dte) %>%
  dplyr::arrange(strike) %>%
  dplyr::mutate(strike_gap = dplyr::lead(strike, 1) - strike) %>%
  dplyr::mutate(strike_gap = ifelse(is.na(strike_gap), strike - dplyr::lag(strike, 1), strike_gap)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(near_strike_contrib =
                  ifelse(dte_rank == 1 & strike < f1,
                         (strike_gap/(strike ^ 2)) * exp(r1 * t1) * put_mid_price,
                         ifelse(dte_rank == 1 & strike > f1,
                                (strike_gap/(strike ^ 2)) * exp(r1 * t1) * call_mid_price,
                                ifelse(dte_rank == 1 & strike == f1,
                                       (strike_gap/(strike ^ 2)) * exp(r1 * t1) *
                                         ((call_mid_price + put_mid_price) / 2), 0)))) %>%
  dplyr::mutate(next_strike_contrib =
                  ifelse(dte_rank == 2 & strike < f2,
                         (strike_gap/(strike ^ 2)) * exp(r2 * t2) * put_mid_price,
                         ifelse(dte_rank == 2 & strike > f2,
                                (strike_gap/(strike ^ 2)) * exp(r2 * t2) * call_mid_price,
                                ifelse(dte_rank == 2 & strike == f2,
                                       (strike_gap/(strike ^ 2)) * exp(r2 * t2) *
                                         ((call_mid_price + put_mid_price) / 2), 0)))) %>%
  dplyr::mutate(q1 = (1/t1) * ((f1/k1) - 1) ^ 2,
                q2 = (1/t2) * ((f2/k2) - 1) ^ 2) %>%
  dplyr::group_by(quotedate) %>%
  dplyr::mutate(next_dte = max(dte)) %>%
  dplyr::mutate(near_tot = (2 / t1) * sum(near_strike_contrib),
                next_tot = (2 / t2) * sum(next_strike_contrib)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(sigma1 = near_tot - q1,
                sigma2 = next_tot - q2) %>%
  dplyr::mutate(IV =
                  100 * sqrt(
                    (t1 * sigma1 * (((min_day * next_dte) - min_month) / ((min_day * next_dte) - (min_day * near_dte))) +
                       t2 * sigma2 * ((min_month - (min_day * near_dte)) / ((min_day * next_dte) - (min_day * near_dte)))) *
                      (min_year / min_month))
  )

IV_data <- option_data %>%
  dplyr::distinct(quotedate, close, IV)

# Calculate IVRank
IV_data <- IV_data %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::arrange(quotedate) %>%
  dplyr::mutate(iv_rank_252 =
                  round(100 * TTR::runPercentRank(IV, n = 252, cumulative = FALSE, exact.multiplier = 1), digits = 0))

monthly <- readRDS(paste0(here::here(), "/data/monthly.RDS"))

monthly_high_IV <- IV_data %>%
  dplyr::filter(iv_rank_252 >= 50) %>%
  dplyr::filter(quotedate %in% monthly$date) %>%
  dplyr::mutate(date = quotedate) %>%
  dplyr::select(date)

saveRDS(monthly_high_IV, file = paste0(here::here(), "/data/monthly_high_iv.RDS"))
}

