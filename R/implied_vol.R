implied_vol <- function(symbol) {
  library(dplyr)
  #symbol <- "AMD"
  min_day <- 1440
  min_month <- min_day * 30
  min_year <- 525600

  option_data <- readRDS(paste0(here::here(), "/data/options/", symbol, ".RDS"))
  rates <- risk_free_rate()

  option_data <- option_data %>%
    dplyr::mutate(mid = (ask + bid) / 2)

  puts <- option_data %>%
    dplyr::filter(type == "put") %>%
    dplyr::mutate(put_mid_price = mid,
                  put_bid = bid) %>%
    dplyr::select(c(symbol, quotedate, close, expiration, strike, dte, exp_type, put_mid_price, put_bid))

  calls <- option_data %>%
    dplyr::filter(type == "call") %>%
    dplyr::mutate(call_mid_price = mid,
                  call_bid = bid) %>%
    dplyr::select(c(symbol, quotedate, close, expiration, strike, dte, exp_type, call_mid_price, call_bid))

  option_data <- puts %>%
    dplyr::left_join(calls, by = Reduce(intersect, list(names(puts), names(calls))))

  option_data <- option_data %>%
    dplyr::filter(dte > 23,
                  ((strike <= close & put_bid > 0) | (strike >= close & call_bid > 0))) %>%
    dplyr::group_by(quotedate) %>%
    dplyr::mutate(dte_rank = dense_rank(dte)) %>%
    dplyr::filter(dte_rank == 1 | dte_rank == 2) %>%
    dplyr::mutate(dte_near = min(dte),
                  dte_next = max(dte)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(rates, by = c("quotedate" = "date"))

  summary_output <- data.frame()

  vol_calc <- function(dt) {
    sub_data <- filter(option_data, quotedate == dt)
    if (nrow(sub_data) > 0) {
      r0 <- dplyr::filter(rates, date == dt)

      F_level_near <- sub_data %>%
        dplyr::filter(dte == dte_near) %>%
        dplyr::mutate(p_c_abs = abs(call_mid_price - put_mid_price)) %>%
        dplyr::filter(p_c_abs == min(p_c_abs, na.rm = TRUE))

      F_level_next <- sub_data %>%
        dplyr::filter(dte == dte_next) %>%
        dplyr::mutate(p_c_abs = abs(call_mid_price - put_mid_price)) %>%
        dplyr::filter(p_c_abs == min(p_c_abs, na.rm = TRUE))

      if (nrow(F_level_near) > 0 & nrow(F_level_next) > 0 & nrow(r0) > 0) {
        sub_data <- sub_data %>%
          dplyr::filter(dplyr::row_number() == 1) %>%
          dplyr::mutate(r1 = r0[[1, 3]] / 100,
                        r2 = r0[[1, 5]] / 100,
                        t1 = (dte_near * min_day) / min_year,
                        t2 = (dte_next * min_day) / min_year,
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

  # option_data <- option_data %>%
  #   dplyr::mutate(strike_diff = close - strike) %>%
  #   dplyr::group_by(quotedate, expiration) %>%
  #   dplyr::top_n(-4, abs(strike_diff)) %>%
  #   dplyr::ungroup()

  option_data <- dplyr::left_join(option_data, summary_output, by = c("symbol", "quotedate"))

  option_data <- option_data %>%
    dplyr::filter(complete.cases(.))

  option_data <- option_data %>%
    dplyr::group_by(quotedate, dte) %>%
    dplyr::arrange(strike) %>%
    dplyr::mutate(strike_gap = dplyr::lead(strike, 1) - strike) %>%
    dplyr::mutate(strike_gap = ifelse(is.na(strike_gap), strike - dplyr::lag(strike, 1), strike_gap)) %>%
    dplyr::ungroup()

  option_data <- option_data %>%
    dplyr::mutate(near_strike_contrib =
                    ifelse(dte == dte_near & strike < f1,
                           (strike_gap/(strike ^ 2)) * exp(r1 * t1) * put_mid_price,
                           ifelse(dte == dte_near & strike > f1,
                                  (strike_gap/(strike ^ 2)) * exp(r1 * t1) * call_mid_price,
                                  ifelse(dte == dte_near & strike == f1,
                                         (strike_gap/(strike ^ 2)) * exp(r1 * t1) * ((call_mid_price + put_mid_price) / 2),
                                         0)))) %>%
    dplyr::mutate(next_strike_contrib =
                    ifelse(dte == dte_next & strike < f2,
                           (strike_gap/(strike ^ 2)) * exp(r2 * t2) * put_mid_price,
                           ifelse(dte == dte_next & strike > f2,
                                  (strike_gap/(strike ^ 2)) * exp(r2 * t2) * call_mid_price,
                                  ifelse(dte == dte_next & strike == f2,
                                         (strike_gap/(strike ^ 2)) * exp(r2 * t2) * ((call_mid_price + put_mid_price) / 2),
                                         0))))

  option_data <- option_data %>%
    dplyr::mutate(q1 = (1/t1) * ((f1/k1) - 1) ^ 2,
                  q2 = (1/t2) * ((f2/k2) - 1) ^ 2) %>%
    dplyr::group_by(quotedate) %>%
    dplyr::mutate(near_tot = (2 / t1) * sum(near_strike_contrib),
                  next_tot = (2 / t2) * sum(next_strike_contrib)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(sigma1 = near_tot - q1,
                  sigma2 = next_tot - q2) %>%
    dplyr::mutate(IV =
                    100 * sqrt(
                      (t1 * sigma1 * (((min_day * dte_next) - min_month) / ((min_day * dte_next) - (min_day * dte_near))) +
                         t2 * sigma2 * ((min_month - (min_day * dte_near)) / ((min_day * dte_next) - (min_day * dte_near)))) *
                        (min_year / min_month))
    )

  IV_data <- option_data %>%
    dplyr::distinct(quotedate, close, IV)
}

