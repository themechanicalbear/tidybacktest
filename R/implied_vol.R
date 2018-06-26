library(data.table)
library(dplyr)
library(lubridate)
library(RcppBDT)

# Risk free rate found at:
# - https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=billrates

# Pre-Processing data ----
# Variables for later use in script
single_date <- "Yes"
test_date <- "2016-09-21"
symbol <- "SPY"
min_day <- 1440
min_month <- min_day * 30
min_year <- 525600

option_data <- readRDS(paste0("~/Library/Mobile Documents/com~apple~CloudDocs/r_projects/tidybacktest/data/options/",
symbol, ".RDS"))

# Combine Call and Put data with same (date, expiration, and strike)
dt_puts <- option_data %>%
  dplyr::filter(type == "put") %>%
  dplyr::mutate(mid = (ask + bid) / 2) %>%
  dplyr::mutate(put_mid_price = mid,
                put_bid = bid) %>%
  dplyr::select(c(symbol, quotedate, close, expiration, strike, dte, exp_type, put_mid_price, put_bid))

dt_calls <- option_data %>%
  dplyr::filter(type == "call") %>%
  dplyr::mutate(mid = (ask + bid) / 2) %>%
  dplyr::mutate(call_mid_price = mid,
                call_bid = bid) %>%
  dplyr::select(c(symbol, quotedate, close, expiration, strike, dte, exp_type, call_mid_price, call_bid))

option_data <- dt_puts %>%
  dplyr::left_join(dt_calls, by = Reduce(intersect, list(names(dt_puts), names(dt_calls))))

# Step 1 Add Risk-Free-Rate, Near & Next expirations to dataset ----
# Make a data.frame with all the unique open dates to be used as reference table
uni_dates <- as.data.frame(unique(option_data$quotedate)) %>%
  setnames("date")

# Risk free rate using 4 & 13 week coupon rate
# rate_url <- "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=billratesAll"
#
# rates_raw <- read_html(rate_url) %>%
#   html_node('.t-chart') %>%
#   html_table()
#
# rates <- rates_raw %>%
#   data.table::setnames(c("date", "wk_4_bank_discount", "wk_4_coupon", "wk_13_bank_discount", "wk_13_coupon",
#                          "wk_26_bank_discount", "wk_26_coupon", "wk_52_bank_discount", "wk_52_coupon")) %>%
#   dplyr::mutate(date = as.Date(date, "%m/%d/%y")) %>%
#   dplyr::filter(!dplyr::row_number() == 1) %>%
#   dplyr::mutate_at(vars(starts_with("wk")),funs(as.numeric))

# saveRDS(rates, file = "data/rates.RDS")
rates <- readRDS("data/rates.RDS")

# Remove all the bid == 0 rows because they will not be used in the calculation
# Equities are calculated off only monthly expirations
option_data <- option_data %>%
  filter(dte > 23,
         #exp_type == "Monthly",
         ((strike <= close & put_bid > 0) | (strike >= close & call_bid > 0)))

# option_data <- option_data %>%
#   dplyr::mutate(strike_diff = close - strike) %>%
#   dplyr::group_by(quotedate, expiration) %>%
#   dplyr::top_n(-4, abs(strike_diff)) %>%
#   dplyr::ungroup()

# Temp shrink dataset to make testing faster
if (single_date == "Yes") {
  option_data <- filter(option_data, quotedate == test_date)
  uni_dates <- filter(uni_dates, date == test_date)
}

for (i in 1:nrow(uni_dates)) {
  sub_data <- filter(option_data, quotedate == uni_dates[i, "date"])
  rdate <- uni_dates[[i, "date"]]
  if (nrow(sub_data) > 0) {
    dte_near <- min(sub_data$dte)
    sub_data <- filter(sub_data, dte > dte_near + 6)
    dte_next <- min(sub_data$dte)
    r0 <- filter(rates, date == rdate)
    r1 <- r0[[1, 3]] / 100
    r2 <- r0[[1, 5]] / 100
    uni_dates[i, "near_dte"] <- dte_near
    uni_dates[i, "next_dte"] <- dte_next
    uni_dates[i, "r1"] <- r1
    uni_dates[i, "r2"] <- r2
    uni_dates[i, "t1"] <- (dte_near * min_day) / min_year
    uni_dates[i, "t2"] <- (dte_next * min_day) / min_year
  }
}

# Merge the data together adding near and next dte
option_data <- option_data %>%
  dplyr::left_join(uni_dates, by = c("quotedate" = "date"))

option_data <- filter(option_data, dte == near_dte | dte == next_dte)

# Step 2 Find the ATM strike ----
for (i in 1:nrow(uni_dates)) {
  Fdate <- uni_dates[[i, "date"]]

  F_data <- option_data %>%
    dplyr::filter(quotedate == Fdate)

  F_level_near <- F_data %>%
    dplyr::filter(dte == near_dte) %>%
    dplyr::mutate(p_c_abs = abs(call_mid_price - put_mid_price)) %>%
    dplyr::filter(p_c_abs == min(p_c_abs, na.rm = TRUE))

  F_level_next <- F_data %>%
    dplyr::filter(dte == next_dte) %>%
    dplyr::mutate(p_c_abs = abs(call_mid_price - put_mid_price)) %>%
    dplyr::filter(p_c_abs == min(p_c_abs, na.rm = TRUE))

  # Get near-term values
  K1 <- F_level_near[[1, "strike"]]
  R1 <- F_level_near[[1, "r1"]]
  T1 <- F_level_near[[1, "t1"]]
  M1 <- F_level_near[[1, "call_mid_price"]] - F_level_near[[1, "put_mid_price"]]
  F1 <- K1 + exp(R1 * T1) * M1

  # Get next-term values
  K2 <- F_level_next[[1, "strike"]]
  R2 <- F_level_next[[1, "r2"]]
  T2 <- F_level_next[[1, "t2"]]
  M2 <- F_level_next[[1, "call_mid_price"]] - F_level_next[[1, "put_mid_price"]]
  F2 <- K2 + exp(R2 * T2) * M2

  uni_dates[i, "K1"] <- K1
  uni_dates[i, "K2"] <- K2
  uni_dates[i, "F1"] <- F1
  uni_dates[i, "F2"] <- F2
}

K_data <- select(uni_dates, c(date, K1, K2, F1, F2))
option_data <- dplyr::left_join(option_data, K_data, by = c("quotedate" = "date"))

# Step 3 Calculate gap between strikes----
all_pro <- data.frame()

# Function to calculate gap between consecutive strikes
strike_gap <- function(x) {
  for (i in 1:nrow(x)) {
    dplyr::arrange(x, dte, strike)
    ifelse(i != nrow(x),
           x[i, "strike_gap"] <- x[i + 1, "strike"] - x[i, "strike"],
           x[i, "strike_gap"] <- x[i, "strike"] - x[i - 1, "strike"])
  }
  sg <- dplyr::select(x, c("quotedate", "expiration", "strike", "dte", "strike_gap"))
  all_pro <- rbind(all_pro, sg)
  return(all_pro)
}

for (i in 1:nrow(uni_dates)) {
  Fdate <- uni_dates[[i, "date"]]

  F_data <- filter(option_data, quotedate == Fdate)
  F_near_data <- filter(F_data, dte == near_dte)
  F_next_data <- filter(F_data, dte == next_dte)

  if (nrow(F_near_data) > 1) {
    all_pro <- strike_gap(F_near_data)
  }

  if (nrow(F_next_data) > 1) {
    all_pro <- strike_gap(F_next_data)
  }
}

option_data <- option_data %>%
  dplyr::left_join(all_pro, by = c("quotedate", "expiration", "strike", "dte"))

# Step 4 Calculate strike contribution ----
option_data <- option_data %>%
  dplyr::mutate(near_strike_contrib = ifelse(dte == near_dte & strike < F1,
                                      (strike_gap/(strike ^ 2)) * exp(r1 * t1) * put_mid_price,
                                      ifelse(dte == near_dte & strike > F1,
                                             (strike_gap/(strike ^ 2)) * exp(r1 * t1) * call_mid_price,
                                             ifelse(dte == near_dte & strike == F1,
                                                    (strike_gap/(strike ^ 2)) * exp(r1 * t1) * ((call_mid_price + put_mid_price) / 2),
                                                    0)))) %>%
  dplyr::mutate(next_strike_contrib = ifelse(dte == next_dte & strike < F2,
                                      (strike_gap/(strike ^ 2)) * exp(r2 * t2) * put_mid_price,
                                      ifelse(dte == next_dte & strike > F2,
                                             (strike_gap/(strike ^ 2)) * exp(r2 * t2) * call_mid_price,
                                             ifelse(dte == next_dte & strike == F2,
                                                    (strike_gap/(strike ^ 2)) * exp(r2 * t2) * ((call_mid_price + put_mid_price) / 2),
                                                    0))))

# Step 5 Calculate contribution totals & Sigma----
df <- data.frame()

option_data <- option_data %>%
  dplyr::mutate(Q1 = (1/t1) * ((F1/K1) - 1) ^ 2,
                Q2 = (1/t2) * ((F2/K2) - 1) ^ 2)

for (i in 1:nrow(uni_dates)) {
  Fdate <- uni_dates[[i, "date"]]

  F_data <- dplyr::filter(option_data, quotedate == Fdate)
  T1 <- F_data[1, "t1"]
  T2 <- F_data[1, "t2"]
  F_near_data <- dplyr::filter(F_data, dte == near_dte)
  near_contrib_tot <- (2 / T1) * sum(F_near_data$near_strike_contrib)
  F_next_data <- dplyr::filter(F_data, dte == next_dte)
  next_contrib_tot <- (2 / T2) * sum(F_next_data$next_strike_contrib)
  df[i, "near_tot"] <- near_contrib_tot
  df[i, "next_tot"] <- next_contrib_tot
  df[i, "date"] <- uni_dates[[i, "date"]]
}

df$date <- as.Date(df$date, origin = "1970-01-01")

option_data <- option_data %>%
  dplyr::left_join(df, by = c("quotedate" = "date"))

option_data <- dplyr::mutate(option_data, sigma1 = near_tot - Q1,
                             sigma2 = next_tot - Q2)

# Step 6 Calculate IV ----

option_data <- option_data %>%
  mutate(IV =
           100 * sqrt(
             (t1 * sigma1 * (((min_day * next_dte) - min_month) / ((min_day * next_dte) - (min_day * near_dte))) +
                t2 * sigma2 * ((min_month - (min_day * near_dte)) / ((min_day * next_dte) - (min_day * near_dte)))) *
               (min_year / min_month))
  )

# Step 7 Filter to single row for each day ----
(IV_data <- option_data %>%
  dplyr::distinct(quotedate, close, IV))

