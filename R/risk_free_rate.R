risk_free_rate <- function() {
  # Risk free rate found at:
  # https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=billrates

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
  readRDS("data/rates.RDS")
  #assign("rates", rates, envir = .GlobalEnv)
}
