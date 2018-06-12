#' @export
format_results <- function(results) {
  results <- trade_results[[paste0("option_closes_", stock)]] %>%
    dplyr::select(-c(quotedate, expiration, dte)) %>%
    dplyr::mutate(trade_open = as.Date(trade_open, origin = "1970-01-01"),
                  trade_close = as.Date(trade_close, origin = "1970-01-01"),
                  profit = 100 * (trade_open_price - trade_close_price),
                  id = as.integer(rownames(.)),
                  color = ifelse(profit >= 0, "#00a65a", "red"),
                  stock_move = (100 * (close - lag(close, 1))),
                  stock_move = ifelse(is.na(stock_move), 0, stock_move),
                  stock_cumsum = cumsum(stock_move),
                  days_held = as.numeric(trade_close) - as.numeric(trade_open)) %>%
    dplyr::arrange(trade_open)

  results_table <- dplyr::select(results, c(symbol, trade_open, trade_close, strike, close, profit, days_held))

  # results_table <- DT::formatCurrency(results_table, "profit", currency = "$", interval = 3, mark = ",",
  #                                 digits = 2, dec.mark = getOption("OutDec"), before = TRUE)
  assign_global(results, results_table)
}
