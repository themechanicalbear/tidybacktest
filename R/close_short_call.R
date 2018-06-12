#' @export
close_short_call <- function(stock, open_date, strk, open_price, exp, close_date, spd) {
  close_details <- get(paste0(stock, "_options"), envir = as.environment(options_data)) %>%
    dplyr::filter(quotedate == close_date,
                  type == "call",
                  strike == strk,
                  expiration == exp) %>%
    dplyr::mutate(trade_open = open_date,
                  trade_close = close_date,
                  trade_open_price = open_price,
                  trade_close_price = ifelse((bid + ask) / 2 == 0.005, 0, (bid + ask) / 2),
                  spread = spd,
                  spread_perc = spread / trade_open_price) %>%
    dplyr::select(symbol, quotedate, expiration, strike, close, dte, trade_open,
                  trade_close, trade_open_price, trade_close_price, spread, spread_perc)
  assign(paste0("option_close_", stock, "_", open_date), close_details,
         envir = close_trades)
}
