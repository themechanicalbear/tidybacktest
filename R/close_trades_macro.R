#' @export
close_trades_macro <- function(stock, func) {
  df <- get(paste0("open_options_", stock), envir = as.environment(open_trades))

  purrr::pmap_df(list(stock = df$symbol,
                      open_date = df$quotedate,
                      strk = df$strike,
                      open_price = df$mid,
                      exp = df$expiration,
                      close_date = df$expiration,
                      spd = df$spread),
                 match.fun(func))
}
