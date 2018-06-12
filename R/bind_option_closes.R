#' @export
bind_option_closes <- function(stock) {
  df_pattern <- paste0("option_close_", stock, "_[0-9]{5}")
  df_list <- ls(pattern = df_pattern, envir = close_trades)

  option_analysis_closes <- dplyr::bind_rows(lapply(df_list,
                                                    function(x) get(x, envir = close_trades)))
  assign(paste0("option_closes_", stock), option_analysis_closes, envir = trade_results)
}
