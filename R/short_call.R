#' @export
short_call <- function(progress.int, t) {
  shiny::withProgress(message = "Progress Bar", detail = "Opening Trades", {
    if (!exists(paste0(stock, "_options"), envir = as.environment(options_data))) {load_options_data(stock)}
    invisible(purrr::map(stock, open_short_call))
    invisible(purrr::pmap(list(stock, "close_short_call"), close_trades_macro))
    invisible(purrr::map(stock, bind_option_closes))
    format_results(results)
  })
}

