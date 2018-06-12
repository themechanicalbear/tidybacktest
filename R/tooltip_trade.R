#' Generating tooltip (hover over) text for plots
#' @description{
#' tooltip_trade takes a results data.frame from a study and selects the row that
#' you are hoveringformats numerics to currency for out put in shiny app
#' }
#' @param x data.frame of trade results
#'
#' @return string of HTML formated values formated from the specific results row
#' to be included in the hover over of a plot
#'
#' @examples
#' tooltip_trade(results)
#'
#' @export
tooltip_trade <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.null(x$id)) return(NULL)

  # Pick out the trade with this trade_num
  trade <- results[results$id == x$id, ]

  paste0("Open: ", trade$trade_open, "<br>",
         "Close: ", trade$trade_close, "<br>",
         "Strike: ", trade$strike, "<br>",
         "Profit: $", print_money(trade$profit))
}
