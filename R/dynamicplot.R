#' @export
dynamicplot <- function() {
  s1 <- input$DT_table_rows_current  # rows on the current page
  s2 <- input$DT_table_rows_all      # rows on all pages (after being filtered)
  s3 <- input$DT_table_rows_selected # rows selected in the table

  par(mar = c(4, 4, 1, .1))
  plot(x = results_table2$trade_open, y = results_table2$profit, pch = 21)

  # solid dots (pch = 19) for current page
  if (length(s1)) {
    points(results_table2[s1, , drop = FALSE], pch = 19, cex = 2)
  }

  # show red circles when performing searching
  if (length(s2) > 0 && length(s2) < nrow(results_table2)) {
    points(results_table2[s2, , drop = FALSE], pch = 21, cex = 3, col = 'red')
  }

  # show blue circles when performing selection
  if (length(s3) > 0 && length(s3) < nrow(results_table2)) {
    points(results_table2[s3, , drop = FALSE], pch = 21, cex = 3, col = 'blue')
  }

  # dynamically change the legend text
  s <- input$DT_table_search
  txt <- if (is.null(s) || s == '') 'Filtered data' else {
    sprintf('Data matching "%s"', s)
  }

  # legend(
  #   'topright', c('Original data', 'Data on current page', txt),
  #   pch = c(21, 19, 21), pt.cex = c(1, 2, 3), col = c(1, 1, 2),
  #   y.intersp = 2, bty = 'n'
  # )
}
