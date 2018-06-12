#' Format numeric to currency for output
#' @description{
#' print_money formats numerics to currency for out put in shiny app
#' }
#' @param x Numeric
#'
#' @export
#'
#' @return numeric with 2 decimal places and big.mark comma
#'
#' @examples
#' print_money(14857)
#' returns 14,857.00
#'
print_money <- function(x){
  format(x, digits = 10, nsmall = 2, decimal.mark = ".", big.mark = ",")
}
