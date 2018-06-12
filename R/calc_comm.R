#' Calculate commisions on trade execution
#'
#' @description{
#' calc_comm calculates commission fees for trades based on TD Ameritrades fee
#' structure
#' }
#'
#' @export
#'
#' @param strat string of strategy run
#'
#' @param close numeric of the trade close price
#'
#' @param profit numeric of the trade profit prior to commissions
#'
#' @examples
#'
#'
#'
#'# Function arguments:-----------------------------------------------------------
#   strat = option strategy
#   close = closing option mid price
#   profit = trade profits prior to commissions
#   (sp, lp, sc, lc) + strk = short/long put/call strikes
#   (sp, lp, sc, lc) + cred = short/long put/call mid price
#   qty = lot size
#   shares = option share multiplier

# Function----------------------------------------------------------------------
# Add in commission at a rate of $1.5 per contract excluding closing <= .05
calc_comm <- function(strat, close, profit) {
  if (strat == "short_put") {
    ifelse(close <= .05, profit - 1.5, profit - 3)
  }
  if (strat == "short_strangle" | strat == "short_straddle") {
    ifelse(close <= .05, profit - 3, profit - 6)
  }
  if (strat == "long_stock") {
    profit - 9.95 * 2
  }
}
