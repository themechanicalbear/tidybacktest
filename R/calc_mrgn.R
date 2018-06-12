#' Calculate initial margin on opening trades
#'
#' @description{
#' calc_mrgn calculates the opening margin required depending on strategy type
#' }
#'
#' @export
#'
#'
#'
#' @examples
#'
#'
# Function arguments:-----------------------------------------------------------
#   strat = option strategy
#   prc = stock price
#   (sp, lp, sc, lc) + strk = short/long put/call strikes
#   (sp, lp, sc, lc) + cred = short/long put/call mid price
#   qty = lot size
#   shares = option share multiplier


# Function----------------------------------------------------------------------
# Opening margin
calc_mrgn <- function(strat = NULL, prc = NULL, spstrk = NULL, lpstrk = NULL,
                      scstrk = NULL, lcstrk = NULL, spcred = NULL, lpcred = NULL,
                      sccred = NULL, lccred = NULL, qty = 1, shares = 100) {
  if (strat == "short_put") {
    max(
      (.2 * prc * qty * shares) - ((prc - spstrk) * qty * shares) + (spcred * qty * shares),
      (.1 * spstrk * qty * shares ) + (qty * shares * spcred),
      (50 * qty) + (spcred * qty * shares)
    )
  }
}
