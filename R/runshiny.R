#' @export
runshiny <- function() {
  appDir <- system.file("shiny", "tidybacktest", package = "tidybacktest")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `tidybacktest`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
