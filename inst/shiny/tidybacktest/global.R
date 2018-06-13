# Todos ----
# TODO Should axis variable be in global?
# TODO profile app - profvis::profvis(shiny::runApp())

# Global setup ----
suppressWarnings(suppressMessages(suppressPackageStartupMessages({
  library_list <- c("shiny", "shinythemes", "shinydashboard", "shinyjs", "ggvis", "rbokeh",
                    "ggplot2", "scales", "shinyBS", "shinycssloaders", "rintrojs", "here", "DT")
  invisible(lapply(library_list, require, character.only = TRUE))
})))

# Variables that can be put on the axis
axis_vars <- c(
  "Open Date" = "trade_open",
  "Days Held" = "days_held",
  "IV Rank" = "open_ivrank",
  "Open ROC" = "open_roc",
  "Profit" = "profit",
  "RSI" = "open_rsi",
  "Year" = "year")

data_root <- here::here("data/git_options/")
symbol_list <- gsub(".RDS", "", list.files(here("data/git_options/")))

load_options_data <- function(stock) {
  data <- paste0(data_root, stock, ".RDS")
  assign(paste0(stock, "_options"),
         readRDS(data),
         envir = options_data)
}

open_trades <- new.env(hash = TRUE, size = NA)
options_data <- new.env(hash = TRUE, size = NA)
close_trades <- new.env(hash = TRUE, size = NA)
trade_results <- new.env(hash = TRUE, size = NA)
