
assign_global <- function(results, results_table) {
  assign("num_t", nrow(results), envir = .GlobalEnv)
  assign("tot_profit", sum(results$profit), envir = .GlobalEnv)
  assign("avg_profit", tot_profit/num_t, envir = .GlobalEnv)
  assign("tot_days", sum(results$days_held), envir = .GlobalEnv)
  assign("avg_days", tot_days/num_t, envir = .GlobalEnv)
  assign("avg_prof_day", avg_profit/avg_days, envir = .GlobalEnv)
  assign("maximum_loss", ifelse(min(results$profit) >= 0, 0, min(results$profit)),
         envir = .GlobalEnv)
  assign("max_win", ifelse(max(results$profit) < 0, 0, max(results$profit)),
         envir = .GlobalEnv)
  assign("avg.entry.margin", sum(results$option_margin)/num_t, envir = .GlobalEnv)
  assign("percent_winners", percent(length(which(results$profit > 0)) / num_t),
         envir = .GlobalEnv)
  assign("exit.expiration",
         ifelse(percent(length(which(results$exit_reason == "Expiration"))
                        / num_t) == "NaN%",
                0, percent(length(which(results$exit_reason == "Expiration"))
                           / num_t)), envir = .GlobalEnv)
  assign("exit.profit.target",
         ifelse(percent(length(which(results$exit_reason == "Profit target"))
                        / num_t) == "NaN%",
                0, percent(length(which(results$exit_reason == "Profit target"))
                           / num_t)), envir = .GlobalEnv)
  assign("exit.loss.limit",
         ifelse(percent(length(which(results$exit_reason == "Loss limit"))
                        / num_t) == "NaN%",
                0, percent(length(which(results$exit_reason == "Loss limit"))
                           / num_t)), envir = .GlobalEnv)
  assign("exit.gamma.risk",
         ifelse(percent(length(which(results$exit_reason == "Gamma risk"))
                        / num_t) == "NaN%",
                0, percent(length(which(results$exit_reason == "Gamma risk"))
                           / num_t)), envir = .GlobalEnv)
  assign("exit.earnings",
         ifelse(percent(length(which(results$exit_reason == "Earnings"))
                        / num_t) == "NaN%",
                0, percent(length(which(results$exit_reason == "Earnings"))
                           / num_t)), envir = .GlobalEnv)
  assign("results", results, envir = .GlobalEnv)
  assign("results_table", results_table, envir = .GlobalEnv)
}
