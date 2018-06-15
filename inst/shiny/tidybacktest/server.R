# Todos ----
# TODO change envir .Global to current shiny envir
# TODO (https://shiny.rstudio.com/articles/scoping.html)
# TODO ensure earnings date is correct due to AM or PM reporting
# TODO (http://www.shinyapps.io/#_pricing)
# TODO zero line on the ggvis trade_profit chart causes issues when axis choice is modified in UI
# TODO Add data entries for skipped trades due to study inputs or data issues to display in reports
# TODO study short_put_spread
# TODO study short_call_spread
# TODO study long_call
# TODO study long_call_spread
# TODO study long_put
# TODO study long_put_spread
# TODO study short_put_ladder
# TODO study short_call_ladder
# TODO extend long and short logic to all studies so allow for consistent profit calculations
# TODO decide on standard implementation of signs for buy and sell cost (neg for buy?)
# TODO exclude color column from the results table output
# TODO sort the results table with newest trade at the top
# TODO implement solutions here (https://github.com/daattali/advanced-shiny)
# TODO check this list out (http://enhancedatascience.com/2017/07/10/the-packages-you-need-for-your-r-shiny-application/)
# TODO (http://www.ggplot2-exts.org/gallery/)
# TODO add testthat scripts
# TODO move all functions to their own .R file for testthat
# TODO make all function names start with same prefix when grouped like functions ie(open_put, open_call, close_put, close_call)
# TODO move data to the package directory and remove from .git
# TODO enquo and quo for the variable selection in plotting axis
# TODO aes string in ggplot to tidy evalutation

# Shiny Server ----
shiny::shinyServer(function(input, output, session) {
  session$onSessionEnded(stopApp) # Stops the shiny server when the browser window is closed
  shiny::shinyOptions(progress.style = "old")
  main_color <- "#00a65a" # Green

  # Uncomment next line for debugging to console
  # options(shiny.trace = TRUE)
  # The following two lines can be inserted to box in code section for profiling
  # Rprof("boot.out")
  # Rprof(NULL)
  # options(shiny.error = recover)

  # initiate hints on startup with custom button and event
  rintrojs::hintjs(session, options = list("hintButtonLabel" = "Hope this hint was helpful"),
                   events = list("onhintclose" = I('alert("Wasn\'t that hint helpful")')))

  shiny::observeEvent(input$help, {
    environment(show_help) <- environment()
    show_help()
  })

  output$userpanel <- shiny::renderUI({
    # session$user is non-NULL only in authenticated sessions
    #if (!is.null(session$user)) {
    shinydashboard::sidebarUserPanel(
      span("Logged in as ", session$user),
      subtitle = a(icon("sign-out"), "Logout", href = "__logout__"))
    #}
  })

  # Help ----
  output$notificationMenu <- shinydashboard::renderMenu({
    shinydashboard::notificationItem(text = "Need help?", icon = shiny::icon("user"),
                                     status = "info", href = "mailto:jason@themechanicalbear.com")
  })

  data.frame(x_rng = c(as.Date("2012-01-03"), as.Date("2018-01-03")),
             y_rng = c(-100, 100)) %>%
    ggvis(~x_rng, ~y_rng) %>%
    set_options(width = "auto",
                height = 600) %>%
    layer_points(fillOpacity := 0) %>%
    add_axis("x", title = "Trade Open Date") %>%
    add_axis("y", title = "Profit") %>%
    bind_shiny("ggvis_trades")

  # Welcome message ----
  output$welcome_message <- shiny::renderUI({
    HTML("<p style=\"text-align: center; font-size: 60px; color: #FFFFFF;\">.</p>
         <p style=\"text-align: center; font-size: 20px;\">
         <strong>Welcome:</strong> Please fill out the study inputs in the left sidebar</p>
         <p style=\"text-align: center; font-size: 20px;\">When ready click&nbsp;
         <strong>Run Study</strong>&nbsp;to see the results</p>")
  })

  observe({
    if (input$host == "local") {
      data_root <- here::here("data/options/")
      symbol_list <- gsub(".RDS", "", list.files(here("data/options/")))
      assign("symbol_list", symbol_list, envir = .GlobalEnv)
      assign("data_root", data_root, envir = .GlobalEnv)
      updateSelectizeInput(session, 'stock', choices = symbol_list)
    }
    if (input$host == "git") {
      data_root <- here::here("data/git_options/")
      symbol_list <- gsub(".RDS", "", list.files(here("data/git_options/")))
      assign("symbol_list", symbol_list, envir = .GlobalEnv)
      assign("data_root", data_root, envir = .GlobalEnv)
      updateSelectizeInput(session, 'stock', choices = symbol_list)
    }
  })

  # Loading image ----
  shiny::observeEvent(input$goPlot, {
    # Reset the results data.frame when inputs are changed
    if (exists("results", envir = .GlobalEnv) && is.data.frame(get("results"))) {rm(results, envir = .GlobalEnv)}

    # Determine customer's choice for frequency and set
    assign("openOption", input$openOption, envir = .GlobalEnv)
    if (openOption == "First of Month") {
      monthly <- readRDS(here("data/monthly.RDS"))
      assign("first_day", monthly, envir = .GlobalEnv)
      assign("inc.amount", .004)
    }
    else if (openOption == "First of Week") {
      data(list = "mondays")
      assign("first_day", mondays, envir = .GlobalEnv)
      assign("inc.amount", .001)
    }
    else if (openOption == "Daily") {
      data(list = "open_daily")
      assign("first_day", open_daily, envir = .GlobalEnv)
      assign("inc.amount", .0002)
    }
    shiny::withProgress(message = "Progress", detail = "Setting up study", value = .05, {
      t <- 0 # Set inital trade number to zero
      progress.int <- inc.amount # Set progress bar increment amount

      # Values defined by the customer in shiny ui
      assign("study", input$study, envir = .GlobalEnv)
      assign("stock", input$stock, envir = .GlobalEnv)
      assign("low_iv", input$open_ivrank[1], envir = .GlobalEnv)
      assign("high_iv", input$open_ivrank[2], envir = .GlobalEnv)
      assign("o_dte", input$open_dte, envir = .GlobalEnv)
      assign("s_dte", input$second_dte, envir = .GlobalEnv)
      assign("c_delta", input$call_delta, envir = .GlobalEnv)
      assign("p_delta", input$put_delta, envir = .GlobalEnv)
      assign("prof_targ", input$proftarg / 100, envir = .GlobalEnv)
      assign("loss_lim", input$loss_lim + 1, envir = .GlobalEnv)
      assign("l_loss_lim", input$l_loss_lim / 100, envir = .GlobalEnv)
      assign("g", input$gamma_days, envir = .GlobalEnv)
      assign("earn_close", input$earn_close, envir = .GlobalEnv)
      assign("min_roc", input$min_roc, envir = .GlobalEnv)
      assign("p_delta_lim", p_delta + .1, envir = .GlobalEnv)
      assign("c_delta_lim", c_delta - .1, envir = .GlobalEnv)
    })

    # Run function for study selected ----
    if (study == "Call Calendar") {call_calendar(progress.int, t)}
    if (study == "Poor Mans Cov Call") {pmcc(progress.int, t)}
    if (study == "Short Call") {short_call(progress.int, t)}
    if (study == "Short Put") {short_put(progress.int, t)}
    if (study == "Short Put Spread") {short_put_spread(progress.int, t)}
    if (study == "Long Stock") {LongStock(progress.int, t)}
    if (study == "Strangle") {strangle(progress.int, data_set, t)}
    if (study == "Straddle") {straddle(progress.int, t)}
    if (study == "Strangle Daily Close") {strangle_daily_close(progress.int, t)}

    # HTML output ----
    output$welcome_message <- shiny::renderUI({
      HTML("")
    })
    output$n_trades <- shiny::renderUI({
      str_num_trades <- paste0("Number of trades in ", input$stock, ": ", nrow(results))
      HTML(str_num_trades)
    })
    environment(output_HTML) <- environment()
    output_HTML()

    results_table2 <- results_table[, c('trade_open', 'profit')]
    #results_table2 <- results_table

    output$DT_table <- DT::renderDT(results_table,
                                    options = list(pageLength = 15,
                                                   lengthMenu = c(15, 25, nrow(results_table2)),
                                                   scrollX = TRUE),
                                    server = FALSE)

    # Scatterplot with contextual highlighting
    output$x2 = renderPlot({
      environment(dynamicplot) <- environment()
      dynamicplot()
    })

    # Download table
    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        paste0(stock, '_', study, '_results.csv')},
      content = function(file) {write.csv(results_table, file)}
    )

    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]

    # data_line <- data.frame(
    #   x_rng = c(min(results$trade_open), max(results$trade_open)),
    #   y_rng = c(0, 0)
    # )

    results %>%
      ggvis(~get(input$xvar), ~get(input$yvar), key := ~id) %>%
      add_axis(type = "x", title = xvar_name) %>%
      add_axis(type = "y", title = yvar_name) %>%
      set_options(width = "auto", height = 600) %>%
      layer_points(fill := ~color, fillOpacity := 0.5) %>%
      add_tooltip(tooltip_trade, "hover") %>%
      #layer_paths(x = ~x_rng, y = ~y_rng, stroke := main.color, data = data_line) %>%
      bind_shiny("ggvis_trades")

    plot_data <- results
    plot_data <- dplyr::mutate(plot_data, cum_sum = cumsum(profit))

    plot_data %>%
      ggvis(~trade_open, ~cum_sum, stroke := "black") %>%
      set_options(width = "auto", height = 600) %>%
      layer_lines() %>%
      #layer_paths(x = ~x_rng, y = ~y_rng, stroke := "green", data = data_line) %>%
      layer_lines(~trade_open, ~stock_cumsum, stroke := "red") %>%
      # add_legend("stroke", title = "test legend") %>%
      # scale_nominal("stroke", range = c("black", "red")) %>%
      #add_tooltip(trade_tooltip, "hover") %>%
      bind_shiny("ggvis_profits")

    shiny::observe({
      output$ggplot_profits <- shiny::renderPlot(
        ggplot(results, aes_string(input$xvar, input$yvar)) +
          geom_point(aes(colour = profit > 0, alpha = 0.5, size = 3)) +
          scale_colour_manual(name = 'profit > 0', values = setNames(c('red','#00a65a'), c(TRUE, FALSE))) +
          xlab(input$xvar) +
          ylab(input$yvar) +
          theme_bw(),
        height = 600, width = "auto")
    })
  })

  # Reset default values when inputs change ----
  # shiny::observe({
  #   if (input$stock == "EEM" || input$stock == "EWZ" || input$stock == "FXI" ||
  #       input$stock == "GDX" || input$stock == "SLV" || input$stock == "SPY" ||
  #       input$stock == "XLE")  {
  #     shiny::updateSelectInput(session, "earn_close", selected = "No")
  #   }
  # })
})
