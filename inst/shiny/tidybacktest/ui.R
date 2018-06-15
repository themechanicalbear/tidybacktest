# Todos ----
# TODO Add menu items to show why trades were not opened
# TODO Add buy and hold to the profit plot (Short Put, Strangle complete)
# TODO Change buy and hold plot to return on margin instead of profit
# TODO Test all stocks, studies, and dates
# TODO Move to root folder
# TODO If expiration is reached, make full profit or specific loss instead of just
#      end of day price
# TODO Add confidence intervals to the plots
# TODO Function names to verbs
# TODO Argument names in functions (df, x, y, z, p, n)
# TODO Detail arguments should always be given a default value
# TODO List the best results already run on the site for reference or competition for customers
# TODO Cache each run so that it can be retrieved faster
#      (https://shiny.rstudio.com/articles/persistent-data-storage.html)
# TODO Data sets (https://www.wallstreethorizon.com/corporate-event-calendars)
# TODO historicaloptiondata.com usage terms review

# Shiny UI ----
# JavaScript ----
actionLink <- function(inputId, ...) {
  tags$a(href = 'javascript:void',
         id = inputId,
         class = 'action-button',
         ...)
}
useShinyjs()  # Setup shinyjs

# Dashboard ----
shinydashboard::dashboardPage(
  skin = "green",
  title = "The Mechanical Bear",
  shinydashboard::dashboardHeader(
    title = a(href = 'http://www.themechanicalbear.com',
              img(src = 'logo2.png', title = "The Mechanical Bear", height = "100px"),
              style = "padding-top:10px; padding-bottom:10px;"),
    tags$li(class = "dropdown",
            actionButton("help", "Press for instructions")),
    tags$li(a(href = 'http://www.themechanicalbear.com',
              icon("cogs"),
              title = "Back to Apps Home"),
            class = "dropdown"),
    # Set height of dashboardHeader
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 100px}"),
            tags$style(".main-header .logo {height: 100px;}"),
            tags$style(".sidebar-toggle {height: 100px; padding-top: 1px !important;}"),
            tags$style(".navbar {min-height:100px !important}")),
    shinydashboard::dropdownMenuOutput("messageMenu"),
    shinydashboard::dropdownMenuOutput("notificationMenu"),
    shinydashboard::dropdownMenuOutput("taskMenu")
  ),
  # Sidebar ----
  sidebar <- shinydashboard::dashboardSidebar(
    # Shift the sidebar down to ensure the logo fits above it
    # Hide the default logout panel w/ dynamically-generated user panel
    tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
    tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    uiOutput("userpanel"),

    # introBox(
    #   shiny::actionButton("help", "Press for instructions"),
    #   data.step = 1,
    #   data.intro = "This is a button",
    #   data.hint = "You can press me"
    # ),

    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Study", tabName = "Study", icon = icon("cogs"), startExpanded = TRUE,
                               shiny::selectInput("host", "Host", c("git", "local")),
                               shiny::selectInput("stock", "Stock", symbol_list),
                               shiny::selectInput("study", "Study", c("Short Call", "Short Put", "Short Put Spread")),
                               shiny::selectInput("openOption", "Open on", c("First of Month", "First of Week", "Daily"))),
      shinydashboard::menuItem("Entry Criteria", tabName = "Entry Criteria", icon = icon("cogs"),
                               shiny::sliderInput("open_dte", "DTE", 0, 90, 30, step = 5),
                               shiny::conditionalPanel(
                                 condition = ("input_study == 'Short Call'"),
                                 shiny::sliderInput("call_delta", "Call delta", 0, 1, .16, step = .01)),
                               shiny::conditionalPanel(
                                 condition = ("input_study == 'Short Put' || input_study == 'Strangle'"),
                                 shiny::sliderInput("put_delta", "Put delta", -1, 0, -.16, step = .01)),
                               shiny::conditionalPanel(
                                 condition = ("input_study == 'Call Calendar' || input_study == 'Poor Mans Cov Call'"),
                                 shiny::sliderInput("second_dte", "Min short DTE", 0, 90, 30, step = 5)),
                               shiny::sliderInput("open_ivrank", "IV Rank", 0, 100, c(0, 100), step = 1),
                               shiny::sliderInput("min_roc", "Min ROC", 0, 50, 0, step = 1)),
      shinydashboard::menuItem("Exit Criteria", tabName = "Exit Criteria", icon = icon("cogs"),
                               shiny::sliderInput("proftarg", "Profit target %", 0, 100, 50, step = 5),
                               shiny::sliderInput("loss_lim", "Max loss x times credit received", 0, 10, 2, step = .25),
                               shiny::conditionalPanel(
                                 condition = ("input_study == 'Poor Mans Cov Call'"),
                                 shiny::sliderInput("l_loss_lim", "Long max loss % debit paid", 10, 100, 50, step = 5)),
                               shiny::sliderInput("gamma_days", "Days prior to expiration", 0, 30, 0, step = 1)),
      shiny::actionButton('goPlot', 'Run Study', icon = icon("play-circle"))
    )
  ),
  # Body ----
  body <- shinydashboard::dashboardBody(
    introjsUI(), # Setup help walkthrough
    shiny::fluidRow(
      shiny::column(width = 4,
                    tags$head(HTML("<script type='text/javascript' src='google-analytics.js'></script>")),
                    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                    shiny::htmlOutput("total_profit"),
                    shiny::htmlOutput("avg_prof_trade"),
                    shiny::htmlOutput("avg_prof_day"),
                    shiny::htmlOutput("avg_days"),
                    h4("")),
      shiny::column(width = 4,
                    shiny::htmlOutput("n_trades"),
                    shiny::htmlOutput("percent_winners"),
                    shiny::htmlOutput("max_loss"),
                    shiny::htmlOutput("max_win")),
      shiny::column(width = 4,
                    shiny::htmlOutput("exit_profit_target"),
                    shiny::htmlOutput("exit_loss_limit"),
                    shiny::htmlOutput("exit_expiration"),
                    shiny::htmlOutput("exit_gamma_risk"))),
    shiny::fluidRow(
      tabBox(
        id = "tabset1", height = "800px", width = "1000px",
        shiny::tabPanel("Trade Results",
                        shiny::htmlOutput("welcome_message"),
                        introBox(plotOutput("ggplot_profits", height = 650), data.step = 5, data.intro = "Here we will plot the
                                 running proift of the chosen study against the buy and hold stock position."),
                        shiny::fluidRow(
                          shiny::column(width = 1, ""),
                          shiny::column(width = 5,
                                        shiny::selectInput("xvar", "X-axis variable", axis_vars,
                                                           selected = "trade_open")),
                          shiny::column(width = 6,
                                        shiny::selectInput("yvar", "Y-axis variable", axis_vars, selected = "profit"))
                        )

      ),
      shiny::tabPanel("Portfolio", introBox(ggvisOutput("ggvis_profits"), data.step = 2, data.intro = "Here we will plot
                                              the running proift of the chosen study against the buy and hold stock position.")),
      shiny::tabPanel("Table",
                      introBox(shiny::downloadButton('downloadData', 'Download'), h4(" "), data.step = 4, data.intro = "Click this
                                 button to download the results of the study in .csv format."),
                      introBox(shiny::dataTableOutput('table'), data.step = 3, data.intro = "Here we will provide a table of the
                                 trade results for download."),
                      column(6, DT::DTOutput('DT_table')),
                      column(6, plotOutput('x2', height = 500))),
      shiny::tabPanel("Portfolio ggplot", introBox(ggvisOutput("ggvis_trades"), data.step = 1, data.intro = "After running a study each
                                 individual trade will be plotted here")
      )
      )
    )
  ) # shinydashboard::dashboardBody
)
