source('global.R')

shinyUI(dashboardPage(
  dashboardHeader(title = "NBA Hall of Fame"),
  dashboardSidebar(
    selectizeInput(
      inputId = "selected_player",
      label = "NBA Players",
      choices = players
    ),
    radioButtons(
      inputId = "player_stat",
      label = "Stats",
      choices = player_stat
    ),
    # checkboxGroupInput(
    #   inputId = "comparison",
    #   label = "Compared to",
    #   choices = compare
    # ),
    radioButtons(
      inputId = "position",
      label = "Position",
      choices = position
    ),
    sliderInput(
      "Year",
      label = h3("Year"),
      min = 1960,
      max = 2016,
      value = c(1990, 2000),
      sep = ""
    )
  ),
  dashboardBody(fluidRow(
    tabsetPanel(
      type = "tabs",
      tabPanel("Stats", plotOutput("stat_compare")),
      tabPanel("Table", tableOutput("table"))
    )
  ))
))
