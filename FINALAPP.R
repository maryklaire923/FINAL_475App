library(fpp3)
library(shiny)
library(shinyWidgets)
library(shinydashboard) 
library(plotly)

df <- subset(aus_livestock, aus_livestock$Animal == "Calves" & aus_livestock$State == "New South Wales")


ui <- dashboardPage(
  dashboardHeader( title = "Australia Livestock"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("INFORMATION", tabName = "INFORMATION"),
        menuItem("TimeSeries", tabName = "TimeSeries"),
        menuItem("Seasonality", tabName = "Seasonality"),
        menuItem("Autocorrelation", tabName = "Autocorrelation"),
        menuItem("Decomposition", tabName = "Decomposition"),
        menuItem("ARIMA", tabName = "ARIMA"),
        menuItem("Holts", tabName = "Holts"),
        menuItem("Naive", tabName = "Naive"),
        menuItem("SeasonalNaive", tabName = "SeasonalNaive"),
        menuItem("Mean", tabName = "Mean"),
        menuItem("Drift", tabName = "Drift")
      )
    ),

dashboardBody(
  tabItems(
    tabItem(
      tabName = "INFORMATION",
    h5("Created by: Mary Klaire Osborne"),
    h2("Instructions: This app shows the trends in New South Wales Livestock. It displays graphs based on
                 which type of output is selected."),
    h3("Step 1: Select which type of graph output you're interested in viewing"),
    h3("Step 2: View the graph output displayed")
    ),
    tabItem(
      tabName = "TimeSeries",
      plotlyOutput("TimeSeries")
    ),
    tabItem(
      tabName = "Seasonality",
      plotlyOutput("Seasonality")
    ),
    tabItem(
      tabName = "Autocorrelation",
      plotlyOutput("ACF")
    ),
    tabItem(
      tabName = "Decomposition",
      plotlyOutput("Decomposition")
    ),
    tabItem(
      tabName = "ARIMA",
      plotOutput("ARIMA")
    ),
    tabItem(
      tabName = "Holts",
      plotOutput("Holts")
    ),
    tabItem(
      tabName = "Naive",
      plotOutput("Naive")
    ),
    tabItem(
      tabName = "SeasonalNaive",
      plotOutput("SeasonalNaive")
    ),
    tabItem(
      tabName = "Mean",
      plotOutput("Mean")
    ),
    tabItem(
      tabName = "Drift",
      plotOutput("Drift")
    )
  )
))



#     ,
# 
# h4("Time Series"), plotOutput('TimeSeries'),
# h4("Seasonality"), plotOutput('Seasonality'),
# h4("Autocorrelation"), plotOutput('ACF'),
# h4("Decomposition"), plotOutput('Decomposition'),
# h4("ARIMA"), plotOutput('ARIMA'),
# h4("Holts"), plotOutput('Holts')
# )

server <- function(input, output) { 

  output$TimeSeries <- renderPlotly({
  autoplot(df)
})

output$Seasonality <- renderPlotly ({ 
  gg_season(df, Count)})

output$ACF <- renderPlot({ 
  autoplot(ACF(df, Count))
})

output$Decomposition <- renderPlotly({ 
  autoplot(components(model(df, classical_decomposition(Count)))) 
})

output$ARIMA <- renderPlot ({ fit <- df %>%
  model(
    autoarima = ARIMA(Count),
    manualwdrift = ARIMA(Count ~ 1 + pdq(0, 1, 0))
  )
fc <- fit %>% forecast(h = "3 years")
fc %>%
  autoplot(df)
})

output$Holts <- renderPlot ({fit <- df %>%
  model(
    additive = ETS(Count ~ error("A") + trend("A") +
                     season("A")),
    multiplicative = ETS(Count ~ error("M") + trend("A") +
                           season("M"))
  )
fc <- fit %>% forecast(h = "3 years")
fc %>%
  autoplot(df, level = NULL)
})

output$Naive <- renderPlot ({  fit <- df %>% model(NAIVE(Count))
fc <- fit %>% forecast(h = "3 years")
fc %>%
  autoplot(df)
})

output$SeasonalNaive <- renderPlot  ({   fit <- df %>% model(SNAIVE(Count))
fc <- fit %>% forecast(h = "3 years")
fc %>%
  autoplot(df)
})


output$Mean <- renderPlot ({    fit <- df %>% model(MEAN(Count))
fc <- fit %>% forecast(h = "3 years")
fc %>%
  autoplot(df)
})
 


output$Drift <- renderPlot ({  fit <- df %>% model(NAIVE(Count ~ drift()))
fc <- fit %>% forecast(h = "3 years")
fc %>%
  autoplot(df)
})


}

shinyApp(ui = ui, server = server)





