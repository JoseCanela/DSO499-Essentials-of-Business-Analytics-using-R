# install.packages("shiny")
# install.packages(c("quantmod", "ggplot2", "dplyr", "tidyverse", "lubridate", "xts", "scales", "kableExtra"))
library(shiny)
library(lubridate)
library(quantmod)
library(help = quantmod)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(xts)
library(scales)
library(kableExtra)
# install.packages("zoo")
library(zoo)

# Define a UI for the app
ui = fluidPage(
  # Title of the app
  titlePanel("Visualizing Historical Stock Prices from Yahoo Finance"),
  fluidRow(
    column(
      4,
      wellPanel(
        textInput("symbol", "Symbol", value = "AAPL"),
        # Give the user the ability to choose the type of plot (line, bar chart, etc...)
        # NOTE: The quantmod package's default plot is auto-selected
        selectInput(
          "type",
          "Type",
          choices = c("auto", "line", "bars", "candlesticks", "matchsticks"),
          selected = "auto"
        )
        ,
        # Give the user the ability to choose what chartSeries theme (background color) they'd like
        selectInput(
          "theme",
          "Theme",
          choices = c("black", "white"),
          selected = "black"
        ),
        # Give the user the ability to choose whether or not they want to add
        # the stock volume data to the plot figure
        checkboxInput("Volume", "Volume", value = T),
        # Give the user the ability to choose whether or not they want to add
        # the Bollinger Bands to the plot
        checkboxInput("Bollinger", "Bollinger Bands", value = F),
        # Give the user the ability to choose the date range that
        # their plot will represent
        dateRangeInput(
          'dateRange',
          label = 'Date Range Input: yyyy-mm-dd',
          start = '2010-01-01',
          end = Sys.Date()
        ),
        # Give the user ability to choose the date range options
        # that are found on Yahoo Finances's historical stock
        # price page: 
        # (a) 1M  = The past month 
        # (b) 3M  = The past 3 months 
        # (c) 6M  = The past 6 months
        # (d) YTD = From the start of the current year to the current date
        # (e) 1Y  = The past year
        # (f) 2Y  = The past 2 years
        # (g) 5Y  = The past 5 years
        # (h) 10Y = The past 10 years
        # (i) MAX
        # NOTE: "MAX" is auto-selected and refers to the largest date range available
        # in Yahoo Finance's records.
        radioButtons(
          "tspan",
          "Time Span:",
          choices = c(
            "Use above dates",
            "1M",
            "3M",
            "6M",
            "YTD",
            "1Y",
            "2Y",
            "5Y",
            "10Y",
            "MAX"
          ),
          selected = "MAX",
          inline = T
        ),
        # Give the user ability to choose the type of aggregation method
        # they want to use to visualize the stock prices.
        radioButtons(
          "aggs",
          "Aggregation:",
          choices = c("hourly",
                      "daily",
                      "weekly",
                      "monthly",
                      "quarterly",
                      "yearly"),
          selected = "hourly",
          inline = F
        )
      )
    ),
    # Displays the plot of the stock prices
    # and some descriptive features
    column(
      width = 8,
      plotOutput("pricePlot"),
      verbatimTextOutput("myText")
    )
  )
)

server = function(input, output) {
  output$pricePlot <- renderPlot({
    # First, let's load the historical stock prices from Yahoo Finance
    stock_prices <-
      getSymbols(input$symbol, src = 'yahoo', from = '1900-01-01')
    # Let's create a closure for the stock price data so that
    # we remember the data for the symbol we chose.
    get.stock.prices <<- get(input$symbol)
    # Let's now make starting dates for our time span option
    if (input$tspan == "1M") {
      strDate = paste0(Sys.Date() %m-% months(1), "::")
    }
    else if (input$tspan == "3M") {
      strDate = paste0(Sys.Date() %m-% months(3), "::")
    }
    else if (input$tspan == "6M") {
      strDate = paste0(Sys.Date() %m-% months(6), "::")
    }
    else if (input$tspan == "YTD") {
      startyr = today()
      month(startyr) = 1
      day(startyr) = 1
      strDate = paste0(as.character(startyr), "::")
    }
    else if (input$tspan == "1Y") {
      strDate = paste0(Sys.Date() - years(1), "::")
    }
    else if (input$tspan == "2Y") {
      strDate = paste0(Sys.Date() - years(2), "::")
    }
    else if (input$tspan == "5Y") {
      strDate = paste0(Sys.Date() - years(5), "::")
    }
    else if (input$tspan == "10Y") {
      strDate = paste0(Sys.Date() - years(10), "::")
    }
    else if (input$tspan == "MAX") {
      strDate = "::"
    }
    # ELSE the starting date will be the one the user inputted
    else{
      strDate = paste0(as.character(input$dateRange[1]),
                       "::",
                       as.character(input$dateRange[2]))
    }
    
    chartSeries(
      get(input$symbol),
      name = input$symbol,
      type = input$type,
      subset = strDate,
      theme = input$theme
    )
    
    # Initializing the technical indicator parameter for chartSeries().
    TA <- NULL
    # IF the user chooses to add volume data, the
    # TA parameter will make the chartSeries function
    # add a volume chart to the plot figure.
    if (input$Volume == T) {
      TA = c(TA, addVo())
    }
    # IF the user chooses to add Bollinger Bands, the
    # TA parameter will make the chartSeries function
    # add a Bollinger Bands to the plot.
    if (input$Bollinger == T) {
      TA = c(TA, addBBands())
    }
    cat(file = stderr(),
        paste0("chartSeries(", input$symbol, ", ", strDate, ")\n"))
    # IF the user does not choose to add a technical indicator to the chart,
    # they will print the default chartSeries plot while taking into
    # consideration the type of aggregation (daily, monthly, yearly, etc..)
    if (is.null(TA)) {
      if (input$aggs == "hourly") {
        chartSeries(
          get(input$symbol),
          name = input$symbol,
          type = input$type,
          TA = NULL,
          subset = strDate,
          theme = input$theme
        )
      }
      else if (input$aggs == "daily") {
        chartSeries(
          to.daily(get(input$symbol)),
          name = input$symbol,
          type = input$type,
          TA = NULL,
          subset = strDate,
          theme = input$theme
        )
      }
      else if (input$aggs == "weekly") {
        chartSeries(
          to.weekly(get(input$symbol)),
          name = input$symbol,
          type = input$type,
          TA = NULL,
          subset = strDate,
          theme = input$theme
        )
      }
      else if (input$aggs == "monthly") {
        chartSeries(
          to.monthly(get(input$symbol)),
          name = input$symbol,
          type = input$type,
          TA = NULL,
          subset = strDate,
          theme = input$theme
        )
      }
      else if (input$aggs == "quarterly") {
        chartSeries(
          to.quarterly(get(input$symbol)),
          name = input$symbol,
          type = input$type,
          TA = NULL,
          subset = strDate,
          theme = input$theme
        )
      }
      else{
        chartSeries(
          to.yearly(get(input$symbol)),
          name = input$symbol,
          type = input$type,
          TA = NULL,
          subset = strDate,
          theme = input$theme
        )
      }
    }
    # ELSE the user will print out a chartSeries with the selected aggregation method
    else{
      if (input$aggs == "hourly") {
        chartSeries(
          to.hourly(get(input$symbol)),
          name = input$symbol,
          type = input$type,
          TA = TA,
          subset = strDate,
          theme = input$theme
        )
      }
      else if (input$aggs == "daily") {
        chartSeries(
          to.daily(get(input$symbol)),
          name = input$symbol,
          type = input$type,
          TA = TA,
          subset = strDate,
          theme = input$theme
        )
      }
      else if (input$aggs == "weekly") {
        chartSeries(
          to.weekly(get(input$symbol)),
          name = input$symbol,
          type = input$type,
          TA = TA,
          subset = strDate,
          theme = input$theme
        )
      }
      else if (input$aggs == "monthly") {
        chartSeries(
          to.monthly(get(input$symbol)),
          name = input$symbol,
          type = input$type,
          TA = TA,
          subset = strDate,
          theme = input$theme
        )
      }
      else if (input$aggs == "quarterly") {
        chartSeries(
          to.quarterly(get(input$symbol)),
          name = input$symbol,
          type = input$type,
          TA = TA,
          subset = strDate,
          theme = input$theme
        )
      }
      else{
        chartSeries(
          to.yearly(get(input$symbol)),
          name = input$symbol,
          type = input$type,
          TA = TA,
          subset = strDate,
          theme = input$theme
        )
      }
    }
  })
  # Let's output (print) stock price info from the
  # first year and last year of the MAX time span.
  output$myText <- renderPrint({
    cat(paste0(input$symbol, "\n"))
    print(head(get.stock.prices))
    print(tail(get.stock.prices))
  })
}

shinyApp(ui, server)