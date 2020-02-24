# install.packages("shiny")
library(shiny)
# install.packages("ggplot2")
library(ggplot2)

ui = fluidPage(
  titlePanel("Visualizing the Diamonds Data"),
  sidebarLayout(
    sidebarPanel(
      helpText("This app is designed to help visualize 
               diamonds data. It provides a summary by cut
               and clarity."),
      textInput(inputId = "title", label = "Enter the Title here:"),
      textInput(inputId = "xlabel", label = "Enter the X axis label here"),
      textInput(inputId = "ylabel", label = "Enter the Y axis label here:"),
      selectInput(inputId = "choices", label = "Select the type of chart",
                  choices = c("Stack"="stack", 
                              "Side-by-Side" = "dodge"))
    ),
    mainPanel(plotOutput(outputId = "plot"))
    
  )
)


server = function(input, output){
  output$plot <- renderPlot(ggplot(diamonds, aes(x = cut, fill = clarity)) +
                              geom_bar(position = input$choices) + 
                              ggtitle(input$title) + 
                              xlab(input$xlabel) +
                              ylab(input$ylabel)  
  )
}

shinyApp(ui, server)
