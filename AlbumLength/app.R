#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)


# Dataset
Albums<-read_csv("./Album.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  br(),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "y", label = "Y-axis:",
        choices = c("Minutes", "Hours", "Sales", "Tracks"),
        selected = "audience_score"
      ),
      
      selectInput(
        inputId = "x", label = "X-axis:",
        choices = c("Year", "Minutes", "Genre"),
        selected = "critics_score"
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "scatterplot", brush = "plot_brush"),
      DT::dataTableOutput(outputId = "moviestable"),
      br(),
      plotOutput(outputId = "salesBar")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$scatterplot <- renderPlot({
    ggplot(data = Albums, aes_string(x = input$x, y = input$y, color = "Genre")) +
      geom_point()
  })
  
  output$moviestable <- renderDataTable({
    brushedPoints(Albums, brush = input$plot_brush) %>%
      select(Year, Artist, Album, Minutes, Sales)
  })
  
  salesByYear <- Albums %>%
    group_by(Year) %>%
    summarise(TotalSales = sum(Sales))
  
  output$salesBar <- renderPlot({
    ggplot(salesByYear) +
      geom_bar(aes(x = Year, y = TotalSales), stat="identity")
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
