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
library(forcats)


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
      plotOutput(outputId = "salesBar"),
      plotOutput(outputId = "salesLine")
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
    complete(Year, Genre, fill = list(Sales = 0)) %>%
    mutate(Genre = fct_lump_n(Genre, n = 4)) %>%
    group_by(Year, Genre) %>%
    summarise(TotalSales = sum(Sales)) %>%
    arrange(desc(TotalSales))
  
  output$salesBar <- renderPlot({
    ggplot(salesByYear) +
      geom_bar(aes(x = Year, y = TotalSales, fill = Genre), position = "stack", stat="identity") +
      geom_curve(
        data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
        arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
        color = "gray20", curvature = 0.15) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      scale_y_continuous(name="Total Units Sold", labels = scales::comma) +
      scale_x_continuous("Year", labels = as.character(salesByYear$Year), breaks = salesByYear$Year) +
      annotate("text", x = 2010, y = 120687708+10000000, hjust = 0,
               label = "Adele released her \nbest selling album '21'") +
      annotate("text", x = 2014-0.3, y = 15687708+80000000, hjust = 0,
               label = "Spotify hits \n123 million active users")
  })
  
  arrows <- 
    tibble(
      x1 = c(2010, 2017),
      x2 = c(2011, 2016),
      y1 = c(120687708, 75000000+10000000), 
      y2 = c(92149234+5000000, 43026811+5000000)
    )
  output$salesLine <- renderPlot({
  ggplot(pos = "identity") + 
      geom_area(salesByYear, mapping = aes(x = Year, y = TotalSales, fill = Genre)) +
      geom_point(salesByYear, mapping = aes(x = Year, y = TotalSales, fill = Genre), position = "stack", size = 1) +
      geom_curve(
        data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
        arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
        color = "gray20", curvature = 0.15) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      scale_y_continuous(name="Total Units Sold", labels = scales::comma) +
      scale_x_continuous("Year", labels = as.character(salesByYear$Year), breaks = salesByYear$Year) +
      annotate("text", x = 2010, y = 120687708+10000000, hjust = 0,
               label = "Adele released her \nbest selling album '21'") +
      annotate("text", x = 2014-0.3, y = 15687708+80000000, hjust = 0,
               label = "Spotify hits \n123 million active users")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
