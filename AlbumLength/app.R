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
library(tidyr)


# Dataset
Albums<-read_csv("./Album.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  br(),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "y", label = "Y-axis:",
        choices = c("Mintues", "Hours", "Sales", "Tracks"),
        selected = "Sales"
      ),
      
      selectInput(
        inputId = "x", label = "X-axis:",
        choices = c("Year", "Minutes", "Genre"),
        selected = "critics_score"
      ),
      selectInput(
        inputId = "genre", label = "Choose your preferred genre: ",
        choices = c("Blues", "Classical", "Country", "EDM", "Hip Hop", "Jazz", "Pop", "R&B", "Rock", "World"),
        selected = "Pop"
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "scatterplot", brush = "plot_brush"),
      DT::dataTableOutput(outputId = "moviestable"),
      
      br(),
      plotOutput(outputId = "salesBar"),
      plotOutput(outputId = "salesLine"),
      br(),
      DT::dataTableOutput(outputId = "genreTable"),
      plotOutput(outputId = "genreBar")
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
      select(Year, Album, Artist, Minutes, Sales)
  })
  
  
  musicByGenre <- reactive({subset(Albums, (Genre == input$genre))})
  
  albumArtist <- unite(Albums, albumAndArtist, c(Album, Artist), sep = " - ", remove = FALSE)
  musicByGenre10 <- reactive({head(arrange(subset(albumArtist, (Genre == input$genre)),desc(Sales)), n = 10)})
  
  
  
  
  
  
  
  output$genreTable <- renderDataTable(musicByGenre() %>%
     select("Year", "Album", "Genre", "Artist", "Minutes", "Sales"),
     options = list(
       order = list(list(6, 'dsc')),
       pageLength = 10
     )
     )
  
  salesByYear <- Albums %>%
    group_by(Year) %>%
    summarise(TotalSales = sum(Sales))
  
  output$salesBar <- renderPlot({
    ggplot(salesByYear) +
      geom_bar(aes(x = Year, y = TotalSales), stat="identity")
  })
  
  output$salesLine <- renderPlot({
  ggplot(data = salesByYear) + 
    geom_line(mapping = aes(x = Year, y = TotalSales))
  })
  
  output$genreBar <- renderPlot({
    ggplot(musicByGenre10()) +
      geom_bar(aes(x = Sales, y = reorder(albumAndArtist, +Sales)), stat="identity") +
      scale_x_continuous(name="Total Album Sales", labels = scales::comma) +
      ylab("Album Name") +
      ggtitle(paste("If you like", input$genre, "may we suggest these albums:"))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
