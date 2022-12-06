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
library(tidyr)



# Dataset
Albums<-read_csv("./Album.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  br(),
  headerPanel('Title'),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "y", label = "Y-axis:",
        choices = c("Minutes", "Hours", "Sales", "Tracks"),
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
      plotOutput(outputId = "genreBar"),
      plotOutput(outputId = "artistBar"),
      plotOutput(outputId = "genreViolinChart")
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
    complete(Year, Genre, fill = list(Sales = 0)) %>%
    mutate(Genre = fct_lump_n(Genre, n = 4)) %>%
    group_by(Year, Genre) %>%
    summarise(TotalSales = sum(Sales)) %>%
    arrange(desc(TotalSales))
  
  salesByArtist <- Albums %>%
    group_by(Artist, Genre) %>%
    summarise(ArtistSales = sum(Sales))
  
  salesByArtistR <- reactive({head(arrange(subset(salesByArtist, (Genre == input$genre)),desc(ArtistSales)), n = 10)})
  
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
  
  
  output$genreBar <- renderPlot({
    ggplot(musicByGenre10()) +
      geom_bar(aes(x = Sales, y = reorder(albumAndArtist, +Sales)), stat="identity") +
      scale_x_continuous(name="Total Album Sales", labels = scales::comma) +
      ylab("Album Name") +
      ggtitle(paste("If you like", input$genre, "may we suggest these albums:"))
  })
  
  output$artistBar <- renderPlot({
    ggplot(salesByArtistR()) +
      geom_bar(aes(x = ArtistSales, y = reorder(Artist, +ArtistSales)), stat="identity") +
      scale_x_continuous(name="Total Album Sales", labels = scales::comma) +
      ylab("Artists") + 
      ggtitle(paste("Best selling artists in:",input$genre))
  })
  
  ByGenre <- Albums %>%
    mutate(Genre = fct_lump_n(Genre, n = 4))
  #ByGenre$Sales <- as.factor(ByGenre$Sales)
  
  output$genreViolinChart <- renderPlot({
    ggplot(ByGenre) +
      geom_violin(aes(x = Genre, y = Sales, fill = Genre)) +
      geom_boxplot(aes(x = Genre, y = Sales), width=0.1) +
      #geom_dotplot(aes(x = Genre, y = Sales), binaxis='y', stackdir='center', dotsize=0.3)
      scale_y_continuous(name="Total Album Sales", labels = scales::comma) +
      ggtitle("Violin Chart of Distribution of Sales per Genre")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
