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
library(treemap)
library(plotly)



# Dataset
Albums <- read_csv("./Album.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

# Introduction
  fluidRow(
    column(12,
      titlePanel("Introduction"),
      "This data shows the 10 best-selling albums per year for the last 30 years. "
    ),
    
  ),

#Q1
  fluidRow(
    column(12,
           titlePanel("How does the development of album sales and the year correlate?"),
           "This shows the amount of albums sold each year",
           fluidRow(
             column(6,
                plotOutput(outputId = "salesBar"),
             ),
             column(6,
                plotOutput(outputId = "salesArea"),
             )
           )
    )
  ),

#Q2
  fluidRow(
    column(12,
           titlePanel("Which genres sell best throughout the years?"),
           "This shows how well each genre sells each year",
           fluidRow(
             column(12,
                    plotOutput(outputId = "genreLine"),
                    sliderInput(inputId = "yearSlider", label = "Year", min = min(Albums$Year), max = max(Albums$Year), value = max(Albums$Year), step = 1, animate = TRUE)),
                    checkboxGroupInput(
                      inputId = "genreCheckbox",
                      label = "Select genres",
                      choices = c("Rock", "Pop", "Hip Hop", "R&B", "Country", "Classical", "World", "Other"),
                      selected = c("Rock", "Pop", "Hip Hop", "R&B")
                      , inline = TRUE)
             , align = 'center')
           )
    ),

#Q3
  fluidRow(
      column(12,
            titlePanel("How many times are artists repeated?"),
            "This shows how many albums in top 10 each artist has had throughout the last 30 years",
            fluidRow(
              column(6,
                     plotOutput(outputId = "albumNumberOrderedColumn"),
                     
              ),
              column(6,
                     plotOutput(outputId = "albumNumberTreeMap"),
              )
            )
      )
    ),


#Q4
  fluidRow(
      column(12,
            titlePanel("What artist is recommended (sells the best) if the user prefers a specific genre?"),
            "This shows how well each genre has sold, and can also recommend an album and an artist, based on your taste in genre",
            selectInput(
              inputId = "genre", label = "Choose your preferred genre: ",
              choices = c("Blues", "Classical", "Country", "EDM", "Hip Hop", "Jazz", "Pop", "R&B", "Rock", "World"),
              selected = "Pop"
            ),

            fluidRow(
              column(6,
                      plotOutput(outputId = "genreBar")
              ),
              column(6,
                      plotOutput(outputId = "artistBar")
              )
            ),
            fluidRow(
              column(12, DT::dataTableOutput(outputId = "genreTable"))
            )
      )
    ),

# SHOULD THIS BE REMOVED???? line 111-137
  titlePanel("Playground"),
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
      downloadLink("downloadData", "Want to read about the dashboard? Download the report by clicking this text.")
    ),

    mainPanel(
      plotOutput(outputId = "scatterplot", brush = "plot_brush"),
      DT::dataTableOutput(outputId = "albumTable"),

      br(),
      br(),
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Setup
  # The palette with grey:
  cbPalette <- c("#77AADD", "#EE8866", "#EEDD88", "#FFAABB", "#99DDFF", "#44BB99", "#BBCC33", "#AAAA00", "#DDDDDD")

  # The palette with black:
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  # To use for fills, add
  scale_fill_manual(values = cbPalette)

  # To use for line and point colors, add
  scale_colour_manual(values = cbPalette)
  
  #Download link
  output$downloadData <- downloadHandler(
    filename = "testText.pdf",
    content = function(file) {
      file.copy("testText.pdf", file)
    }
  )
  
  #Q1
  arrows <-
    tibble(
      x1 = c(2010, 2017),
      x2 = c(2011, 2016),
      y1 = c(120687708, 75000000 + 10000000),
      y2 = c(92149234 + 5000000, 43026811 + 5000000)
    )

  salesByYear <- Albums %>%
    complete(Year, Genre, fill = list(Sales = 0)) %>%
    mutate(Genre = fct_lump_n(Genre, n = 4)) %>%
    group_by(Year, Genre) %>%
    summarise(TotalSales = sum(Sales)) %>%
    arrange(desc(TotalSales))

  output$salesBar <- renderPlot({
    ggplot(salesByYear) +
      geom_bar(aes(x = Year, y = TotalSales, fill = Genre), position = "stack", stat = "identity") +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = cbPalette) +
      geom_curve(
        data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
        arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
        color = "gray20", curvature = 0.15) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      scale_y_continuous(name = "Total Units Sold", labels = scales::comma) +
      scale_x_continuous("Year", labels = as.character(salesByYear$Year), breaks = salesByYear$Year) +
      annotate("text", x = 2010, y = 120687708 + 10000000, hjust = 0,
               label = "Adele released her \nbest selling album '21'") +
      annotate("text", x = 2014 - 0.3, y = 15687708 + 80000000, hjust = 0,
               label = "Spotify hits \n123 million active users")
  })

  output$salesArea <- renderPlot({
    ggplot(pos = "identity") +
      geom_area(salesByYear, mapping = aes(x = Year, y = TotalSales, fill = Genre)) +
      geom_point(salesByYear, mapping = aes(x = Year, y = TotalSales, fill = Genre), position = "stack", size = 1) +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = cbPalette) +
      geom_curve(
        data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
        arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
        color = "gray20", curvature = 0.15) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      scale_y_continuous(name = "Total Units Sold", labels = scales::comma) +
      scale_x_continuous("Year", labels = as.character(salesByYear$Year), breaks = salesByYear$Year) +
      annotate("text", x = 2010, y = 120687708 + 10000000, hjust = 0,
               label = "Adele released her \nbest selling album '21'") +
      annotate("text", x = 2014 - 0.3, y = 15687708 + 80000000, hjust = 0,
               label = "Spotify hits \n123 million active users")
  })

  #Q2
  q2_prepared_data <- Albums %>%
    group_by(Year, Genre) %>%
    summarise(TotalSales = sum(Sales))

  q2_genres <- unique(q2_prepared_data$Genre)
  q2_years <- unique(q2_prepared_data$Year)

  Q2_MAX_DECIMALS <- 1
  Q2_MAX_GENRES <- 7

  q2_max_sold <- max(q2_prepared_data$TotalSales)
  q2_y_breaks <- seq(0, q2_max_sold, by = 5e6)
  q2_y_labels <- paste0(q2_y_breaks / 1e6, "M")

  calculate_abbriviation <- function(sales) {
    sales <- as.numeric(sales)
    count <- ifelse(sales <= 0, paste0("0"), paste0(round(sales / 1e6, Q2_MAX_DECIMALS), "M"))
    return(count)
  }

  # identify the top MAX_GENRES genres
  q2_top_genres <- q2_prepared_data %>%
    group_by(Genre) %>%
    summarise(TotalSales = sum(TotalSales)) %>%
    arrange(desc(TotalSales)) %>%
    slice(1:Q2_MAX_GENRES) %>%
    pull(Genre)

  # populate the genres that are not existent every year with 0
  for (genre in q2_genres) {
    for (year in q2_years) {
      if (nrow(q2_prepared_data[q2_prepared_data$Year == year & q2_prepared_data$Genre == genre, ]) == 0) {
        q2_prepared_data <- rbind(q2_prepared_data, data.frame(Year = year, Genre = genre, TotalSales = 0))
      }
    }
    if (!(genre %in% q2_top_genres)) {
      q2_prepared_data <- q2_prepared_data %>% mutate(Genre = ifelse(Genre == genre, "Other", Genre))
    }
  }

  # only show the genres that are selected
  q2_selected_data <- reactive({

    # remove all the entries that are above the selected year
    q2_prepared_data <- q2_prepared_data %>%
      filter(Year <= input$yearSlider)

    # only show the genres that are selected
    q2_prepared_data %>%
    filter(Genre %in% input$genreCheckbox)
  })

  output$genreLine <- renderPlot({
    ggplot(q2_selected_data(), aes(x = Year, y = TotalSales, by = 1, color = Genre)) +
      geom_line(stat = "identity", linewidth = 1) +
      geom_point(size = 2) +
      scale_colour_manual(values = cbPalette) +
      scale_fill_manual(values = cbPalette) +
      geom_label(data = q2_selected_data() %>%
                filter(Year == max(q2_selected_data()$Year)),
                aes(x = Year, y = TotalSales, label = paste0(Genre, ": ", calculate_abbriviation(TotalSales)), fill = Genre),
                size = 4, hjust = 0.5, vjust = 0.5, colour = "white", fontface = "bold") +
      scale_x_continuous(name = 'Year', breaks = q2_years) +
      scale_y_continuous(name = 'Total Sales', breaks = seq(0, q2_max_sold, by = 5e6), labels = q2_y_labels) +
      theme_bw() +
      theme(axis.line = element_line(color = 'black'),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom")
  })
  
  # Q3
  
  Albums %>% count(Genre)
  repeatedArtists <- Albums %>% count(Artist, sort = TRUE)
  amountOfRepeats <- repeatedArtists %>% group_by(n) %>% tally()
  
  # Bar Plot
  output$albumNumberOrderedColumn <- renderPlot({
    counts <- table(repeatedArtists)
    barplot(counts, 
            main="Number of Albums in top 10 by the same artist",
            xlab="Number of albums",
            ylab="Number of artists"
            ) 
  })
  
  
  # Treemap
  # Create data
  group <- c(amountOfRepeats$n)
  value <- c(amountOfRepeats$nn)
  data <- data.frame(group,value)
  
  # treemap
  output$albumNumberTreeMap <- renderPlot({
  treemap(data,
          index="group",
          vSize="value",
          type="index",
          palette = cbPalette,
          title = "",
  )
  })
  
  # Pie chart
  # Create Data
  Prop <- c(amountOfRepeats$nn)
  pieLabels <- amountOfRepeats$n
  
  output$albumNumberPieChart <- renderPlot({
  # You can change the border of each area with the classical parameters:
  pie(Prop , labels = c(pieLabels), border="black", col=cbPalette, radius = 1, cex = 0.8 )
  })
  
  #Q4
  output$genreBar <- renderPlot({
    ggplot(musicByGenre10()) +
      geom_bar(aes(x = Sales, y = reorder(albumAndArtist, + Sales)), stat = "identity") +
      scale_fill_manual(values = cbPalette) +
      scale_x_continuous(name = "Total Album Sales", labels = scales::comma) +
      ylab("Album Name") +
      ggtitle(paste("If you like", input$genre, "may we suggest these albums?"))
  })

  salesByArtist <- Albums %>%
    group_by(Artist, Genre) %>%
    summarise(ArtistSales = sum(Sales))

  salesByArtistR <- reactive({ head(arrange(subset(salesByArtist, (Genre == input$genre)), desc(ArtistSales)), n = 10) })

  output$artistBar <- renderPlot({
    ggplot(salesByArtistR()) +
      geom_bar(aes(x = ArtistSales, y = reorder(Artist, + ArtistSales)), stat = "identity") +
      scale_fill_manual(values = cbPalette) +
      scale_x_continuous(name = "Total Album Sales", labels = scales::comma) +
      ylab("Artists") +
      ggtitle(paste("Best selling artists in:", input$genre))
  })

  musicByGenre <- reactive({ subset(Albums, (Genre == input$genre)) })

  albumArtist <- unite(Albums, albumAndArtist, c(Album, Artist), sep = " - ", remove = FALSE)
  musicByGenre10 <- reactive({ head(arrange(subset(albumArtist, (Genre == input$genre)), desc(Sales)), n = 10) })

  output$genreTable <- renderDataTable(musicByGenre() %>%
                                         select("Year", "Album", "Genre", "Artist", "Minutes", "Sales"),
                                       options = list(
                                         order = list(list(6, 'dsc')),
                                         pageLength = 10
                                       )
  )


  # Playground
  AlbumsWithOthersGenre <- Albums %>%
    mutate(Genre = fct_lump_n(Genre, n = 7))

  output$scatterplot <- renderPlot({
    ggplot(data = AlbumsWithOthersGenre, aes_string(x = input$x, y = input$y, color = "Genre")) +
      geom_point() +
      scale_colour_manual(values = cbPalette)
  })

  output$albumTable <- renderDataTable({
    brushedPoints(Albums, brush = input$plot_brush) %>%
      select(Year, Album, Artist, Minutes, Sales)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
