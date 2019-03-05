library(shiny)
library(shinydashboard)
library(shinythemes)
library(readr)
library(plotly)
library(tidyverse)


imdb_df <- readRDS("imdb-top100.rds")
colnames(imdb_df) <- c("Title", "Rank", "Length", "Rating", "Metascore", "Genre", "Gross")
genre_counts <- readRDS("genre-counts.rds")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "IMDB Top Movies"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      menuItem("One-Variable Plot", tabName = "onevar", icon = icon("chart-pie")),
      menuItem("Two-Variable Plot", tabName = "twovar", icon = icon("chart-bar")),
      menuItem("Three-Variable Plot", tabName = "threevar", icon = icon("chart-area"))
      )
  ),
  dashboardBody(
    tabItems(
      
      # first tab: table
      tabItem(tabName = "table",
              fluidRow(
                column(12,
                       dataTableOutput('moviestable')
                       )
                )
              ),
      
      # second tab: onevar
      tabItem(tabName = "onevar",
              fluidRow(
                column(3,
                       box(width = NULL, 
                         selectInput("onevar", h3("Variable Selection"), 
                                     choices = c("Length",
                                                 "Rating",
                                                 "Metascore",
                                                 "Gross",
                                                 "Genre"),
                                     selected = "Rating"),
                         selectInput("type", h3("Plot Selection"), 
                                     choices = c("Histogram",
                                                 "Boxplot"),
                                     selected = "Genre"),
                         helpText("Note: Plot selection is not available for the varaible 'genre'."))
                       ),
                column(9,
                       box(width = NULL, solidHeader = TRUE,
                           plotlyOutput("onevarplot")
                       )
                       )
                )
              ),
      
      # third tab: twovar
      tabItem(tabName = "twovar",
              fluidRow(
                column(3,
                       box(width = NULL, 
                           selectInput("twox", h3("X-Variable Selection"), 
                                       choices = c("Length",
                                                   "Rating",
                                                   "Metascore",
                                                   "Gross"),
                                       selected = "Rating"),
                           selectInput("twoy", h3("Y-Variable Selection"), 
                                       choices = c("Length",
                                                   "Rating",
                                                   "Metascore",
                                                   "Gross"),
                                       selected = "Gross"))
                ),
                column(9,
                       box(width = NULL, solidHeader = TRUE,
                           plotlyOutput("twovarplot")
                       )
                )
              )
              ),
      # fourth tab: threevar
      tabItem(tabName = "threevar",
              fluidRow(
                column(3,
                       box(width = NULL, 
                           selectInput("threex", h3("X-Variable Selection"), 
                                       choices = c("Length",
                                                   "Rating",
                                                   "Metascore",
                                                   "Gross"),
                                       selected = "Rating"),
                           selectInput("threey", h3("Y-Variable Selection"), 
                                       choices = c("Length",
                                                   "Rating",
                                                   "Metascore",
                                                   "Gross"),
                                       selected = "Length"),
                           selectInput("threez", h3("Z-Variable Selection"), 
                                       choices = c("Length",
                                                   "Rating",
                                                   "Metascore",
                                                   "Gross"),
                                       selected = "Gross"))
                ),
                column(9,
                       box(width = NULL, solidHeader = TRUE,
                           plotlyOutput("threevarplot")
                       )
                )
              )
      )
      )
      )
)

server <- function(input, output) {
  
  # top 100 movies table to be displayed in the tab "table"
  output$moviestable = renderDataTable(imdb_df)
  
  # one-variable plots to be displayed in the tab "onevar"
  output$onevarplot <- renderPlotly({
    if(input$onevar == "Length"){
      y <- "Minutes"
    }else if(input$onevar == "Rating"){
      y <- "Points (out of 100)"
    }else if(input$onevar == "Metascore"){
      y <- "Points (out of 10)"
    }else if(input$onevar == "Gross"){
      y <- "Dollars"
    }
    
    if(input$onevar == "Genre"){
      plot_ly(genre_counts, labels = ~genre, values = ~count, textposition = 'outside',
              textinfo = 'label') %>%
        add_pie(hole = 0.6) %>%
        layout(title = 'Distribution of Movie Genres', showlegend = F)
    }else{
      if(input$type == "Histogram"){
        ggplotly(ggplot(imdb_df, aes_string(x=input$onevar)) + 
                   geom_histogram(bins = 10, aes(fill = ..count..)) + 
                   xlab(input$onevar) + ylab("Count"))
      }else{
        plot_ly(imdb_df, y = ~get(input$onevar), type = "box", name = input$onevar)%>%
          layout(yaxis = list(title = y))
      }
    }
  })
  
  # two-variable plots to be displayed in the tab "twovar"
  output$twovarplot <- renderPlotly({
    ggplotly(ggplot(imdb_df, aes_string(x=input$twox, y=input$twoy, color=input$twoy)) +
               geom_point(size=1.5, alpha=0.7) + geom_smooth(method = "loess") +
               xlab(input$twox) + ylab(input$twoy))
  })
  
  # three-variable plots to be displayed in the tab "threevar"
  output$threevarplot <- renderPlotly({
    plot_ly(imdb_df, x = ~get(input$threex), y = ~get(input$threey), z = ~get(input$threez), color = ~get(input$threez), alpha = 0.9) %>%
      add_markers() %>%
      colorbar(title = input$threez) %>%
      layout(scene = list(xaxis = list(title = input$threex),
                          yaxis = list(title = input$threey),
                          zaxis = list(title = input$threez)))
  })
  
}

shinyApp(ui, server)