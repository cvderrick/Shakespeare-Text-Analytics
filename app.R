library(shiny)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(shinythemes)
library(RColorBrewer)

books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# task4: add in getFreq function for pre-processing
getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  return(text)
}

ui <- fluidPage(
  
  # task6: add in shinythemes function
  theme = shinytheme("yeti"),
  
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(position = "left",
                sidebarPanel(
  
  # task2: add in the inputs in the sidebarPanel
                  selectInput(inputId = "book", "Choose a book:", 
                              choices = books),
                  checkboxInput(inputId = "stopwords", "Stop words", 
                                value = TRUE),
                  actionButton(inputId = "go", "Rerun"),
                  hr(),
                  h3("Word Cloud Settings"),
                  sliderInput(inputId = "maxwords", "Max # of Words:",
                              min = 10, max = 200, value = 100, step = 10),
                  sliderInput(inputId = "largesize", "Size of Largest Words:",
                              min = 1, max = 8, value = 4),
                  sliderInput(inputId = "smallsize", "Size of Smallest Words:",
                              min = 0.1, max = 4, value = 0.5),
                  hr(),
                  h3("Word Count Settings"),
                  sliderInput(inputId = "mincount", 
                              "Minimum Words for Counts Chart:",
                              min = 10, max = 100, value = 25),
                  sliderInput(inputId = "fontsize", "Word Size for Counts Chart:",
                              min = 8, max = 30, value = 14)
                ),
 
  # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)                
                mainPanel(
                  tabsetPanel(type = "tabs",
  
  # task3: add in the outputs in the mainPanel                             
                              tabPanel("Word Cloud", 
                                       plotOutput("cloud", height = "600px")),
                              tabPanel("Word Counts", plotOutput("freq")),
                  )
                )
  )
  
  # task6: and modify your figure heights
)

server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  freq <- eventReactive(input$go, {
    withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$book, input$stopwords) 
    }) 
  })
  
  output$cloud <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    v %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(input$largesize, input$smallsize),
          random.order = FALSE, 
          max.words = input$maxwords, 
          colors=pal))
  }) 
  
  output$freq <- renderPlot({
    v <- freq()
    v %>% 
      filter(n >= input$mincount) %>%
      ggplot(aes(x = reorder(word, n), y = n)) +
        geom_col() + 
        coord_flip() +
        theme(text = element_text(size = input$fontsize),
              axis.title.x = element_blank(),
              axis.title.y = element_blank())
  }) 
}

shinyApp(ui = ui, server = server)
