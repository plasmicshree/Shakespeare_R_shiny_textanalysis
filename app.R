library(shiny)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(tidytext)
library(RColorBrewer)
#install.packages("shinythemes")
#install.packages("rsconnect")
library(shinythemes)
library(rsconnect)

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

out<- getFreq("romeo",stopwords=TRUE)

head(out)

# task6: add in shinythemes function

ui <- fluidPage(
  theme=shinytheme('united'),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    
    sidebarPanel(
      selectInput("books",
                  "Choose a book:",
                  choices=books),
      checkboxInput("stopwords","Stop Words",
                    value=TRUE),
      actionButton("action","Rerun"),
      
      hr(),
      h3("Word Cloud Setting"),
      
      sliderInput("maxwords","Max # of Words",
                  min=10,max=200,value=100,step=10),
      sliderInput("largestsize","Size of largest word",
                  min=1, max =8, value=4),
      sliderInput("smallestsize","Size of smallest word",
                  min=0.1, max =4, value=0.5),
      hr(),
      h3("Word Count Setting"),
      sliderInput("Min_chart_count","Minimum words for Counts Chart",
                  min=10, max =100, value=25),
      sliderInput("fontsize","Word size for Counts Chart",
                  min=8, max =30, value=14)
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(title="Word Clouds",
                 plotOutput("cloud",height='600px')
                 
                 
                 
                 
                 
                 ),
        tabPanel(title='Word Counts',
                 plotOutput("freq",height='600px')
                 
                 
                 
                 
                 
                 )
      )
      
    )
    
  )
  
  # task3: add in the outputs in the sidebarPanel
  
  # task6: and modify your figure heights
)

server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  freq <- eventReactive(input$action, {
                withProgress({
                  setProgress(message = "Processing corpus...")
                  getFreq(input$books,input$stopwords) # ... = replace with the two inputs from Task 2
                })
      }
      )
                
   output$cloud <- renderPlot({
     v <- freq()
     pal <- brewer.pal(8,"Dark2")
     
     v %>% 
       with(
         wordcloud(
           word, 
           n, 
           scale = c(input$largestsize, input$smallestsize),
           random.order = FALSE, 
           max.words = input$maxwords, 
           colors=pal))
     
   })
   
   output$freq <- renderPlot({
     v <- freq()
     
     v2 <- v %>% filter(n > input$Min_chart_count)
     ggplot(v2,aes(x=reorder(word,n),y=n)) +geom_bar(stat="identity")+
       coord_flip() + theme(text=element_text(size=input$fontsize),
                            axis.title.x=element_blank(),
                            axis.title.y=element_blank())
     
   })
   
}
  

shinyApp(ui = ui, server = server)
