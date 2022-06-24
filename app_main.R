
#' Title: Case Study GSERM Course
#' Purpose: Apply LDA and other concepts to a set of PDFs
#' Author: Felipe Sick
#' email: felipe.sick@unisg.ch
#' License: no licence
#' Date: June 24, 2022
#'




# Load the required libraries 
library(tm) 
library(wordcloud)
library(RColorBrewer)
library(pdftools)
library(tm)
library(pbapply)
library(lda)
library(LDAvis)
library(dplyr)
library(treemap)
library(qdapRegex)
library(SnowballC)
library(topicmodels)
library(parallel)
library(udpipe)



# Load R packages
library(shiny)
library(shinythemes)
library(tm)
library(wordcloud)
library(memoise)
#parameter optimization
library(ldatuning)

# setwd("C:/Users/felip/Desktop/GSERM_Text_Remote_student-master/GSERM_Text_Remote_student/case_study")


library(tm)
library(wordcloud)
library(memoise)


source("ZZZ_supportingFunctions.R")


### ----------------------

# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("Word Cloud - Case Study"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choose a report type:",
                  choices = books),
      actionButton("update", "Change"),
      hr(),
      #sliderInput("freq",
       #           "Minimum Frequency:",
        #          min = 1,  max = 50, value = 15),
      sliderInput("max",
                  "Number of Words:",
                  min = 1,  max = 300,  value = 100),
      # sliderInput("k",
      #             "Number of topics:",
      #             min = 1,  max = 50, value = 15),
      actionButton("Start", "Start", width = '100%')
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot")
    )
  )
)


# Define server function  


server <- function(input, output, session) {
  
  
  RV = reactiveValues(
    x = NULL
    
  )
  
  
  
  # Change when the "update" button is pressed...
  observeEvent(input$Start,{
    
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        # The list of valid books
        books <- list("Integrated Reports" = "summer",
                       "Standalone reports" = "merchant",
                       "All reports" = "romeo")
        
        texfile = NULL
        
        # load files into the environment
        if (input$selection == "Standalone reports"){
          files <- list.files("PDFs", pattern = "pdf$")
          textfiles <- lapply(paste0("PDFs/",files), pdf_text)
          
        }
        
        if (input$selection == "Integrated Reports"){
          files <- list.files("PDFs", pattern = "pdf$")
          textfiles <- lapply(paste0("PDFs2/",files), pdf_text)
          
        }
    
     

        
        
        # Options, Functions and Stopwords
        options(stringsAsFactors = FALSE)
        Sys.setlocale('LC_ALL','C')
        stops <- c(stopwords('SMART'), 'report')
        
        
        #unlist to single char vector
        tmp <- unlist(textfiles)
        
        # preprocessing
        tmp <- iconv(tmp, "latin1", "ASCII", sub="")
        tmp <- rm_bracket(tmp , pattern=c("square", "round", "curly"))
        #tmp <- replace_abbreviation(tmp)
        
        
        tmp <- stemDocument(tmp, language = "english")
        
        
        # clean the vector w/old functions
        txt <- VCorpus(VectorSource(tmp))
        txt <- cleanCorpus(txt, stops)
        
        
        # Extract the clean text
        txt <- unlist(pblapply(txt, content))
        
        # Remove any blank
        txt <- pblapply(txt, blankRemoval)
        
        # Lexicalize
        txtLex <- lexicalize(txt)
        
        # Corpus stats
        txtWordCount  <- word.counts(txtLex$documents, txtLex$vocab)
        txtDocLength  <- document.lengths(txtLex$documents)
        
        # LDA Topic Modeling

        dtm <- DocumentTermMatrix(VCorpus(VectorSource(txtLex)))
        
        # parameter optimization based on four different metrics,  "Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"
       
        result <- FindTopicsNumber(
          dtm,
          topics = seq(from = 2, to = 15, by = 1),
          metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
          method = "Gibbs",
          control = list(seed = 77),
          verbose = TRUE
        )
        
        NumTopics = result$topics[which.min(result$Arun2010)]
        FindTopicsNumber_plot(result)
      
      
        
        k       <- NumTopics # number of topics
        numIter <- 5 # number of reviews, it performs random word sampling each time
        alpha   <- 0.02 
        eta     <- 0.02
        set.seed(1234)
        fit <- lda.collapsed.gibbs.sampler(documents      = txtLex$documents,
                                           K              = k,
                                           vocab          = txtLex$vocab,
                                           num.iterations = numIter,
                                           alpha          = alpha,
                                           eta            = eta,
                                           initial        = NULL,
                                           burnin         = 0,
                                           compute.log.likelihood = TRUE)
        
        
        topics <- fit$topics
        x <- as.data.frame(topics)
        
        
        
        x <- as.data.frame(topics)
        topics <- fit$topics
        
          
        
        RV$x = x 
        
        
      })
    })
    
  })
  
 
  # show output in a wordcloud
  output$plot <- renderPlot({
    req(RV$x)
    req(input$max)
    x = RV$x
    comparison.cloud(t(x), max.words = input$max)
    

    
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

