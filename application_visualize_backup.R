
# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html





#can we do one more thing after this?

#PDFs2 = standalone reports  yes


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


#number of words?

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
  titlePanel("Word Cloud"),
  
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
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100),
      sliderInput("k",
                  "Number of topics:",
                  min = 1,  max = 50, value = 15),
      actionButton("Start", "Start", width = '100%')
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot")
    )
  )
)


# Define server function  
# Text of the books downloaded from:
# A Mid Summer Night's Dream:
#  http://www.gutenberg.org/cache/epub/2242/pg2242.txt
# The Merchant of Venice:
#  http://www.gutenberg.org/cache/epub/2243/pg2243.txt
# Romeo and Juliet:
#  http://www.gutenberg.org/cache/epub/1112/pg1112.txt

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
        
        #library ud pipe for lemmatize. https://github.com/kwartler/GSERM_Text_Remote_student/blob/master/student_lessons/E_SyntacticParsing_DataSources/scripts/A_UD_syntacticParsing.R
        # for syntactic parsing.
        
        
        #don't stem use udpipe, do lemmatization, don't stemmatize
        
        tmp <- stemDocument(tmp, language = "english")
        # tmp <- stemCompletion(tmp,
        #                       unlist(textfiles),
        #                       type = c("longest"))
        
        
        # Instead of DTM/TDM, just clean the vector w/old functions
        txt <- VCorpus(VectorSource(tmp))
        txt <- cleanCorpus(txt, stops)
        
        
        # Extract the clean text
        txt <- unlist(pblapply(txt, content))
        
        # Remove any blanks, happens sometimes w/tweets bc small length & stopwords
        txt <- pblapply(txt, blankRemoval)
        
        # Lexicalize
        txtLex <- lexicalize(txt)
        
        # Corpus stats
        txtWordCount  <- word.counts(txtLex$documents, txtLex$vocab)
        txtDocLength  <- document.lengths(txtLex$documents)
        
        # LDA Topic Modeling
        # suppose you have a bag of dice (documents)
        # alpha - there is a distribution of the probabilities of how similar they are to each other, are dice similar in size/shape/weight?
        # eta   - there is also a distribution of probabilities for the number of topics inside a single document, are dice 6 sided or other?
        #
        
        # library(topicmodels)
        # data("AssociatedPress", package="topicmodels")
        # dtm <- AssociatedPress[1:10, ]
        # FindTopicsNumber(dtm, topics = 2:10, metrics = "Arun2010", mc.cores = 1L)
        
        
        dtm <- DocumentTermMatrix(VCorpus(VectorSource(txtLex)))
        
        
       
        
        result <- FindTopicsNumber(
          dtm,
          topics = seq(from = 2, to = 15, by = 1),
          metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
          method = "Gibbs",
          control = list(seed = 77),
          verbose = TRUE
        )
        
       # NumTopics = result$topics[which.min(result$Arun2010)]
        NumTopics = input$k 
        
      
      
        #mc.cores NA, integer or, cluster; the number of CPU cores to process models simultaneously. If an integer, create a cluster on the local machine. If a cluster, use but
        #don't destroy it (allows multiple-node clusters). Defaults to NA, which triggers
        #auto-detection of number of cores on the local machine.
      
        
        k       <- NumTopics # number of topics
        numIter <- 25 # number of reviews, it performs random word sampling each time
        alpha   <- 0.02 #see above
        eta     <- 0.02 #see above
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
        
        # # Examine a portion
        # head(t(x))
        
        
        x <- as.data.frame(topics)
        topics <- fit$topics
        
          
        
        RV$x = x 
        
        print(k)
        
      })
    })
    
  })
  
  # Make the wordcloud drawing predictable during a session
  # wordcloud_rep <- repeatable(wordcloud)
  
  
  output$plot <- renderPlot({
    req(RV$x)
    req(input$max)
    x = RV$x
    comparison.cloud(t(x), max.words = input$max)
    
    # v <- terms()
    # wordcloud_rep(names(v), v, scale=c(4,0.5),
    #               min.freq = input$freq, max.words=input$max,
    #               colors=brewer.pal(8, "Dark2"))
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

