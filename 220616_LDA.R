#' Title: GSERM REMOTE CASE STUDY
#' Purpose: CSR REPORTS APPLICATION OF TOPIC MODELLING
#' NAME: FELIPE SICK
#' Date: JUN 16 2022
#' 
#' 
#' 
#'current issues: bracketX
#'could not find function "bracketX"              --> is it required?!
#'could not find function "replace_abbreviation"  --> is it required?!
#'
#'
#'URISource on line 70, otherwise error


# Load the required libraries 
library(tm) 
library(wordcloud)
library(RColorBrewer)
library(pdftools)
library(qdap)
library(pbapply)
library(lda)
library(LDAvis)
library(dplyr)
library(treemap)
# Set the current working directory
setwd("C:/Users/felip/Desktop/GSERM_Text_Remote_student-master/GSERM_Text_Remote_student/case_study/data/PDFs")

# Bring in supporting functions
source("C:/Users/felip/Desktop/GSERM_Text_Remote_student-master/GSERM_Text_Remote_student/student_lessons/Z_otherScripts/ZZZ_supportingFunctions.R")


blankRemoval<-function(x){
  x <- unlist(strsplit(x,' '))
  x <- subset(x,nchar(x)>0)
  x <- paste(x,collapse=' ')
}

docAssignment<-function(x){
  x <- table(x) 
  x <- as.matrix(x)
  x <- t(x)
  idx <-max.col(x)
  x <- as.numeric(names(x[1,idx]))
  return(x)
}

# load files into the environment
files <- list.files("C:/Users/felip/Desktop/GSERM_Text_Remote_student-master/GSERM_Text_Remote_student/case_study/data/PDFs", pattern = "pdf$")
textfiles <- lapply(files, pdf_text)

# verify how many files are in the textfiles and check the length of each pdf file
length(textfiles)
lapply(textfiles, length) 


# Options, Functions and Stopwords
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')
stops <- c(stopwords('SMART'))


# pdfdata <- VCorpus(URISource(files), readerControl = list(reader = readPDF))# creating a PDF database



# Preprocessing chain



# String clean up 
textfiles <- iconv(URISource(textfiles), "latin1", "ASCII", sub="")
textfiles <- bracketX(textfiles , bracket="all") 
textfiles <- replace_abbreviation(textfiles ) 
textfiles <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
textfiles <- tm_map(textfiles, removeNumbers)
textfiles <- tm_map(textfiles, content_transformer(tolower))
textfiles <- tm_map(textfiles, stemDocument, language = "english")
textfiles <- tm_map(textfiles, stripWhitespace)
textfiles[1]

# Instead of DTM/TDM, just clean the vector w/old functions
txt <- VCorpus(VectorSource(textfiles))
txt <- cleanCorpus(txt, stops)

head(txt)


# Extract the clean text
txt <- unlist(pblapply(txt, content))

# Remove any blanks, happens sometimes w/tweets bc small length & stopwords
txt <- pblapply(txt, blankRemoval)

# Lexicalize
txtLex <- lexicalize(txt)

# Examine the vocab or key and value pairing between key ()
head(txtLex$vocab, 30) # rememnber #6
length(txtLex$vocab) #8k+ unique words among all articles, each 
head(txtLex$documents[[1]][,1:15]) #look at [,6] & [,10]
head(txtLex$documents[[20]])

# Corpus stats
txtWordCount  <- word.counts(txtLex$documents, txtLex$vocab)
txtDocLength  <- document.lengths(txtLex$documents)

# LDA Topic Modeling
# suppose you have a bag of dice (documents)
# alpha - there is a distribution of the probabilities of how similar they are to each other, are dice similar in size/shape/weight?
# eta   - there is also a distribution of probabilities for the number of topics inside a single document, are dice 6 sided or other?
# 


# tuning parameters. 
# https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html#:~:text=From%20this%20plot%20can%20be,is%20in%20range%2090%2D140.


k       <- 5 # number of topics
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


# explore some of the results
fit$document_sums #topics by articles
head(t(fit$topics)) #words by topics

## Get the top words in the topics
top.words <- top.topic.words(fit$topics, 40, by.score=TRUE)
top.words


head(topic)

wordcloud(words = top.words,
          freq = as.vector(txtWordCount),
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors=brewer.pal(8, "Dark2"))



























# LDAvis params
# normalize the article probabilities to each topic
theta <- t(pbapply(fit$document_sums + alpha, 2, function(x) x/sum(x))) # topic probabilities within a doc will sum to 1

# normalize each topic word's impact to the topic
phi  <- t(pbapply(fit$topics + eta, 1, function(x) x/sum(x)))

ldaJSON <- createJSON(phi = phi,
                      theta = theta, 
                      doc.length = txtDocLength, 
                      vocab = txtLex$vocab, 
                      term.frequency = as.vector(txtWordCount))

serVis(ldaJSON)