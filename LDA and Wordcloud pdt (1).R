





###issues: line 79 
###
###
###

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



# Set the current working directory
setwd("/cloud/project/PDFs")

# Bring in supporting functions
source("/cloud/project/Z_otherScripts/ZZZ_supportingFunctions.R")


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
files <- list.files("/cloud/project/PDFs", pattern = "pdf$")
textfiles <- lapply(files, pdf_text)

# verify how many files are in the textfiles and check the length of each pdf file
length(textfiles)
lapply(textfiles, length) 


# Options, Functions and Stopwords
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')
stops <- c(stopwords('SMART'), 'report')


# pdfdata <- VCorpus(URISource(files), readerControl = list(reader = readPDF))# creating a PDF database


#unlist to single char vector
tmp <- unlist(textfiles)

# preprocessing
tmp <- iconv(tmp, "latin1", "ASCII", sub="")
tmp <- rm_bracket(tmp , pattern=c("square", "round", "curly", "")) 
#tmp <- replace_abbreviation(tmp) 

#library ud pipe for lemmatize. https://github.com/kwartler/GSERM_Text_Remote_student/blob/master/student_lessons/E_SyntacticParsing_DataSources/scripts/A_UD_syntacticParsing.R
# for syntactic parsing.


#don't stem use udpipe, do lemmatization, don't stemmatize

tmp <- stemDocument(tmp, language = "english")
tmp <- stemCompletion(tmp,
               unlist(textfiles),
               type = c("longest"))

tmp[2]

# Instead of DTM/TDM, just clean the vector w/old functions
txt <- VCorpus(VectorSource(tmp))
txt <- cleanCorpus(txt, stops)

content(txt[[2]])


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

library(topicmodels)

dtm <- DocumentTermMatrix(VCorpus(VectorSource(txtLex)))


#parameter optimization 
library(ldatuning)

result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)


FindTopicsNumber_plot(result)

result[which.min(result$Arun2010),]

#provide efficient frontiert
library(psel)


#mc.cores NA, integer or, cluster; the number of CPU cores to process models simultaneously. If an integer, create a cluster on the local machine. If a cluster, use but
#don’t destroy it (allows multiple-node clusters). Defaults to NA, which triggers
#auto-detection of number of cores on the local machine.


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
top.words <- top.topic.words(fit$topics, 5, by.score=TRUE)
top.words


wordcloud(words = top.words,
          freq = as.vector(txtWordCount),
          max.words    = 50,
          random.order = FALSE,
          rot.per      = 0.35,
          scale        = c(2,1))



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




topics <- fit$topics
x <- as.data.frame(topics)

# Examine a portion
head(t(x))

  topics <- fit$topics
x <- as.data.frame(topics)
head(t(x))

rawProp <- prop.table(t(x), 2)
head(rawProp)

# Visual?
library(wordcloud)
comparison.cloud(t(x), max.words = 5)


normalizedTopics <- t(x)/(colSums(t(x)) + 0.00001)
head(normalizedTopics)

# Top N by column
ntermsWithValues <- 5
topNTerms <- list()
for(i in 1:ncol(rawProp)){
  y <- rawProp[,i]
  y <- sort(y, decreasing = T)
  y <- head(y, ntermsWithValues)
  y <- data.frame(term  = names(y),
                  ldaVal  = y,
                  topic = i,
                  row.names = NULL)
  topNTerms[[i]] <- y
}
do.call(rbind, topNTerms)






function (topics, num.words = 5, by.score = FALSE) 
{
  if (by.score) {
    normalized.topics <- topics/(rowSums(topics) + 0.00001)
    scores <- apply(normalized.topics, 2, function(x) x * 
                      (log(x + 0.00001) - sum(log(x + 0.00001))/length(x)))
    apply(scores, 1, function(x) colnames(scores)[order(x, 
                                                        decreasing = TRUE)[1:num.words]])
  }
  else {
    apply(topics, 1, function(x) colnames(topics)[order(x, 
                                                        decreasing = TRUE)[1:num.words]])
  }
}


serVis(ldaJSON)
tmp2 <- fromJSON(ldaJSON)

install.packages("jsonlite")
library(jsonlite)