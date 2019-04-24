library(tidyverse)
library(lda)

tweets <- read.csv("air_tweet_train.csv", stringsAsFactors = F, header = F) %>%
  rename(Sentiment = V1,
         Tweet = V2)

aircompanies_accounts <- c('VirginAmerica', 'United', 'SouthwestAir', 
                           'JetBlue', 'Delta', 'USAirways', 'AmericanAir')

other_sw <- c('fly', 'flying', 'flight', 'flights', 'plane')

stop_words <- c(tm::stopwords("SMART"), tolower(aircompanies_accounts), other_sw)

texts <- tweets$Tweet

texts <- gsub("http[^[:space:]]+", " ", texts)  

texts <- gsub("[^a-zA-Z']+", " ", texts)        

texts <- gsub("(^ | $)", "", texts)       

texts <- tolower(texts)

sentiment <- tweets$Sentiment

sentiment.f <- rep('Neutral', length(sentiment))

sentiment.f[sentiment < 0] <- 'Negative'

sentiment.f[sentiment > 0] <- 'Positive'

sentiment.f <- as.factor(sentiment.f)

doc.list <- strsplit(texts, " ")

term.table <- table(unlist(doc.list))

term.table <- sort(term.table, decreasing = TRUE)

del_idx <- names(term.table) %in% stop_words | term.table < 5 | nchar(names(term.table)) < 3

term.table <- term.table[!del_idx]

vocab <- names(term.table)

get.terms <- function(x) { # x is vector of words
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}

documents <- lapply(doc.list, get.terms)

notempty_documents_idx <- sapply(documents, function(doc) ncol(doc) > 0)

(corp<-summary(notempty_documents_idx))

set.seed(1017)

num.topics <- 2

alpha <- 0.1 

eta <- 0.1

params <- sample(c(-1,0, 1), num.topics, replace = TRUE)

fit <- slda.em(documents=documents[notempty_documents_idx],
               K=num.topics,
               vocab=vocab,
               num.e.iterations=100,
               num.m.iterations=40,
               alpha=alpha, #Dirichlet distribution parmeter for prior for theta_d
               eta=eta,  #Dirichlet distribution parameter for prior for beta_k
               annotations=sentiment[notempty_documents_idx], 
               params=params,  #numeric vector of initial regression coefficients for EM
               variance=0.25, # initial value for response variance
               method="sLDA")

summary(fit$model)

predictions <- slda.predict(documents,
                            fit$topics, 
                            fit$model,
                            alpha=alpha,
                            eta=eta)

probability_mapping <- function(x, y){
  binIdx <- findInterval(predictions, c(x, y)) == 1
  
  probVec<-table(sentiment[binIdx])
  
  probVec<-probVec/sum(probVec)
}

x <- list(-1.5, -.89, -.79, -.69, -.59, -.49, -.39, -.29, -.19, -.09)
y <- list(-.9, -.8, -.7, -.6, -.5, -.4, -.3, -.2, -.1, 0)

neg_probs <- map2(x, y, function(x, y) probability_mapping(x = x, y = y))

y <- list(1.5, .79, .59, .39, .19)
x <- list(.8, .6, .4, .2, 0)

pos_probs <- map2(x, y, function(x, y) probability_mapping(x = x, y = y))

tweet_prob_generator <- function(x){
  if (x <= -.9){
    return(neg_probs[[1]])
  }
  
  if (x <= -.8 && x >= -.89){
    return(neg_probs[[2]])
  }
  
  if (x <= -.7 && x >= -.79){
    return(neg_probs[[3]])
  }
  
  if (x <= -.6 && x >= -.69){
    return(neg_probs[[4]])
  }
  
  if (x <= -.5 && x >= -.59){
    return(neg_probs[[5]])
  }
  
  if (x <= -.4 && x >= -.49){
    return(neg_probs[[6]])
  }
  
  if (x <= -.3 && x >= -.39){
    return(neg_probs[[7]])
  }
  
  if (x <= -.2 && x >= -.29){
    return(neg_probs[[8]])
  }
  
  if (x <= -.1 && x >= -.19){
    return(neg_probs[[9]])
  }
  
  if (x < 0 && x >= -.09 ){
    return(neg_probs[[10]])
  }
  
  if (x == 0){
    return(pos_probs[[5]])
  }
  
  if (x <= .19 && x > 0){
    return(pos_probs[[5]])
  }
  
  if (x <= .39 && x >= .2 ){
    return(pos_probs[[4]])
  }
  
  if (x <= .59 && x >= .4 ){
    return(pos_probs[[3]])
  }
  
  if (x <= .79 && x >= .6 ){
    return(pos_probs[[2]])
  }
  
  if (x >= .8){
    return(pos_probs[[1]])
  }
}

nan_omit <- is.nan(predictions)

tweet_probs <- round(predictions[nan_omit == F], 2)

func_test <- map(tweet_probs, function(x) tweet_prob_generator(x = x))

head(func_test, 20)

save(list=c('stop_words', 'vocab', 'get.terms', 
            'alpha', 'eta', 'fit', 'tweet_prob_generator', 
            "pos_probs", "neg_probs"),
     file = "slda.model.Rdata")
