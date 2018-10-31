#!/usr/bin/env Rscript
## AirTweet_Client.R
#
# This file should contain student's handler that processes incoming tweets
# from the server in real time. 
####

source('AirTweet_Connection.R')
load("slda.model.Rdata")

# global vars
tweets_counter <- 0                                               # tweet event counter
BUF_SIZE <- 1000                                                  # we create buffers in advance:
received_data <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),     # dataframe for received tweets
                            text=character(BUF_SIZE),
                            negative_prob=as.numeric(rep(NaN, BUF_SIZE)),
                            neutral_prob=as.numeric(rep(NaN, BUF_SIZE)),
                            positive_prob=as.numeric(rep(NaN, BUF_SIZE)),
                            stringsAsFactors=FALSE )

doc.list <<- list()

predictions <<- list()

aircompanies_accounts <- c('VirginAmerica', 'United', 'SouthwestAir', 
                           'JetBlue', 'Delta', 'USAirways', 'AmericanAir')

other_sw <- c('fly', 'flying', 'flight', 'flights', 'plane')

tweet_clean <- function(tweets){
  stop_words <- c(tm::stopwords("SMART"), tolower(aircompanies_accounts), other_sw)
  
  tweets <- gsub("http[^[:space:]]+", " ", tweets)  
  
  tweets <- gsub("[^a-zA-Z']+", " ", tweets)        
  
  tweets <- gsub("(^ | $)", "", tweets)       
  
  tweets <- tolower(tweets)
}


# user defined handler
## arguments:
#### text - character vector of unit length
## returns:
#### sentiment probability vector of length 3
new_tweet_handler <- function(text) {
  now <- Sys.time()
  # log event
  message(now, ' : ', text)
  
  # update dataframe (store probability after classification)
  tweets_counter <<- tweets_counter + 1
  received_data[tweets_counter,] <<- list(now, text, NaN, NaN, NaN)
  
  received_data$text[tweets_counter] <<- tweet_clean(received_data$text[tweets_counter])
  
  doc.list[tweets_counter] <<- strsplit(received_data$text[tweets_counter], " ")
  
  term.table <<- table(unlist(doc.list))
  
  term.table <<- sort(term.table, decreasing = TRUE)
  
  get.terms <- function(x) {
    index <- match(x, vocab)
    index <- index[!is.na(index)]
    rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
  }
  
  documents <<- lapply(doc.list, get.terms)
  
  predictions[tweets_counter] <<- round(slda.predict(documents[tweets_counter],
                                               fit$topics, 
                                               fit$model,
                                               alpha=alpha,
                                               eta=eta), 2)
  
  sentiment_probabilities <<- tweet_prob_generator(x = predictions[tweets_counter])
  
  #log result
  message('Predicted probabilities: ', paste0(sentiment_probabilities, ' '))
  
  #store probabilities in global dataframe
  received_data$negative_prob[tweets_counter] <<- sentiment_probabilities[1]
  received_data$neutral_prob[tweets_counter] <<- sentiment_probabilities[2]
  received_data$positive_prob[tweets_counter] <<- sentiment_probabilities[3]
  
  return(sentiment_probabilities)
}

#training result: 100 / 100 (pentalty of 0.95059333779265)
#model included 2 topics

# server options
host <- "datastream.ilykei.com"
port <- 30009
login <- "markpreston@uchicago.edu"
password <- "rp1Jzr8W"
stream_name <- "AirTweet"
catch_handler_errors <- TRUE  # we recommend using TRUE during the test and FALSE during homework
# make connection with your personal handler
result <- Connect(host, port, login, password, stream_name, new_tweet_handler, catch_handler_errors)

# remove empty values from buffers
received_data <- received_data[!is.na(received_data$time),]

# after all you can dump your data/result and analyze it later
dump(c("received_data", "result"), file = "results.txt")

