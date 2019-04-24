#!/usr/bin/env Rscript
source("Clickstream_connection.R")

# global vars
## we update visitors dataframe on each event inside handler to analyze it later
nFeatures <- 93
received_visitors_counter <- 0                          # event counter
BUF_SIZE <- 10000                                       # we create buffers in advance:
id_v <- integer(BUF_SIZE)                               # visitor ids vector
features_m <- matrix(nrow=BUF_SIZE, ncol=nFeatures)     # visitor features matrix
prob_m <- matrix(nrow=BUF_SIZE, ncol=5)                 # predicted probabilities matrix

frame1 <- data.frame(t(1:nFeatures))                    # prepare dataframe for new features vector
colnames(frame1) = paste0('feat_', 1:nFeatures)


usePackage('rpart') # load library
# load('SavedModel.rda')  # load the tree model saved during offline preparation


# user defined handler
## arguments:
#### 1) visitor_id (integer vector of unit length)
#### 2) features - visitor features (integer vector of length 93)
## returns:
#### vector of predicted probabilities (one probability for each of 5 categories)
#### sum of these probabilities should be equal to 1
new_visitor_handler <- function(visitor_id, features) {
    # log event if you need
    # message('NEW VISITOR at ', Sys.time())
    
    # put the features vector of the new customer into the predefined data frame:
    frame1[received_visitors_counter,] <<- features

    # generate vector of classification probabilities 
    prob<- runif(5)
    prob <- prob/sum(prob)

    # log id, features and predicted probabilities into global vars
    received_visitors_counter <<- received_visitors_counter + 1
    id_v[received_visitors_counter] <<- visitor_id
    features_m[received_visitors_counter,] <<- features
    prob_m[received_visitors_counter,] <<- prob
    
    return(prob)
}


# server options
host <- "datastream.ilykei.com"
port <- 30005
login <- "markpreston@uchicago.edu"
password <- "rp1Jzr8W"
stream_name <- "Clickstream"
catch_handler_errors <- TRUE  # we recommend using TRUE during the test and FALSE during homework
# make connection with your personal handler
result <- Connect(host, port, login, password, stream_name, new_visitor_handler, catch_handler_errors)


# remove empty values from buffers
if (received_visitors_counter > 0) {
    id_v <- id_v[1:received_visitors_counter]
    features_m <- features_m[1:received_visitors_counter,]
    prob_m <- prob_m[1:received_visitors_counter,]
}
# after all you can dump your data/result and analyze it later
dump(c("id_v", "features_m", "prob_m", "result"), file = "results.txt")
