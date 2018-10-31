library(tidyverse)

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

frame1 <- as.data.frame(features_m)

colnames(frame1) = paste0('feat_', 1:nFeatures)

usePackage("caret")
usePackage("randomForest")

load("RealTime_Forest.rda")  
load("RealTime_knn.rda")

# user defined handler
## arguments:
#### 1) visitor_id (integer vector of unit length)
#### 2) features - visitor features (integer vector of length 93)
## returns:
#### vector of predicted probabilities (one probability for each of 5 categories)
#### sum of these probabilities should be equal to 1

#Best test score: 100 / 100
#LogLoss = 0.345966829886076
#Parameters: Q = .5 (Weight between RF and KNN probs)

new_visitor_handler <- function(visitor_id, features) {
  # log event if you need
  # message('NEW VISITOR at ', Sys.time())
  
  # put the features vector of the new customer into the predefined data frame:
  frame1[received_visitors_counter,] <<- features
  
  # generate vector of classification probabilities 
  forest_probs <<- predict(object = forest_train, 
                           newdata =  frame1[received_visitors_counter,],
                           type = "prob")

  knn_probs <<- predict(object = knn_train, 
                        newdata =  frame1[received_visitors_counter,],
                        type = "prob")
  
  Q <- 0.5
  
  prob <<- Q * as.matrix(forest_probs) + (1 - Q) * as.matrix(knn_probs)
  
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
  frame1 <- frame1[1:received_visitors_counter,]
  prob_m <- prob_m[1:received_visitors_counter,]
}
# after all you can dump your data/result and analyze it later
dump(c("id_v", "features_m", "prob_m", "result"), file = "results.txt")
