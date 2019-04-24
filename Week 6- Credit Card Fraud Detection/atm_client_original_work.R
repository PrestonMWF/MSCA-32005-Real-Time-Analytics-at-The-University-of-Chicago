#!/usr/bin/env Rscript
source("atm_connection.R")

library(tidyverse)

load("custParams.rda")
load("globalPar.rda")

customer_params <- customer_params %>%
  rownames_to_column(var = "customer_id") %>%
  select(customer_id, everything())

# global vars
event_counter <- 0
BUF_SIZE <- 1000                                                  # we create buffer in advance:
event_data <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),        # dataframe for received events
                         visit_id=as.integer(rep(NA, BUF_SIZE)),
                         atm_signal=character(10),
                         alert_signal=as.integer(rep(NA, BUF_SIZE)),
                         stringsAsFactors=FALSE)

signal_track <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),
                           visit_id=as.integer(rep(NA, BUF_SIZE)),
                           customer_id=as.numeric(rep(NA,BUF_SIZE)),
                           message = character(10),
                           value = as.numeric(rep(NA, BUF_SIZE)))

signal_track$message <- as.character(signal_track$message)

initial_timestamp <- Sys.time()
alerts_counter <- 0

cardNumber <- ''   # current card number
errorCode <- 0
t0PIN <- 0         # input PIN started timestamp
t0 <- 0            # amount selected timestamp
PINAccepted <- FALSE
MesID <- ''

# user defined handler
## event handler is expected to have two arguments:
### 1) visit_id (integer vector of unit length),
### 2) atm_signal (character vector of unit length);
## and to return integer alert signal which is sent back to server
event_handler <- function(visit_id, atm_signal) {
  # log event if you need
  now <- Sys.time()
  message(now, ': visit_id=', visit_id, ', atm_signal=', atm_signal)
  
  # store event in event_data dataframe (alert signal is set at the end of handler)
  event_counter <<- event_counter + 1
  event_data[event_counter,] <<- list(now, visit_id, atm_signal, NA)
  
  signal_track$message[event_counter] <<- as.character(parse_line(line = atm_signal)[1])
  signal_track$value[event_counter] <<-  as.character(parse_line(line = atm_signal)[2])
  
  signal_track$time[event_counter] <<- event_data$time[event_counter]
  signal_track$visit_id[event_counter] <<- event_data$visit_id[event_counter]
  signal_track$customer_id[event_counter] <<- ifelse(
    as.numeric(signal_track$value[event_counter]) > 3000, 
    signal_track$value[event_counter], 
    NA
  )

  alert_signal <- 0
  
  if (event_counter > 1){
    
    signal_track$customer_id[event_counter] <<- signal_track$customer_id[event_counter - 1] 
    
    signal_track$time[event_counter] <<- event_data$time[event_counter]
    signal_track$visit_id[event_counter] <<- event_data$visit_id[event_counter]
    signal_track$message[event_counter] <<- as.character(parse_line(line = atm_signal)[1])
    signal_track$value[event_counter] <<-  as.character(parse_line(line = atm_signal)[2])
    
    signal_track$pin_time_low[event_counter] <<- ifelse(
      signal_track$customer_id[event_counter] == customer_params$customer_id,
      customer_params$low[customer_params$customer_id == 
                            as.character(signal_track$customer_id[event_counter])],
      NA
    ) %>% na.omit() %>%
      as.numeric()
    
    signal_track$pin_time_high[event_counter] <<- ifelse(
      signal_track$customer_id[event_counter] == customer_params$customer_id,
      customer_params$high[customer_params$customer_id == 
                            as.character(signal_track$customer_id[event_counter])],
      NA
    ) %>% na.omit() %>%
      as.numeric()
    
    if(signal_track$visit_id[event_counter] == signal_track$visit_id[event_counter - 1]){
      
      if (signal_track$message[event_counter] == "PIN_INIT_START"){
        pin_start <<- signal_track$time[event_counter]
      }
      
      if (signal_track$message[event_counter] == "PIN_STOP"){
        pin_time <<- difftime(signal_track$time[event_counter], pin_start) %>%
          abs() %>%
          as.numeric()
        
        pin_entered <<- TRUE
        
        if (pin_time >= signal_track$pin_time_high[event_counter] |
            pin_time <= signal_track$pin_time_low[event_counter]){
          
          alert_signal <- 1
          alerts_counter <<- alerts_counter + 1
        }
        
      }
      
      if (signal_track$message[event_counter] == "AMOUNT_SEL"){
        withdraw_start <<- signal_track$time[event_counter]
      }
      
      if (signal_track$message[event_counter] == "WDR_INIT_START" && pin_entered){
        withdraw_time <<- difftime(signal_track$time[event_counter], withdraw_start) %>%
          abs() %>%
          as.numeric()
        
        if (withdraw_time >= globalPar$perf){
          alert_signal <- 1
          alerts_counter <<- alerts_counter + 1
        }
      }
      
      if (pin_entered == F){
        alert_signal <- 1
        alerts_counter <<- alerts_counter + 1
      }
      
    if(signal_track$message[event_counter] == "ESD_SENSOR" &&
       as.numeric(signal_track$value[event_counter]) > globalPar$ESD){
        alert_signal <- 1
        alerts_counter <<- alerts_counter + 1
      }
    }
    
    if(signal_track$visit_id[event_counter] != signal_track$visit_id[event_counter - 1]){
      signal_track$customer_id[event_counter] <<- ifelse(
        signal_track$value[event_counter] > 3000, 
        signal_track$value[event_counter], 
        NA
      )
      
    }
  }
  
  # store alert_signal in event_data dataframe
  event_data$alert_signal[event_counter] <<- alert_signal
  
  return(alert_signal)
}

#19 points is the initial penalty with no added logic

#11 penalty with withdraw time and ESD logic

parse_line <- function (line) {
  str = unlist(strsplit(line,"[ ,]"))
  MessageID <- str[2]
  Value <- ifelse(length(str) >= 5, str[5], NA)
  return(list(MesID=MessageID, value=Value))
}


# server options
host <- "datastream.ilykei.com"
port <- 30006
login <- "markpreston@uchicago.edu"
password <- "rp1Jzr8W"
stream_name <- "ATM"
catch_handler_errors <- TRUE  # we recommend using TRUE during the test and FALSE during homework
# make connection with your personal handler
result <- Connect(host, port, login, password, stream_name, event_handler, catch_handler_errors)


# remove empty values from buffers
event_data <- event_data[!is.na(event_data$time),]
signal_track <- signal_track[!is.na(signal_track$time),]

# after all you can dump your data/result and analyze it later
dump(c("event_data", "result"), file = "results.txt")
