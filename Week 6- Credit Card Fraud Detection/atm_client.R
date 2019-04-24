#!/usr/bin/env Rscript
source("atm_connection.R")

# global vars
event_counter <- 0
BUF_SIZE <- 1000                                                  # we create buffer in advance:
event_data <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),        # dataframe for received events
                         visit_id=as.integer(rep(NA, BUF_SIZE)),
                         atm_signal=character(10),
                         alert_signal=as.integer(rep(NA, BUF_SIZE)),
                         stringsAsFactors=FALSE)

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
    
    
    
    
    # parse ATM signal (output: lst$MesID, lst$value - both characters!)
    lst <- parse_line(atm_signal)
    #print(lst)
    MesID <<- lst$MesID
    if (MesID == 'CARD_INSERT') {
        cardNumber <<- lst$value
    }
    else if (MesID == 'CARD_REMOVED') {
        # clean global vars
        errorCode <<- 0
        t0PIN <<- 0
        t0 <<- 0
        PINAccepted <<- FALSE
    }
    # else ...
    
    # calc alert_signal example
    alert_signal <- 0
    if (difftime(now, initial_timestamp, unit="sec") > 30 && alerts_counter == 0)
    {
        alert_signal <- 1
        alerts_counter <<- alerts_counter + 1
    }
    
    # store alert_signal in event_data dataframe
    event_data$alert_signal[event_counter] <<- alert_signal
    
    return(alert_signal)
}


parse_line <- function (line) {
    str = unlist(strsplit(line,"[ ,]"))  # using <space> and <comma> as delimiters
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

# after all you can dump your data/result and analyze it later
dump(c("event_data", "result"), file = "results.txt")
