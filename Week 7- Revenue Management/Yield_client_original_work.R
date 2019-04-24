#!/usr/bin/env Rscript
source("Yield_connection.R")

library(tidyverse)
library(RM2)

# global vars
BUF_SIZE <- 1000 # we create buffers in advance
ticket_counter <- 0
sold_tickets <- data.frame(time = .POSIXct(rep(NA, BUF_SIZE)),
                           price = as.numeric(rep(NA, BUF_SIZE)),
                           ongoing_time = as.numeric(rep(NA, BUF_SIZE)),
                           capacity = as.numeric(rep(NA, BUF_SIZE)),
                           opt_price = as.integer(rep(NA, BUF_SIZE)))

price_spreads <- data.frame(
  price1 = as.integer(rep(NA, BUF_SIZE)),
  price2 = as.integer(rep(NA, BUF_SIZE)),
  price3 = as.integer(rep(NA, BUF_SIZE))
)
                          
price_changes_counter <- 0
price_changes <- data.frame(time = .POSIXct(rep(NA, BUF_SIZE)),
                            price = as.numeric(rep(NA, BUF_SIZE)) )
initial_timestamp <- Sys.time()      # start-of-program timestamp
plot_timestamp <- initial_timestamp  # we use plot_timestamp to draw the plot once in a second

fare <- sort(c(95, 112, 125), decreasing = TRUE)

intensity <- sort(c(12.1, 10.59, 9.2), decreasing = FALSE)

init_price <- min(fare)

# add initial price into price_changes dataframe:
price_changes_counter <- 1
price_changes[price_changes_counter,] <- list(time=initial_timestamp, price=init_price)


# user defined handler is expected to have two arguments:
### 1) sold (logical vector of unit length) - TRUE when ticket was sold. Otherwise the event is just a heartbeat.
### 2) price (numeric vector of unit length) - sold price (when sold==TRUE)
## and to return a list of 2 variables:
### 1) lst$change_price (logical vector of unit length) - TRUE when operator desires to change ticket price
### 2) lst$price (numeric vector of unit length) - new ticket price
event_handler <- function(sold, price) {
  now <- Sys.time()
  if (sold) {
    # log event if you want
    message(now, ": Sold ticket, price=", price)
    ticket_counter <<- ticket_counter + 1
    sold_tickets[ticket_counter,] <<- list(time=now, price=price)
  }
  else {
    message(now, ": Get Sellin'")
  }
  
  change_price <- FALSE
  new_price <- 0
  total_time <- 10
  
  sold_tickets$capacity[ticket_counter] <<- 100 - ticket_counter
  
  sold_tickets$ongoing_time[ticket_counter] <<- difftime(
    time1 = sold_tickets$time[ticket_counter], 
    time2 = sold_tickets$time[1], 
    units = "mins") %>% 
    as.numeric() %>%
    round(2)
  
  price_spreads[ticket_counter,] <<- EMSRb(Fare = fare, 
                                           Mean = (total_time - sold_tickets$ongoing_time[ticket_counter]) *
                                             intensity, 
                                           Var = (total_time - sold_tickets$ongoing_time[ticket_counter]) *
                                             intensity, 
                                           cap = sold_tickets$capacity[ticket_counter])
  
  sold_tickets$opt_price[ticket_counter] <<- max(fare[price_spreads[ticket_counter,] == 
                                                        sold_tickets$capacity[ticket_counter]])
  
  sold_tickets$future_intensity[ticket_counter] <<- round(
    sold_tickets$capacity[ticket_counter] / 
      (total_time - sold_tickets$ongoing_time[ticket_counter]),
    2
  )
  
  if(ticket_counter > 1){
  
  if (sold_tickets$price[ticket_counter] != sold_tickets$opt_price[ticket_counter]) {
    change_price <- TRUE
    new_price <- sold_tickets$opt_price[ticket_counter]
    }
  }
    
  if (change_price) {
    # update price_changes dataframe (append last value):
    price_changes_counter <<- price_changes_counter + 1
    price_changes[price_changes_counter,] <<- list(time = now, price = new_price)
  }
  
  Draw()
  
  return( list(change_price=change_price, price=new_price) )
}


# plot once in a second
Draw <- function()
{
  now <- Sys.time();
  if (difftime(now, plot_timestamp, unit='sec') >= 1) {
    plot_timestamp <<- now;
    if (ticket_counter > 0) {
      title <- paste0("Tickets sold: ", ticket_counter, 
                      ", current price: ", price_changes$price[price_changes_counter]);
      t <- difftime(sold_tickets$time[1:ticket_counter], initial_timestamp, unit='sec');
      plot(x=t, y=1:length(t), 
           xlim=c(0, difftime(now, initial_timestamp, unit='sec')),
           type='s', xlab='time (seconds)', ylab='tickets sold',
           main=title);
      
      if (price_changes_counter > 0) {
        # draw price changes
        for (i in 1:price_changes_counter) {
          x <- difftime(price_changes$time[i], initial_timestamp, unit='sec')
          abline(v=x, col='red', lwd=2);
          text(x, ticket_counter/2, as.character(price_changes$price[i]), pos=4)
        }
      }
    }
  }
}


#Best training: 98/100 with PnL=11530
#fare <- sort(c(95, 112, 125), decreasing = TRUE)
#intensity <- sort(c(12.1, 10.59, 9.2), decreasing = FALSE)
#(time - time remaining) modifier


# server options
host <- "datastream.ilykei.com"
port <- 30008
login <- 'markpreston@uchicago.edu'
password <- 'rp1Jzr8W'
stream_name <- "Yield"
catch_handler_errors <- TRUE  # we recommend using TRUE during the test and FALSE during homework
# make connection with your personal handler
result <- Connect(host, port, login, password, stream_name, event_handler, catch_handler_errors, init_price)

# remove empty values from buffers
sold_tickets <- sold_tickets[!is.na(sold_tickets$time),]
price_changes <- price_changes[!is.na(price_changes$time),]
price_spreads <- price_spreads[!is.na(price_spreads[,1]),]

# after all you can dump your data/results and analyze it later
dump(c("sold_tickets", "price_changes", "result"), file = "results.txt")

