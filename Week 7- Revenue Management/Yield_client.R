#!/usr/bin/env Rscript
source("Yield_connection.R")

# global vars
BUF_SIZE <- 1000 # we create buffers in advance
ticket_counter <- 0
sold_tickets <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),
                           price=as.numeric(rep(NA, BUF_SIZE)) )
price_changes_counter <- 0
price_changes <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),
                            price=as.numeric(rep(NA, BUF_SIZE)) )
initial_timestamp <- Sys.time()      # start-of-program timestamp
plot_timestamp <- initial_timestamp  # we use plot_timestamp to draw the plot once in a second

ticket_prices <- c(60, 80, 100, 120, 140, 160, 180, 200, 220, 240, 260)

init_price <- ticket_prices[1]
  
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
    
    # example how to change prices
    if (ticket_counter == 20 && sold) {
        change_price <- TRUE
        new_price <- 120
    }
    
    if (change_price) {
        # update price_changes dataframe (append last value):
        price_changes_counter <<- price_changes_counter + 1
        price_changes[price_changes_counter,] <<- list(time=now, price=new_price)
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

# after all you can dump your data/results and analyze it later
dump(c("sold_tickets", "price_changes", "result"), file = "results.txt")

