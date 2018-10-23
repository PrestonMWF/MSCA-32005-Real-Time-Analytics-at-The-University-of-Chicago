library(tidyverse)

#!/usr/bin/env Rscript
source("hft2_connection.R")

library(MASS)
load('hft2_probit_model.Rdata')  # load probit model
load('hft2_decomp_model.Rdata')  # load decomposition model

# global vars 
## !!! barrier should be changed for test assignment !!!
barrier <- 194500  # ES barrier multiplied by 100
tick <- 25         # ES tick is multiplied by 100 since prices are also multiplied by 100
position <- 0      # current position
# create buffers in advance:
trades_counter <- 0                                               # market trades event counter
orderexec_counter <- 0                                            # order execution event counter
BUF_SIZE <- 2000                                                  # we create buffers in advance:
trades_df <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),         # dataframe for ES and VX market trades
                        symbol=character(BUF_SIZE),               # 'ES' for s&p500 futures, 'VX' for vix futures
                        trade_price=as.integer(rep(NaN, BUF_SIZE)),
                        trade_size=as.integer(rep(NaN, BUF_SIZE)),
                        trade_side=factor(rep(NA, BUF_SIZE), levels = c('B','A')),  # bid or ask trade side
                        stringsAsFactors = F
)

executed_orders <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),   # dataframe for executed orders
                              side=factor(rep(NA, BUF_SIZE), levels = c('BOUGHT','SOLD')),
                              quantity=as.integer(rep(NaN, BUF_SIZE)),
                              price=as.integer(rep(NaN, BUF_SIZE)),
                              forced=as.logical(rep(NA, BUF_SIZE))
)

start_time <- Sys.time()
plot_timestamp <- Sys.time()  # last plotting time, we use it to plot the graph once in a second

# we use local_df to store values used for predictions
probit_local_df <- data.frame(y1=factor(rep("0", BUF_SIZE),levels=m1$xlevels$y1),
                              y2=factor(rep("0", BUF_SIZE),levels=m1$xlevels$y2),
                              y3=factor(rep("0", BUF_SIZE),levels=m1$xlevels$y3),
                              v1=as.numeric(rep(0, BUF_SIZE)),
                              v2=as.numeric(rep(0, BUF_SIZE)),
                              v3=as.numeric(rep(0, BUF_SIZE)),
                              vix=as.numeric(rep(0, BUF_SIZE)),
                              last_es_price=as.numeric(rep(NA, BUF_SIZE)),
                              pch=as.numeric(rep(NA, BUF_SIZE)),
                              cut_pch=as.numeric(rep(NA, BUF_SIZE)))

decomp_local_df <- data.frame(aim1=as.numeric(rep(0, BUF_SIZE)),
                              dim1=as.numeric(rep(0, BUF_SIZE)),
                              sim1=as.numeric(rep(0, BUF_SIZE)),
                              k=as.numeric(rep(0, BUF_SIZE)),
                              cross_prob=as.numeric(rep(0, BUF_SIZE)))

last_es_price <- 0

## markettrade_handler is expected to have 4 arguments:
### 1) symbol('ES' or 'VX')
### 2) trade_price (integer vector of unit length)
### 3) trade_size (integer vector of unit length)
### 4) trade_side('A' or 'B')
## and to return 1 to buy@ask, -1 to sell@bid and 0 to do nothing.


markettrade_handler <- function(symbol, trade_price, trade_size, trade_side) {
  now <- Sys.time()
  # update trades_df dataframe:
  trades_counter <<- trades_counter + 1
  trades_df[trades_counter,] <<- list(now, symbol, trade_price, trade_size, trade_side)
  
  probit_local_df$vix[trades_counter] <<- ifelse(
    trades_df$symbol[trades_counter] == "VX", 
    trades_df$trade_price[trades_counter],
    probit_local_df$vix[trades_counter - 1]
  )
  
  if (symbol == 'ES' && trades_counter >  3) {
    probit_local_df$events[trades_counter] <<- trades_counter
    probit_local_df$last_es_price[trades_counter] <<- trades_df$trade_price[trades_counter - 1]
    
    probit_local_df$pch[trades_counter] <<- (trade_price - probit_local_df$last_es_price[trades_counter]) / tick
    probit_local_df$cut_pch[trades_counter] <<- min(1, max(-1, probit_local_df$pch[trades_counter]))  # cut too high jumps
    
    probit_local_df$y1[trades_counter] <<- probit_local_df$pch[trades_counter - 1]
    probit_local_df$y1[trades_counter] <<- ifelse(is.na(probit_local_df$y1[trades_counter]),
                                                  0,
                                                  probit_local_df$y1[trades_counter])
    
    
    probit_local_df$y2[trades_counter] <<- probit_local_df$pch[trades_counter - 2]
    probit_local_df$y2[trades_counter] <<- ifelse(is.na(probit_local_df$y2[trades_counter]),
                                                  0,
                                                  probit_local_df$y2[trades_counter])
    
    probit_local_df$y3[trades_counter] <<- probit_local_df$pch[trades_counter - 3]
    probit_local_df$y3[trades_counter] <<- ifelse(is.na(probit_local_df$y3[trades_counter]),
                                                  0,
                                                  probit_local_df$y3[trades_counter])
    
    probit_local_df$v3[trades_counter] <<- probit_local_df$v1[trades_counter - 2]
    probit_local_df$v2[trades_counter] <<- probit_local_df$v1[trades_counter - 1]
    probit_local_df$v1[trades_counter] <<- trade_size
    
    probit_local_df$barrier_price_diff[trades_counter] <<- barrier - probit_local_df$last_es_price[trades_counter] - tick
    
    decomp_local_df$aim1[trades_counter] <<- ifelse(probit_local_df$pch[trades_counter] == 0, 0, 1)
    decomp_local_df$dim1[trades_counter] <<- sign(probit_local_df$pch[trades_counter])
    decomp_local_df$sim1[trades_counter] <<- abs(probit_local_df$pch[trades_counter])
    decomp_local_df$k[trades_counter] <<- (barrier - trades_df$trade_price[trades_counter]) / tick
    decomp_local_df$cross_prob[trades_counter] <<- 1 - (pch_decomposition_cdf(decomp_local_df$k[trades_counter],
                                                                              decomp_local_df$aim1[trades_counter], 
                                                                              decomp_local_df$dim1[trades_counter], 
                                                                              decomp_local_df$sim1[trades_counter], 
                                                                              decomp_params))
    
  }
  
  if (symbol == 'VX' && trades_counter >  3){
    probit_local_df$last_es_price[trades_counter] <<- trades_df$trade_price[trades_counter - 1]
  }
  
  probit_predictions <<- as.data.frame(predict(m1, probit_local_df, type = "p")) %>%
    rename_all(function(x) paste0(x, "_pred")) %>%
    mutate_all(function(x) ifelse(is.na(x), 0, x))
  
  
  
  # now calculate the signal
  # default signal = 0 means nothing to do
  
  
  #best test score: 100 / 100
  #PnL: -575 on 20 trades (5 forced)
  #barrier threshold at .3
  
  barrier_threshold <- .3
  
  signal <- 0
  
    if (trades_counter >  3 && position < 1){
      if (probit_local_df$last_es_price[trades_counter]  >= barrier){
        if(sum(probit_predictions$`0_pred`[trades_counter] + 
               probit_predictions$`-1_pred`[trades_counter] +
               decomp_local_df$cross_prob[trades_counter], na.rm = T) / 2 > barrier_threshold){
          signal <- +1
          }
        }
      }
  
  if (trades_counter >  3 && position >= 1 && trades_df$symbol[trades_counter] == "ES"){
    if (probit_local_df$last_es_price[trades_counter] < barrier){
      if(sum(probit_predictions$`0_pred`[trades_counter] + 
             probit_predictions$`-1_pred`[trades_counter] +
             decomp_local_df$cross_prob[trades_counter], na.rm = T) / 2 < barrier_threshold){
        signal <- -1
      }
      
      if (probit_local_df$last_es_price[trades_counter] - barrier < -15000){
      signal <- 0
        }
      }
  } 
  
  if (signal != 0)
    message(now, " : ","Sending signal: ", signal)
  else
    Draw()  # drow only when signal==0 to send signal faster
  
  return(signal)
}


# orderexec_handler is expected to have 4 arguments:
## 1) side ('BOUGHT' or 'SOLD')
## 2) quantity (int vector of unit length), always positive
## 3) price (int vector of unit length)
## 4) forced (bool vector of unit length) - TRUE when the order is initiated by the system 
# and to return nothing
orderexec_handler <- function(side, quantity, price, forced) {
  now <- Sys.time()
  message(now, " : ", side, " @ ", price, " (forced=", forced, ")")
  # update executed_orders dataframe
  orderexec_counter <<- orderexec_counter + 1
  executed_orders[orderexec_counter,] <<- list(now, side, quantity, price, forced)
  # update current position
  if (side == 'BOUGHT')
    position <<- position + quantity
  else if (side == 'SOLD')
    position <<- position - quantity
  
  Draw() # plot once in a second
  
  return
}


# draw graph once in a second
Draw <- function()
{
  now <- Sys.time()
  if (difftime(now, plot_timestamp, unit="sec") >= 1) {
    plot_timestamp <<- now
    if (trades_counter > 0) {
      good_trades <- trades_df[1:trades_counter,]
      es_trades <- good_trades[good_trades$symbol == 'ES',]
      if (nrow(es_trades) > 0) {
        trades_time_values <- difftime(es_trades$time, start_time, units="sec")
        # trades:
        plot (x=trades_time_values, y=es_trades$trade_price, type='s', col='blue', 
              ylim=c(min(es_trades$trade_price, barrier)-tick, max(es_trades$trade_price, barrier)+tick),
              xlab="time (seconds)", ylab="price")
        # barrier line:
        abline(h=barrier, col='black') 
        if (orderexec_counter > 0) {
          # executed orders:
          exord_good_df <- executed_orders[1:orderexec_counter,]
          client_buy_idx  <- (exord_good_df$side == 'BOUGHT') & (!exord_good_df$forced)
          client_sell_idx <- (exord_good_df$side == 'SOLD') & (!exord_good_df$forced)
          forced_buy_idx  <- (exord_good_df$side == 'BOUGHT') & (exord_good_df$forced)
          forced_sell_idx <- (exord_good_df$side == 'SOLD') & (exord_good_df$forced)
          tr_time_values <- difftime(exord_good_df$time, start_time, units="sec") 
          points(x=tr_time_values[client_buy_idx], 
                 y=exord_good_df$price[client_buy_idx],
                 pch=19, col='green') # green circles
          points(x=tr_time_values[client_sell_idx], 
                 y=exord_good_df$price[client_sell_idx],
                 pch=19, col='red')   # red circles
          points(x=tr_time_values[forced_buy_idx], 
                 y=exord_good_df$price[forced_buy_idx],
                 pch=15, col='green') # green squares
          points(x=tr_time_values[forced_sell_idx], 
                 y=exord_good_df$price[forced_sell_idx],
                 pch=15, col='red')   # red squares
        }
      }
    }
  }
}


# server options
host <- "datastream.ilykei.com"
port <- 30333
login <- 'markpreston@uchicago.edu'
password <- 'rp1Jzr8W'
stream_name <- "hft2"
catch_handler_errors <- FALSE  # we recommend using TRUE during the test and FALSE during homework
# make connection with your personal handlers
result <- Connect(host, port, login, password, stream_name, markettrade_handler, orderexec_handler, catch_handler_errors);


# remove empty values from buffers
trades_df <- trades_df[!is.na(trades_df$time),]
executed_orders <- executed_orders[!is.na(executed_orders$time),]

probit_local_df <- probit_local_df[!is.na(probit_local_df$last_es_price),]
probit_predictions <- probit_predictions[!is.na(probit_predictions$`0_pred`),]

# after all you can dump your data/results and analyze it later
dump(c("trades_df", "executed_orders", "result"), file = "results.txt")

