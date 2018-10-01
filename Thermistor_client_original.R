library(tidyverse)

#!/usr/bin/env Rscript
source("Thermistor_connection.R")

# global vars
temp_events_counter <- 0                                        # new indication event counter
signals_counter <- 0                                            # our signals counter
BUF_SIZE <- 10000                                               # we create buffers in advance:
temp_events <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),     # dataframe for new temperature observations 
                          temp=as.numeric(rep(NaN,BUF_SIZE)),
                          rolling_diff=as.numeric(rep(NaN,BUF_SIZE)),
                          diff_sum=as.numeric(rep(NaN,BUF_SIZE)),
                          is_stable=as.numeric(rep(0,BUF_SIZE)),
                          rolling_rle=as.numeric(rep(NaN,BUF_SIZE)),
                          signal_sent=as.logical(rep(NA,BUF_SIZE)))

initial_timestamp <- Sys.time()      # start-of-program timestamp
plot_timestamp <- initial_timestamp  # we use plot_timestamp to draw the plot once in a second

# user defined handler
## arguments:
#### rcvd_temp - received temperature (numeric vector of unit length)
## returns:
#### list(send_signal,is_stable,temp) where 
#### -- list$send_signal is a boolean vector of unit length which value is TRUE when we want to send signal back to server
#### -- list$is_stable is a boolean vector of unit length which value is TRUE when the process becomes stable 
####    and FALSE when the process becomes unstable
#### -- list$temp is an object temperature estimate (numeric vector of unit length)
new_indication_handler <- function(rcvd_temp) {
  now <- Sys.time()
  # log event if you want
  #message("EVENT at ", now, ", temp=", rcvd_temp)
  
  # update temp_events dataframe (append last value):
  temp_events_counter <<- temp_events_counter + 1
  temp_events$time[temp_events_counter] <<- now
  temp_events$temp[temp_events_counter] <<- rcvd_temp
  temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
  temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
  
  
  # this is just an example how handler may decide the process is stable
  send_signal <- FALSE
  is_stable <- NA
  stable_temp <- NaN
  
  if(temp_events_counter > 500 && temp_events_counter <= 1000){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    }
  }
  
  if(temp_events_counter > 1000 && temp_events_counter <= 1500){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS UNSTABLE NOW")
      send_signal <- TRUE
      is_stable <- FALSE
      stable_temp <- rcvd_temp
    }
  }
  
  if(temp_events_counter > 1500 && temp_events_counter <= 2000){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS UNSTABLE NOW")
      send_signal <- TRUE
      is_stable <- FALSE
      stable_temp <- rcvd_temp
    }
  }
  
  if(temp_events_counter > 2000 && temp_events_counter <= 2500){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS UNSTABLE NOW")
      send_signal <- TRUE
      is_stable <- FALSE
      stable_temp <- rcvd_temp
    }
  }
  
  if(temp_events_counter > 2500 && temp_events_counter <= 3000){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS UNSTABLE NOW")
      send_signal <- TRUE
      is_stable <- FALSE
      stable_temp <- rcvd_temp
    }
  }
  
  if(temp_events_counter > 3000 && temp_events_counter <= 3500){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS UNSTABLE NOW")
      send_signal <- TRUE
      is_stable <- FALSE
      stable_temp <- rcvd_temp
    }
  }

  if(temp_events_counter > 3500 && temp_events_counter <= 4000){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS UNSTABLE NOW")
      send_signal <- TRUE
      is_stable <- FALSE
      stable_temp <- rcvd_temp
    }
  }
  
  if(temp_events_counter > 4000 && temp_events_counter <= 4500){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS UNSTABLE NOW")
      send_signal <- TRUE
      is_stable <- FALSE
      stable_temp <- rcvd_temp
    }
  }
  
  if(temp_events_counter > 4500 && temp_events_counter <= 5000){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS UNSTABLE NOW")
      send_signal <- TRUE
      is_stable <- FALSE
      stable_temp <- rcvd_temp
    }
  }
  
  if(temp_events_counter > 5000 && temp_events_counter <= 5500){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS UNSTABLE NOW")
      send_signal <- TRUE
      is_stable <- FALSE
      stable_temp <- rcvd_temp
    }
  }
  
  if(temp_events_counter > 5500 && temp_events_counter <= 6000){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS UNSTABLE NOW")
      send_signal <- TRUE
      is_stable <- FALSE
      stable_temp <- rcvd_temp
    }
  }
  
  if(temp_events_counter > 6000 && temp_events_counter <= 6500){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS UNSTABLE NOW")
      send_signal <- TRUE
      is_stable <- FALSE
      stable_temp <- rcvd_temp
    }
  }
  
  if(temp_events_counter > 6500 && temp_events_counter <= 7000){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS UNSTABLE NOW")
      send_signal <- TRUE
      is_stable <- FALSE
      stable_temp <- rcvd_temp
    }
  }
  
  if(temp_events_counter > 7000 && temp_events_counter <= 7500){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS UNSTABLE NOW")
      send_signal <- TRUE
      is_stable <- FALSE
      stable_temp <- rcvd_temp
    }
  }
  
  if(temp_events_counter > 7500 && temp_events_counter <= 8000){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS UNSTABLE NOW")
      send_signal <- TRUE
      is_stable <- FALSE
      stable_temp <- rcvd_temp
    }
  }
  
  if(temp_events_counter > 8000 && temp_events_counter <= 8500){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS UNSTABLE NOW")
      send_signal <- TRUE
      is_stable <- FALSE
      stable_temp <- rcvd_temp
    }
  }
  
  if(temp_events_counter > 8500 && temp_events_counter <= 9000){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS UNSTABLE NOW")
      send_signal <- TRUE
      is_stable <- FALSE
      stable_temp <- rcvd_temp
    }
  }
  
  if(temp_events_counter > 9000 && temp_events_counter <= 9500){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS UNSTABLE NOW")
      send_signal <- TRUE
      is_stable <- FALSE
      stable_temp <- rcvd_temp
    }
  }
  
  if(temp_events_counter > 9500 && temp_events_counter <= 10000){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - 500):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < 0.35, 1, 0)
    
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS STABLE NOW")
      send_signal <- TRUE
      is_stable <- TRUE
      stable_temp <- rcvd_temp
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 68) {
      message(now, "PROCESS IS UNSTABLE NOW")
      send_signal <- TRUE
      is_stable <- FALSE
      stable_temp <- rcvd_temp
    }
  }
  
  if (send_signal) {
    # update signals dataframe (append last value):
    signals_counter <<- signals_counter + 1
    signals[signals_counter,] <<- list(now, is_stable, stable_temp)
  }
  
  
  Draw()
  
  return( list(send_signal=send_signal, is_stable=is_stable, temp=stable_temp) )  # return list of 3 objects
}

# plot once in a second
Draw <- function()
{
    now <- Sys.time()
    if (difftime(now, plot_timestamp, unit='sec') >= 1) {
        plot_timestamp <<- now;
        if (temp_events_counter > 0) {
            # draw temperature chart
            plot(x= difftime(temp_events$time[1:temp_events_counter], initial_timestamp, unit="sec"),
                 y=temp_events$temp[1:temp_events_counter], 
                 type='s', xlab='time (seconds)', ylab='temp');
            
            if (signals_counter > 0) {
                # draw stable and unstable points
                good_signals <- signals[1:signals_counter,]
                points(x=difftime(good_signals$time[good_signals$is_stable], initial_timestamp, unit="sec"),
                       y=good_signals$stable_temp[good_signals$is_stable],
                       pch=19, col='green');  # green circles for stable points
                points(x=difftime(good_signals$time[!good_signals$is_stable], initial_timestamp, unit="sec"),
                       y=good_signals$stable_temp[!good_signals$is_stable],
                       pch=19, col='red');  # red circles for unstable points
            }
        }
    }
}

#best score: 80 (error term of 120)
#settings: 500 groups, .35 abs diff, rle of 68

#improvement thoughts
#raise threshold for stability to .3
#change rolling window
#change rle trigger
#account for direction change- don't use absolute difference, use relative

#analyzing the output

temp_events <- temp_events %>%
  mutate(signal_sent = case_when(
    is_stable == 1 & rolling_rle == 68 ~ "stable",
    is_stable == 0 & rolling_rle == 68 ~ "unstable",
    rolling_rle != 68 ~ "none"),
    is_stable = ifelse(is_stable == 1, "stable", "unstable"),
    events = seq(1, nrow(temp_events), 1)) %>%
  select(events, everything())

temp_events %>%
  ggplot(aes(events, temp)) +
  geom_point(aes(colour = is_stable)) +
  geom_point(aes(colour = signal_sent)) +
  scale_x_continuous(breaks = seq(1, 10000, 500))

write_csv(temp_events, "Thermistor_temperature_output_data.csv")

# server options
host <- "datastream.ilykei.com"
port <- 30001
login <- "markpreston@uchicago.edu";
password <- "rp1Jzr8W";
stream_name <- "Thermistor"
catch_handler_errors <- TRUE  # we recommend using TRUE during the test and FALSE during homework
# make connection with your personal handler
result <- Connect(host, port, login, password, 
                  stream_name, new_indication_handler, catch_handler_errors)

# remove empty values from buffers
temp_events <- temp_events[!is.na(temp_events$time),]
signals <- signals[!is.na(signals$time),]

# after all you can dump your data/results and analyze it later
dump(c("temp_events", "signals", "result"), file = "results.txt")


