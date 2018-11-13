source("Mining_connection.R")

# global vars
incoming_signals_counter <- 0                                   # incoming signals event counter
outgoing_signals_counter <- 0                                   # outgoing signals event counter
BUF_SIZE <- 10000                                               # we create buffers in advance:
inc_signals <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)))     # dataframe for incoming signals 
out_signals <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)))   


signal_track <- data.frame(current_time = as.numeric(rep(NA, BUF_SIZE)),
                           p = as.numeric(rep(NA, BUF_SIZE)),
                           intensity = as.numeric(rep(NA, BUF_SIZE)),
                           time_to_shock = as.numeric(rep(NA, BUF_SIZE)))

eventMoments <- rep(NA, BUF_SIZE)         # time spans from i-th signal and the first signal in minutes, eventMoments[1] = 0 
initial_timestamp <- Sys.time()     # start-of-program timestamp
plot_timestamp <- initial_timestamp # we use plot_timestamp to draw the plot once in a second


# user defined handler
## no arguments
## returns:
#### logical vector of unit length which value is TRUE when we want to send signal back to server
## !! Note that only the first outgoing signal makes sence! Others will be ignored by server.

# This simple code is given as an example only. You can tune "eventRate_barrier" parameter for the
# workshop data, but it won't give you the best result on the test data.
# We will send an alarm signal if estimated event rate exceeds eventRate_barrier.
new_event_handler <- function() {
  now <- Sys.time()
  if(incoming_signals_counter < 0.5){
    initial_timestamp <<- now
  }
  # time in minutes from the start of the stream 
  t <- as.double(difftime(now, initial_timestamp, unit='min'))
  # log event if necessary
  ##message("EVENT at ", now)
  
  # update inc_signals dataframe (append last value):
  incoming_signals_counter <<- incoming_signals_counter + 1
  inc_signals[incoming_signals_counter,] <<- list(now)
  eventMoments[incoming_signals_counter] <<- t
  
  signal_track$current_time[incoming_signals_counter] <<- t
  
  dt <- 0.05
  
  w <- 10 # experiment with window width to estimate intensity
  
  n <- 50 # experiment with number of observations of intensity in the model
  
  t0 <- ((n + w) * dt) # earliest time when regression can be fitted
  
  earliest_fit <- 12
  
  tc_collect <- function(record){
    
    current_time <- signal_track$current_time[record]
    
    t_grid <- seq(current_time - t0 + dt, current_time, by = dt) # grid at t0
    
    events_grid <- findInterval(t_grid, na.omit(signal_track$current_time))
    
    N <- length(t_grid)
    
    intensity <- events_grid[(w+1):N] - events_grid[1:(N-w)]
    
    intensity <- intensity / (dt*w) # events per minute
    
    time_grid <- t_grid[(N-n+1):N] # timeGrid contains t_i, i=1,...n
    
    # Use pmax(x, 0.1) to avoid log(0)
    log_intensity <- log(pmax(intensity, 0.1))
    
    res <- nloptr(x0= current_time+1, 
                  eval_f=regression, 
                  lb=current_time+0.1, 
                  ub=current_time+10, 
                  opts=list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel" = 1e-04),
                  logRate=log_intensity,
                  regressionTimes=time_grid, returnError=TRUE)
    
    tc <- res$solution
    
    p <- regression(res$solution, log_intensity, time_grid, FALSE)
    
    time_to_shock <- tc - current_time
    
    params <- list(p = p, 
                   intensity = tail(intensity, 1), 
                   time_to_shock = time_to_shock)
    
    return(params)
  }
  
  send_signal <- FALSE
  
  if (incoming_signals_counter > earliest_fit){
  
    tc <- tc_collect(record = incoming_signals_counter)  
    
    signal_track[incoming_signals_counter, 2:4] <<- tc
    
    collapse_predictions[incoming_signals_counter,]  <<- predict(
      object = collapse_forest, 
      newdata = signal_track[incoming_signals_counter, ], 
      type = "response"
        )%>% 
      as.data.frame() %>%
      rename(preds = ".") %>%
      mutate(preds = as.numeric(preds) - 1)
    
    collapse_predictions$collapse_rle[incoming_signals_counter] <<- rle(
      x = collapse_predictions$preds[incoming_signals_counter]
        )
    
  }
    
  if (send_signal) {
    # update out_signals dataframe (append last value):
    outgoing_signals_counter <<- outgoing_signals_counter + 1
    out_signals[outgoing_signals_counter,] <<- list(now)
  }
  
  Draw()
  
  return( send_signal )
}


# plot once in a second
Draw <- function()
{
  now <- Sys.time();
  if (difftime(now, plot_timestamp, unit='sec') >= 1) {
    plot_timestamp <<- now;
    if (incoming_signals_counter > 0) {
      t <- difftime(inc_signals$time[1:incoming_signals_counter], initial_timestamp, unit='min');
      plot(x=t, y=1:length(t), 
           xlim=c(0, difftime(now, initial_timestamp, unit='min')),
           type='s', xlab='time (minutes)', ylab='n_events');
      
      if (outgoing_signals_counter > 0) {
        # draw outgoing signal (only the first one)
        abline(v=difftime(out_signals$time, initial_timestamp, unit='min')[1],
               col='red', lwd=2);
      }
    }
  }
}


# server options
host <- "datastream.ilykei.com"
port <- 30004
login <- "markpreston@uchicago.edu"
password <- "rp1Jzr8W"
stream_name <- "Mining"
catch_handler_errors <- TRUE  # we recommend using TRUE during the test and FALSE during homework
# make connection with your personal handler
result <- Connect(host, port, login, password, stream_name, new_event_handler, catch_handler_errors)

# remove empty values from buffers
inc_signals <- inc_signals[!is.na(inc_signals$time),]

out_signals <- out_signals[!is.na(out_signals$time),]

signal_track <- signal_track[!is.na(signal_track$current_time),] %>%
  as.data.frame()

eventMoments <- eventMoments[1:incoming_signals_counter]

alarmTime <- as.double(difftime(out_signals[1], inc_signals[1] , unit='min'))

message("alarmTime = ", alarmTime)

# after all you can dump your data/results and analyze it later
dump(c("inc_signals", "out_signals", "result"), file = "results.txt")

