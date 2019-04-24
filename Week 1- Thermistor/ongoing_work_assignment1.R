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
  
  grouping_size <- 450
  
  stability_threshold <- .25
  
  stable_rle_threshold <- 66
  unstable_rle_threshold <- 28
  
  if(temp_events_counter > grouping_size && temp_events_counter <= BUF_SIZE){
    temp_events$time[temp_events_counter] <<- now
    temp_events$temp[temp_events_counter] <<- rcvd_temp
    temp_events$rolling_diff <<- c(0, diff(temp_events$temp))
    temp_events$diff_sum[temp_events_counter] <<- abs(sum(temp_events$rolling_diff[(temp_events_counter - grouping_size):temp_events_counter]))
    temp_events$is_stable[temp_events_counter] <<- ifelse(temp_events$diff_sum[temp_events_counter] < stability_threshold, 1, 0)
    temp_events$rolling_rle[temp_events_counter] <<- sequence(rle(temp_events$is_stable)$lengths)[temp_events_counter]
    
    signals_reference <- signals
    signals_reference <- signals_reference[!is.na(signals_reference$time),]
    
    if (temp_events$is_stable[temp_events_counter] == 1 &&  temp_events$rolling_rle[temp_events_counter] == 1) {
      if (temp_events$is_stable[temp_events_counter] == 1 & tail(signals_reference$time, 1) == TRUE){
        send_signal <- FALSE
      } else if (temp_events$is_stable[temp_events_counter] == 1 & 
                 tail(signals_reference$time, 1) == FALSE | tail(signals_reference$time, 1) == "POSIXct of length 0"){
        message(now, "PROCESS IS STABLE NOW")
        send_signal <- TRUE
        is_stable <- TRUE
        stable_temp <- rcvd_temp
      }
      
    } else if (temp_events$is_stable[temp_events_counter] == 0 && temp_events$rolling_rle[temp_events_counter] == 1) {
      if (temp_events$is_stable[temp_events_counter] == 1 & tail(signals$time, 1) == TRUE){
        send_signal <- FALSE
      } else if (temp_events$is_stable[temp_events_counter] == 1 & tail(signals$time, 1) == FALSE){
        message(now, "PROCESS IS STABLE NOW")
        send_signal <- TRUE
        is_stable <- TRUE
        stable_temp <- rcvd_temp
      }
    }
    
    if (send_signal) {
      # update signals dataframe (append last value):
      signals_counter <<- signals_counter + 1
      signals[signals_counter,] <<- list(now, is_stable, stable_temp)
      
      signals_reference[signals_counter,] <<- list(now, is_stable, stable_temp)
    }
  }
}
  
  

  
  
  