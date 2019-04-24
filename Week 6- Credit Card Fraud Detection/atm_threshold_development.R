library(tidyverse)
library(evir)

event_log <- read.table("ATM_train.txt", 
                       header = TRUE, 
                       sep = "|", stringsAsFactors = FALSE)

event_log <- event_log %>%
  mutate(event = str_replace(string = event, pattern = "MessageID: ", replacement = ""))

visits <- grep(x = event_log$event, pattern = "CARD_INSERT") %>%
  na.omit()

(n_visits <- length(visits))

hist_data <- data.frame(ID = character(n_visits),      # card number
                        ESD = integer(n_visits),       # ESD value
                        tPIN = numeric(n_visits),      # time to enter PIN
                        tToWdr = numeric(n_visits),    # time from amount
                      # selection to cash withdrawal
                        wrongSequence = logical(n_visits), # TRUE in case of
                      # cash withdrawal withoun PIN input
                        stringsAsFactors = FALSE)

parseVisit <- function(start,lst) {
  PINEntered = FALSE
  cardNumber = substring(lst$event[start], nchar(lst$event[start])-15)
  for (i in (start+1):nrow(lst)) {
    str = unlist(strsplit(event_log$event[i],"[ ]"))
    if (str[1] == 'ESD_SENSOR,') ESDValue = as.numeric(str[3])
    if (str[1] == 'PIN_INIT_START') t0PIN = event_log$time[i]
    if (str[1] == 'PIN_STOP') {
      PINTime = event_log$time[i] - t0PIN
      PINEntered = TRUE
    }
    if (str[1] == 'AMOUNT_SEL') t0 = event_log$time[i]
    if (str[1] == 'WDR_INIT_START') {
      if (PINEntered) fromSelToWdr = event_log$time[i] - t0
      else return(list(ID = cardNumber, ESD = 0, tPIN = 0,
                       tToWdr = 0,wrongSequence = TRUE))
    }
    if (str[1] == 'CARD_REMOVED') {
      return(list(ID = cardNumber, ESD = ESDValue, tPIN = PINTime,
                  tToWdr = fromSelToWdr,wrongSequence = FALSE))
    }
  }
}

parseVisit(visits[1],event_log)

hist_data <- map_df(1:n_visits, function(n) parseVisit(start = visits[n], event_log))

head(hist_data)

customers <- sort(hist_data$ID) %>%
  unique()

(n_customers <- length(customers))

customer_params <- data.frame(low = rep(0, n_customers), high = rep(0, n_customers),
                              min = rep(0, n_customers), max = rep(0, n_customers),
                              row.names = customers)


##set thresholds for ESD
P <- 0.999

(esd_quant <- quant(hist_data$ESD, 
                    P,
                    start = 0.05 * length(hist_data$ESD), 
                    end = 0.3 * length(hist_data$ESD)))

esd_quant[,30]

#set level at 10% exceedances
0.1 * length(hist_data$ESD)

#estimate for threshold is 1013.752
t(esd_quant) %>%
  as.data.frame() %>%
  filter(exceedances  == 40)

gpd_ESD <- gpd(hist_data$ESD, 
               nextremes = 0.1 * length(hist_data$ESD), 
               method = "ml")

riskmeasures(gpd_ESD, P)

upper_ESD <- as.numeric(riskmeasures(gpd_ESD, P)[,"quantile"])

c(upper_ESD, max(hist_data$ESD))

##set threshold for abnormal cash dispensing time
withdraw_time <- hist_data %>%
  select(tToWdr) %>%
  na.omit()

withdraw_time <- withdraw_time$tToWdr %>%
  as.numeric()

head(withdraw_time)

quant(withdraw_time, 
      P,
      start = 0.05 * length(withdraw_time), 
      end = 0.3 * length(withdraw_time))

gpd_perfc <- gpd(withdraw_time, nextremes = 0.1 * length(withdraw_time), method = "ml")

upper_perf <- as.numeric(riskmeasures(gpd_perfc, P)[,"quantile"])

c(upper_perf, max(withdraw_time))

##set abnormality for pin time
p_cust <- 0.995

ID <- rownames(customer_params)[20]

id_filter <- hist_data$ID == ID

t_pin <- hist_data$tPIN[id_filter]

length(t_pin)

(time_quant <- quant(t_pin, 
                     p_cust,
                     start = 0.2 * length(t_pin), 
                     end = 0.4 * length(t_pin)))

gpd_pin <- gpd(t_pin, nextremes = 0.3 * length(t_pin), method = "ml")

upper_pin <- riskmeasures(gpd_pin, p_cust)[,"quantile"]

c(upper_pin, max(t_pin))

quant(-t_pin, 
      p_cust,
      start = 0.2 * length(t_pin), 
      end = 0.4*length(t_pin))

gpd_pin <- gpd(-t_pin, nextremes = 0.24 * length(t_pin), method = "ml")

lower_pin <- -as.numeric(riskmeasures(gpd_pin, p_cust)[,"quantile"])

c(lower_pin, min(t_pin))

for (ID in rownames(customer_params)) {
  id_filter <- hist_data$ID == ID
  
  t_pin <- hist_data$tPIN[id_filter]
  
  print(length(hist_data$ID[hist_data$wrongSequence])) 
  
  customer_params[ID,c("min","max")] = c(min(t_pin),max(t_pin))
  gpdtPIN = gpd(t_pin, nextremes = 0.24*length(t_pin), method = "ml")
  customer_params[ID,"high"] = riskmeasures(gpdtPIN, p_cust)[,"quantile"]
  
  gpdtPIN = gpd(-t_pin, nextremes = 0.4*length(t_pin), method = "ml")
  customer_params[ID,"low"] = -riskmeasures(gpdtPIN, p_cust)[,"quantile"]
}

head(customer_params)

twenty_min <- min(t_pin)
twenty_max <- max(t_pin)

gpdtPIN = gpd(t_pin, nextremes = 0.24*length(t_pin), method = "ml")
twenty_high = riskmeasures(gpdtPIN, p_cust)[,"quantile"]

gpdtPIN = gpd(-t_pin, nextremes = 0.24*length(t_pin), method = "ml")
twenty_low = -riskmeasures(gpdtPIN, p_cust)[,"quantile"]

customer_params[20,] <- c(twenty_low, twenty_high, twenty_min, twenty_max)

globalPar <- list(ESD = upper_ESD, perf = upper_perf)

save(globalPar, file = "globalPar.rda")

save(customer_params, file = "custParams.rda")      
      
      