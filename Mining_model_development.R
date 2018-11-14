library(tidyverse)
library(nloptr)
library(caret)
library(randomForest)

mining <- read_csv("MiningTraining2016.csv", col_names = c("time", "collapse")) %>%
  mutate(time = time / 60000000,
         n_events = seq(1, n(), 1))

theme_set(
  theme_minimal()
)

mining %>%
  ggplot(aes(time, n_events)) +
  geom_line(size = 1.4, colour = "dodgerblue2") +
  ggtitle("Seismic acitivity events over time")

#parameter estimation
dt <- 0.05

w <- 10 # experiment with window width to estimate intensity

n <- 50 # experiment with number of observations of intensity in the model

t0 <- ((n + w) * dt) # earliest time when regression can be fitted

i0 <- (findInterval(t0, mining$time) + 1) # earliest event number 

current_time <- mining$time[i0]

t_grid <- seq(current_time - t0 + dt, current_time, by = dt) # grid at t0

head(t_grid)

events_grid <- findInterval(t_grid, mining$time)

events_grid

N <- length(t_grid)

intensity <- events_grid[(w+1):N] - events_grid[1:(N-w)]

intensity <- intensity / (dt*w) # events per minute

time_grid <- t_grid[(N-n+1):N] # timeGrid contains t_i, i=1,...n

# Use pmax(x, 0.1) to avoid log(0)
log_intensity <- log(pmax(intensity, 0.1))

regression <- function(tc, logRate, regressionTimes, returnError){
  # tc - time of collapse
  # logRate - logarithm of intensity calculated at times t_i, i= 1,...,n
  # regressionTimes - sequence (tc-t_i), i= 1,...,n
  # returnError - logical flag, TRUE if function is used in a minimization procedure; then it returns mean-squared residual
  #               if FALSE return coefficient p, the slope, of the model    
  
  logregressionTimes <- log(tc-regressionTimes)
  linModel <- lm(logRate ~ logregressionTimes)
  if(returnError){ #if we use this function in a minimization procedure
    err <- sqrt(mean(linModel$residuals^2))
    res <- err    
  }
  else{ #we will need the p coefficient after minimization
    p <- linModel$coefficients[2]
    names(p) <- NULL
    res <- p
  }
  return(res)
}

res <- nloptr(x0= current_time+1, 
              eval_f=regression, 
              lb=current_time+0.1, 
              ub=current_time+10, 
              opts=list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel" = 1e-04),
              logRate=log_intensity,
              regressionTimes=time_grid, returnError=TRUE)

tc <- res$solution

p <- regression(res$solution, log_intensity, time_grid, FALSE)

tc_collect <- function(record){
  current_time <- mining$time[record]
  
  t_grid <- seq(current_time - t0 + dt, current_time, by = dt) # grid at t0
  
  events_grid <- findInterval(t_grid, mining$time)
  
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
  
  params <- list(tc = tc, p = p, intensity = tail(intensity, 1))
  
  return(params)
}

results_table <- map_df(25:nrow(mining), function(x) tc_collect(record = x)) %>%
  mutate(current_time = mining$time[25:nrow(mining)],
         time_to_shock = tc - current_time)

results_table <- results_table %>%
  mutate(alert_time = c(map(103:1, function(x) max(results_table$current_time) - 
                            results_table$current_time[nrow(results_table) - x]) %>% 
           as.numeric(), 0),
         collapse = ifelse(alert_time < 2, 1, 0)) %>%
  select(current_time, p, intensity, time_to_shock, collapse)

collapse_glm <- glm(collapse ~., data = results_table)

glm_pred <- ifelse(collapse_glm$fitted.values > .4, 1, 0)

#88% accuracy
confusionMatrix(glm_pred, results_table$collapse)

set.seed(1017)
collapse_forest <- randomForest(as.factor(collapse) ~., data = results_table)

forest_pred <- predict(collapse_forest, type = "response")

#94% accuracy
confusionMatrix(forest_pred, results_table$collapse, positive = "1")

mining %>%
  slice(25:128) %>%
  mutate(fores_pred = forest_pred) %>%
  ggplot(aes(time, n_events, colour = forest_pred)) +
  geom_point() +
  ggtitle("Send message after 3-5 positive signals in a row to avoid false alarm")
