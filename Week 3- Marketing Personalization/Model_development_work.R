library(tidyverse)
library(rpart)
library(rattle)
library(caret)
library(randomForest)
library(dummies)

customers <- read.csv("DTTrain.csv", stringsAsFactors = F) %>%
  select(-time) %>%
  mutate(target = as.factor(target))

formula <- formula(paste("target", ".", sep = "~"))

set.seed(1017)
data_split <- createDataPartition(y = customers$target, p = .8, list = F)

training <- customers %>%
  slice(data_split)

testing <- customers %>%
  slice(-data_split)

set.seed(1017)
small_tree <- rpart(formula = formula, 
                    data = training, 
                    control = rpart.control(minbucket = 200, cp = 0.0001))

fancyRpartPlot(small_tree)

MultiLogLoss <- function(act, pred)
{
  eps = 1e-15;
  nr <- nrow(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  #normalize rows
  ll = sum(act*log(sweep(pred, 1, rowSums(pred), FUN="/")))
  ll = -ll/nrow(act)
  return(ll);
}

tree_probs <- predict(small_tree, testing, type = "prob") %>%
  as.data.frame() %>%
  mutate_all(function(x) ifelse(x == 0, 0.0001, x))

target_test <- dummy.data.frame(data=as.data.frame(testing$target), 
                                sep="_", verbose=F, 
                                dummy.class="ALL")

MultiLogLoss(target_test, as.matrix(tree_probs))

#rf
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid")

forest_train <- randomForest(target ~., data = training, 
                             ntree = 300,
                             mtry = 35, 
                             importance = T, 
                             proximity = T)

varImpPlot(forest_train)

forest_probs <- predict(forest_train, testing, type = "prob") %>%
  as.data.frame() %>%
  mutate_all(function(x) ifelse(x == 0, 0.0001, x)) 

#0.3426466

MultiLogLoss(target_test, as.matrix(forest_probs))

#knn

knn_train <- knn3(target ~. ,data = training, k = 20)

knn_probs <- predict(knn_train, testing, type="prob") %>%
  as.data.frame() %>%
  mutate_all(function(x) ifelse(x == 0, 0.0001, x)) 

MultiLogLoss(target_test, as.matrix(knn_probs))


#ensemble

Q <- 0.5

MultiLogLoss(target_test,Q * as.matrix(forest_probs) +(1-Q)*as.matrix(knn_probs))

save(forest_train, file =  "RealTime_Forest.rda")

save(knn_train, file = "RealTime_knn.rda")
