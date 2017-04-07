#########################
##     Classifiers     ##
#########################


library(rpart)#decision tree
library(glmnet)
library(randomForest)
library(e1071)
library(class)

#construct input and output data
dtTrain_dtActivityTrain <- data.frame(dtTrain, dtActivityTrain) 
dtTrain_dtActivityTrain_cor <- data.frame(dtTrain_cor, dtActivityTrain)
dtTrain_dtActivityTrain_xgb <- data.frame(dtTrain_xgb, dtActivityTrain) 
dtTrain_dtActivityTrain_pca80 <- data.frame(dtTrain_pca80, dtActivityTrain) 


##Decision Tree##
#################

decisionTreeModel <- function(xy_train, x_test, y_test){
  bodyfat_rpart <- rpart(activityNum~., data = xy_train, control = rpart.control(minsplit = 100))
  opt <- which.min(bodyfat_rpart$cptable[,"xerror"])
  cp <- bodyfat_rpart$cptable[opt, "CP"]
  bodyfat_prune <- prune(bodyfat_rpart, cp = cp)
  DEXfat_pred <- predict(bodyfat_prune, newdata = x_test)
  accuracy <- table(round(DEXfat_pred) == y_test$activityNum)
  return(list(runningTime, accuracy))
}

#1. data with full features 
system.time(print(result1 <- decisionTreeModel(dtTrain_dtActivityTrain, dtTest, dtActivityTest)))
#2. data with correlation features
system.time(result2 <- decisionTreeModel(dtTrain_dtActivityTrain_cor, dtTest_cor, dtActivityTest))
#3. data with xgboost features
system.time(result3 <- decisionTreeModel(dtTrain_dtActivityTrain_xgb, dtTest_xgb, dtActivityTest))
#4. data with pca80 features
system.time(result4 <- decisionTreeModel(dtTrain_dtActivityTrain_pca80, dtTest_pca80, dtActivityTest))



##    Lasso    ##
#################

lassoModel <- function(x_train, y_train, x_test, y_test){
  fit <- glmnet(as.matrix(x_train), as.matrix(y_train), family = "multinomial", type.multinomial = "grouped")
  cvfit <- cv.glmnet(as.matrix(x_train), as.matrix(y_train), family="multinomial", type.multinomial = "grouped", parallel = TRUE)
  Lasso_pred <- predict(cvfit,s = 0, newx = as.matrix(x_test), type = "class")
  class(Lasso_pred)
  accuracy <- table(Lasso_pred == y_test$activityNum)
  return(list(runningTime, accuracy))
}
#1. data with full features 
system.time(print(result1 <- lassoModel(dtTrain,dtActivityTrain, dtTest, dtActivityTest)))
#2. data with correlation features
system.time(result2 <- lassoModel(dtTrain_cor, dtActivityTrain, dtTest_cor, dtActivityTest))
#3. data with xgboost features
system.time(result3 <- lassoModel(dtTrain_xgb, dtActivityTrain, dtTest_xgb, dtActivityTest))
#4. data with pca80 features
system.time(result4 <- lassoModel(dtTrain_pca80, dtActivityTrain, dtTest_pca80, dtActivityTest))



## Random Forest ##
###################
randomForestModel <- function(xy_train, x_test, y_test){
  model <- randomForest(activityNum~., data = xy_train)
  RF_pred <- predict(model, newdata = x_test)
  accuracy <- table(round(RF_pred) == y_test$activityNum)
  return(accuracy)
}
#1. data with full features 
system.time(result1 <- randomForestModel(dtTrain_dtActivityTrain, dtTest, dtActivityTest))
#2. data with correlation features
system.time(result2 <- randomForestModel(dtTrain_dtActivityTrain_cor, dtTest_cor, dtActivityTest))
#3. data with xgboost features
system.time(result3 <- randomForestModel(dtTrain_dtActivityTrain_xgb, dtTest_xgb, dtActivityTest))
#4. data with pca80 features
system.time(result4 <- randomForestModel(dtTrain_dtActivityTrain_pca80, dtTest_pca80, dtActivityTest))



##    XGBoost    ##
###################
XgboostModel <- function(x_train, y_train, x_test, y_test){
  y <- as.integer(y_train$activityNum)-1
  bst <- xgboost(data = as.matrix(x_train), label = y, max_depth = 3,eta = 0.32, nthread = 6, nround = 8 , objective = "multi:softmax",num_class = 6)
  xgb_pred <- predict(bst, as.matrix(x_test))
  accuracy <- table(round(xgb_pred) == (y_test$activityNum - 1))
  return(accuracy)
}
#1. data with full features 
system.time(result1 <- XgboostModel(dtTrain, dtActivityTrain, dtTest, dtActivityTest))
#2. data with correlation features
system.time(result2 <- XgboostModel(dtTrain_cor, dtActivityTrain, dtTest, dtActivityTest))
#3. data with xgboost features
system.time(result3 <- XgboostModel(dtTrain_xgb, dtActivityTrain, dtTest_xgb, dtActivityTest))
#4. data with pca80 features
system.time(result4 <- XgboostModel(dtTrain_pca80, dtActivityTrain, dtTest_pca80, dtActivityTest))



##      SVM      ##
###################
SVMModel <- function(xy_train, x_test, y_test){
  svm_model <- svm(activityNum~., data = xy_train)
  pred_svm <- predict(svm_model, x_test)
  accuracy <- table(round(pred_svm) == y_test$activityNum)
  return(accuracy)
}
#1. data with full features 
system.time(result1 <- SVMModel(dtTrain_dtActivityTrain, dtTest, dtActivityTest))
#2. data with correlation features
system.time(result2 <- SVMModel(dtTrain_dtActivityTrain_cor, dtTest, dtActivityTest))
#3. data with xgboost features
system.time(result3 <- SVMModel(dtTrain_dtActivityTrain_xgb, dtTest_xgb, dtActivityTest))
#4. data with pca80 features
system.time(result4 <- SVMModel(dtTrain_dtActivityTrain_pca80, dtTest_pca80, dtActivityTest))



##      KNN      ##
###################
KNNModel <- function(x_train, y_train, x_test, y_test){
  pred_knn <- knn(train = x_train, test = x_test, cl = y_train$activityNum, k=20)
  accuracy <- table(pred_knn == y_test$activityNum)
  return(accuracy)
}
#1. data with full features 
system.time(result1 <- KNNModel(dtTrain, dtActivityTrain, dtTest, dtActivityTest))
#2. data with correlation features
system.time(result2 <- KNNModel(dtTrain_cor, dtActivityTrain, dtTest_cor, dtActivityTest))
#3. data with xgboost features
system.time(result3 <- KNNModel(dtTrain_xgb, dtActivityTrain, dtTest_xgb, dtActivityTest))
#4. data with pca80 features
system.time(result4 <- KNNModel(dtTrain_pca80, dtActivityTrain, dtTest_pca80, dtActivityTest))

