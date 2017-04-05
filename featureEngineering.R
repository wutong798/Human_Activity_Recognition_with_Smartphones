#########################
## Feature Engineering ##
#########################

library(ggplot2)
library(caret)
library(xgboost)

##check feature scaling
range <- sapply(dtTrain,range)
low_boundary <- data.frame(feature = c(1:561),value = range[1,], side = "low_boundary")
up_boundary <- data.frame(feature = c(1:561),value = range[2,], side = "up_boundary")
boundarys <- rbind(low_boundary,up_boundary)
ggplot(boundarys, aes(x=feature, y=value, col=side)) + geom_line()

normalize <- function(x){
  return(x-mean(x))/((max(x)-min(x)))
}
## check if null value exist
n1 <- sum(is.na(dtTrain))
n2 <- sum(is.na(dtTest))
n3 <- sum(is.na(dtActivityTrain))
n4 <- sum(is.na(dtActivityTest))

##correlation feature selection
highlyCorrelated <- findCorrelation(dtTrain, cutoff = 0.5)
cor_selected <- setdiff(c(1:561),highlyCorrelated)
dtTrain_cor <- dtTrain[, cor_selected, with = FALSE]
dtTest_cor <- dtTest[, cor_selected, with = FALSE]

##lasso feature selection
fit = glmnet(dtTrain, dtActivityTrain$activityNum, family = "multinomial", type.multinomial = "grouped")
cvfit=cv.glmnet(dtTrain, dtActivityTrain$activityNum, family="multinomial", type.multinomial = "grouped", parallel = TRUE)
plot(fit, xvar = "lambda", label = TRUE, type.coef = "2norm")
plot(fit, xvar = "dev", label = TRUE)
plot(cvfit)

##xgboost feature selection
dtActivityTrain_xgb <- as.integer(dtActivityTrain$activityNum)-1
dtActivityTest_xgb <- as.integer(dtActivityTest$activityNum)-1
bst <- xgboost(data = as.matrix(dtTrain), label = dtActivityTrain_xgb, max_depth = 2,eta = 1, nthread = 2, nround = 2, objective = "multi:softmax",num_class = 6)
importance_matrix <- xgb.importance(colnames(dtActivityTrain_xgb), model = bst)
xgb.plot.importance(importance_matrix, rel_to_first = TRUE, xlab = "Relative importance")
xgb_selected <- as.integer(importance_matrix$Feature)
dtTrain_xgb <- dtTrain[, xgb_selected, with = FALSE]
dtTest_xgb <- dtTest[, xgb_selected, with = FALSE]
##dimensionality reduction 
pca_train <- princomp(dtTrain)
pca_test <- princomp(dtTest)

pca_train.variance.explained = pca_train$sdev[1:20]^2 / sum(pca_model$sdev[1:20]^2)
pca_train_component20 <- data.frame(component = c(1:20), Variance = pca_train.variance.explained, Variance_Cumulation = cumsum(pca_train.variance.explained))

ggplot(data=pca_train_component20, aes(x=component)) + 
  geom_bar(aes(y=Variance,fill="Variance"), width=.7, stat="identity") +
  geom_line(aes(y=Variance_Cumulation, group=1, linetype="Cumulative Variance"))+
  geom_point(aes(y=Variance_Cumulation))+
  xlab("Components") + ylab("Varience") +
  labs(fill="",linetype="")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
##extrat top3 component for plotting 3D figure
dtTrain_pca3 <- data.frame(pca_train$scores[,1:3])
dtTest_pca3 <- data.frame(pca_test$scores[,1:3])
library(rgl)
with(pca_component_top3,{plot3d(Comp.1, Comp.2, Comp.3, col = dtActivityTrain$activityNum)})
##construct dimensionality reduction data
dtTrain_pca20 <- data.frame(pca_train$scores[,1:20])
dtTest_pca20 <- data.frame(pca_test$scores[,1:20])

##cluster
hclusterplot <- function(data, column){
  distanceMatrix <- dist(data[,column])
  myplclust(hclustering, lab.col = unclass(data$activity), main = as.character(column[length(column)]-1))
}
myplclust <- function(hclust, lab = hclust$labels, lab.col = rep(1,length(hclust$labels)), hang = 0.1, ...){
  y <- rep(hclust$height,2)
  x <- as.numeric(hclust$merge)
  y <- y[which( x < 0 )]
  x <- x[which( x < 0 )]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  plot(hclust, labels = FALSE, hang = hang, ...)
  text(x = x, y = y[hclust$order] - (max(hclust$height)*hang), labels = lab[hclust$order], col = lab.col[hclust$order], srt = 90, adj = c(1,0.5), xpd = NA, ...  )
}
kclusterplot <- {
  clusterdata <- merge(clusterdata, dtActivityNames, by = "activityNum", all.x = TRUE)
  kClust <- kmeans(clusterdata[2:4],centers = 6, nstart = 100)
  table(kClust$cluster, clusterdata$activityName)
} 
