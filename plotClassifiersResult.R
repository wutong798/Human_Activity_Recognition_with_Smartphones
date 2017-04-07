################################
##  Classifiers Result Plot   ##
################################

library(tidyr)

#construct data for plotting
accuracy_raw <- data.frame(c("All_Feature", "Cor_Selected_Features", "XGB_Selected_Features", "PCA_Components"), c(0.83, 0.421, 0.701, 0.621), c(0.9494, 0.893, 0.893, 0.941),c(0.9, 0.728, 0.891, 0.857), c(0.906, 0.817, 0.884, 0.843),c(0.897, 0.711, 0.863, 0.755), c(0.906, 0.834, 0.892, 0.899))
time_raw <- data.frame(c("All_Feature", "Cor_Selected_Features", "XGB_Selected_Features", "PCA_Components"), c(3.7, 0.5, 0.25, 0.19), c(4446, 1838, 694, 1976),c(706, 156, 125, 122), c(17.1, 3.79, 3.55, 2.78),c(130, 78.8, 28.4, 66), c(33, 4.3, 4.9, 3.4))

setnames(accuracy_raw, names(accuracy_raw), c("Selected_Features", "Decision_Tree", "Lasso", "Random_Forest", "XGBoost", "SVM", "kNN"))
setnames(time_raw, names(time_raw), c("Selected_Features", "Decision_Tree", "Lasso", "Random_Forest", "XGBoost", "SVM", "kNN"))

accuracy_long <- accuracy_raw %>% gather(model, accuracy, -Selected_Features)
time_long <- time_raw %>% gather(model, time, -Selected_Features)

accuracy_time <- data.frame(accuracy_long, time = time_long$time)

ggplot( data = accuracy_time, aes(x = model)) + 
  geom_bar(aes(y=log(time + 1),fill=Selected_Features), stat="identity",position = "dodge") 

ggplot( data = accuracy_time, aes(x = model, y=accuracy,group = Selected_Features, colour = Selected_Features)) + geom_line(lwd=2)

ggplot( data = accuracy_time, aes(x = model)) + 
  geom_bar(aes(y=accuracy,fill=Selected_Features), stat="identity") +
  geom_line(aes(y=Variance_Cumulation, group=1, linetype="Cumulative Variance"))+
  geom_point(aes(y=Variance_Cumulation))+
  xlab("Components") + ylab("Varience") +
  labs(fill="",linetype="")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))