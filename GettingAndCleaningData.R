################################
## Getting and Cleanning data ##
################################

library("RCurl")#used for downloading data
library("data.table")#used for read txt data

#Download and unzip the data package
setwd("./GitHub/Human_Activity_Recognition_with_Smartphones")
url<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
path <- as.character(getwd())
f <- "Dataset.zip"
path_data <- file.path(path, "UCI HAR Dataset")
if(!file.exists(path)){
  dir.create(path)
}
if(!file.exists(f)){
  download.file(url, file.path(path, f))
}
executable  <- file.path("C:","Program Files", "7-Zip", "7z.exe")
parameters <- "x"
cmd <- paste(paste0("\"", executable, "\""), parameters, paste0("\"", file.path(path, f), "\""))
system(cmd)
if(!file.exists(path_data)){
  dir.create(path_data)
}


#read data from files##
#######################
dtSubjectTrain <- fread(file.path(path_data, "train", "subject_train.txt"))
dtSubjectTest <- fread(file.path(path_data, "test", "subject_test.txt"))
dtActivityTrain <- fread(file.path(path_data, "train", "Y_train.txt"))
dtActivityTest <- fread(file.path(path_data, "test", "Y_test.txt"))
#The input data(x data) in format "ANSI",can't read by "fread"
#Use data table to read the file
fileToDataTable <- function(f) {
  df <- read.table(f)
  dt <- data.table(df)
}
dtTrain <- fileToDataTable(file.path(path_data, "train", "X_train.txt"))
dtTest <- fileToDataTable(file.path(path_data, "test", "X_test.txt"))



##merge data for plotting##
###########################
#combine data frames
setnames(dtSubjectTrain, "V1", "subject")
setnames(dtSubjectTest, "V1", "subject")
setnames(dtActivityTrain, "V1", "activityNum")
setnames(dtActivityTest, "V1", "activityNum")

dtplot_train <- cbind(dtSubjectTrain, dtTrain, dtActivityTrain)
dtplot_test <- cbind(dtSubjectTest, dtTest, dtActivityTest)
dtplot <- rbind(dtplot_train, dtplot_test)
setkey(dtplot_Train, subject, activityNum)
setkey(dtplot_Test, subject, activityNum)



#read activity and feature Name
dtActivityNames <- fread(file.path(path_data, "activity_labels.txt"))
dtFeatures <- fread(file.path(path_data, "features.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]



#Label the data 
dtplot <- merge(dtplot, dtActivityNames, by = "activityNum", all.x = TRUE)
dtplot <- data.table(melt(dtplot, key(dt), variable.name = "featureCode"))
dtplot <- merge(dtplot, dtFeatures[, list(featureNum, featureCode, featureName)], by = "featureCode", all.x = TRUE)

dtplot$activity <- factor(dtplot$activityName)
dtplot$feature <- factor(dtplot$featureName)

#see the dtplot
head(dtplot)

##featureCode activityName     value featureNum       featureName activity           feature
##1:          V1      WALKING 0.2820216          1 tBodyAcc-mean()-X  WALKING tBodyAcc-mean()-X
##2:          V1      WALKING 0.2558408          1 tBodyAcc-mean()-X  WALKING tBodyAcc-mean()-X
##3:          V1      WALKING 0.2548672          1 tBodyAcc-mean()-X  WALKING tBodyAcc-mean()-X
##4:          V1      WALKING 0.3433705          1 tBodyAcc-mean()-X  WALKING tBodyAcc-mean()-X
##5:          V1      WALKING 0.2762397          1 tBodyAcc-mean()-X  WALKING tBodyAcc-mean()-X
##6:          V1      WALKING 0.2554682          1 tBodyAcc-mean()-X  WALKING tBodyAcc-mean()-X
