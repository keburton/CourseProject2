#' ---
#' title: "Getting and Cleaning Data Course Project 2"
#' author: "Kathedra Burton"
#' Date: "January 21, 2016"
#' ---


library(dplyr)

# Read the test data in
xtest <- read.table("./data/UCI/test/X_test.txt") # test readings
ytest <- read.table("./data/UCI/test/y_test.txt") # test activities
subjecttest <- read.table("./data/UCI/test/subject_test.txt") # subject labels

# Read the training data set in
xtrain <- read.table("./data/UCI/train/X_train.txt") # training readings
ytrain <- read.table("./data/UCI/train/y_train.txt") # training activities
subjecttrain <- read.table("./data/UCI/train/subject_train.txt") #subjects

# Read in features table that contains column names
features <- read.table("./data/UCI/features.txt",stringsAsFactors = FALSE) 

# Read in activity labels
activitylabels <- read.table("./data/UCI/activity_labels.txt") # 6 activity labels

# Create test data set
testdata <- cbind(subjecttest,ytest,xtest)

# Create training data set
trainingdata <- cbind(subjecttrain,ytrain,xtrain)

# Join training and test data 
data <- tbl_df(rbind(testdata,trainingdata))

# Give the data set column names
feat <- features[,2]
names(data) <- c("subject","activity_number",feat)

# Subset the data to only have mean and std dev
desired <- grepl("subject|act|mean|std", names(data))
sdata <- data[,desired]
subdata <- select(sdata,-contains("meanfreq"))

# Use descriptive activity labels
names(activitylabels) <- c("activity_number","activity")

# Add activites to data
dataset <- merge(activitylabels,subdata,by="activity_number")

# Replace current names with more descpritive ones
names(dataset) <- gsub("^t", "time", names(dataset))
names(dataset) <- gsub("^f", "frequency", names(dataset))
names(dataset) <- gsub("Gyro", "Gyroscope", names(dataset))
names(dataset) <- gsub("Acc", "Acceleration", names(dataset))
names(dataset) <- gsub("BodyBody", "Body", names(dataset))
names(dataset) <- gsub("Mag", "Magnitude", names(dataset))

# aggregate data to answer q4
tidy <- aggregate(. ~ subject + activity, dataset, mean)

# write data to csv file
tidier <- tidy[order(tidy$subject,tidy$activity_number),]
write.table(tidier, file="./data/tidydata.txt", row.names = FALSE)