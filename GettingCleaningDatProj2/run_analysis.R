#
# Coursera  - Getting and Cleaning data
# Project 1 - Cleaning Data Set - Human Activity Recognition Using Smartphones 
# Author    - J. Szijjarto
# Date      - May 2015
#
# run_analysis.R 
#

library(data.table)
library(reshape2)
run_analysis <- function() {

  # Load the relevent dataframes
  activity_labels <- read.table(file="./UCI HAR Dataset/activity_labels.txt")
  features        <- read.table(file="./UCI HAR Dataset/features.txt")
  X_test          <- read.table(file="./UCI HAR Dataset/test/X_test.txt")
  X_train         <- read.table(file="./UCI HAR Dataset/train/X_train.txt")
  y_test          <- read.table(file="./UCI HAR Dataset/test/y_test.txt")
  y_train         <- read.table(file="./UCI HAR Dataset/train/y_train.txt")
  subject_test    <- read.table(file="./UCI HAR Dataset/test/subject_test.txt")
  subject_train   <- read.table(file="./UCI HAR Dataset/train/subject_train.txt")
  
  # Add descriptive names 
  names(X_test)         = features[,2]
  names(X_train)        = features[,2]
  names(y_test)         = "Activity"
  names(y_train)        = "Activity"
  names(subject_test)   = "Subject_ID"
  names(subject_train)  = "Subject_ID"
  
  # Convert activity ids to activity labels in the y dataframes
  y_test[,1]  = activity_labels[y_test[,1],2]
  y_train[,1] = activity_labels[y_train[,1],2]
  
  # Extract all mean and standard deviation type columns 
  X_test_mean_std  = X_test[,grep("mean|std", colnames(X_test))]
  X_train_mean_std = X_train[,grep("mean|std", colnames(X_train))]
  
  # Merge dataframes by column
  test_data_mean_std  <- cbind(subject_test, y_test, X_test_mean_std)
  train_data_mean_std <- cbind(subject_train, y_train, X_train_mean_std)
  # Merge test and train dataframes by row
  test_train_data_mean_std = rbind(test_data_mean_std, train_data_mean_std)
  
  # Create tidy_data set for avg of each var for each activity & each subject
  tidy_data <- test_train_data_mean_std
  tidy_data <- transform(tidy_data, Subject_ID=factor(Subject_ID))
  tidy_data <- melt(tidy_data, id=c("Subject_ID","Activity"))
  tidy_data <- dcast(tidy_data, Subject_ID + Activity ~ variable, mean)
  
  write.table(tidy_data, file="./tidy_data.txt", row.name=FALSE)  
  
}