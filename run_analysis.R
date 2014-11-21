# Project assignement Module 3
# PdN 20/11/2014

# Steps to be performed
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Note (from forum) : the codebook to provide is related to the data set produced in step 5.

library(dplyr)
#library(qdap)

# Reading dataset from files

path_names = c("getdata-projectfiles-UCI HAR Dataset//UCI HAR Dataset//test",
               "getdata-projectfiles-UCI HAR Dataset//UCI HAR Dataset//train",
               "getdata-projectfiles-UCI HAR Dataset//UCI HAR Dataset")

wdtemp <- getwd()
setwd(path_names[1])
# "test" dataset
# If I gather the list from the dir, then I can use it... (TODO : check on StackOverflow)
X_test <- read.csv("X_test.txt", header=FALSE, sep= "")
y_test <- read.csv("y_test.txt", header=FALSE, sep= "")
subject_test <- read.csv("subject_test.txt", header=FALSE, sep= "")

# Back to were I was
setwd(wdtemp)

setwd(path_names[2])
# "train" dataset
# If I gather the list from the dir, then I can use it... (TODO : check on StackOverflow)
X_train <- read.csv("X_train.txt", header=FALSE, sep= "")
y_train <- read.csv("y_train.txt", header=FALSE, sep= "")
subject_train <- read.csv("subject_train.txt", header=FALSE, sep= "")

# Back to were I was
setwd(wdtemp)

# Reading common files
setwd(path_names[3])

features <- read.csv("features.txt", header=FALSE, sep= "")
activity_labels <- read.csv("activity_labels.txt", header=FALSE, sep= "")

# 1.
# Two steps process
# a. Merging the columns:
BigTest <- cbind(X_test, y_test, subject_test)
BigTrain <- cbind(X_train, y_train, subject_train)
# b. merging all rows
BigAll <- rbind(BigTest,BigTrain)

# 4.
# Makes names legal AND eleminate duplications (although not relevant to the assigned task
# i.e. the data set we have to provide)
features$V2 <- make.names(features$V2, unique = TRUE)
# Add labels (the "not clean" ones will go only mean and std are extracted...)
names(BigAll) <- c(features$V2, "Activity", "Subject")

# 2.
# Two steps (couldn't find the way to combine into one instruction)
aa <- select(BigAll, contains("mean"))
bb <- select(BigAll, contains("std"))
TheDataSet <- cbind(aa,bb)

# 3.
# I apply the change in the big merged data set
BigAll$Activity <- lookup(BigAll$Activity, activity_labels)


# 5.
# I create a dataset to support task 5.
DataSet5 <- cbind(TheDataSet, Activity=BigAll$Activity, Subject=BigAll$Subject)
# Note: if I just do this:
#DataSet5 <- cbind(TheDataSet, BigAll$Activity, BigAll$Subject)
# the two added variables will keep the names BigAll$Activity and BigAll$Subject

DataSet5Grouped <- group_by(DataSet5, Subject, Activity)
# I have to summarize all variables but the grouping variables (that I cannot modify by this command)
FinalDataSet <- summarise_each(DataSet5Grouped, funs(mean), -matches("Activity"), -matches("Subject"))

# Final cleaning
names(FinalDataSet) <- gsub("bodybody", "Body", as.character(names(FinalDataSet)), ignore.case=TRUE)

# Back to were I was
setwd(wdtemp)
write.table(FinalDataSet, "module3dataset.txt", row.names=FALSE)
