#
## You should create one R script called run_analysis.R that does the following. 
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3, Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
setwd("C:/dev/Code Cleaning/project/UCI HAR Dataset")
library(dplyr)
library(data.table)



labels <-  read.fwf("./activity_labels.txt",width=c(2,100))
features <- read.table("./features.txt",sep=" ")

## Read features data ##
wd <- c(16);
wd <- rep.int(wd,561)
colTypes <- c("numeric")
colTypes <- rep.int(colTypes,561)
test_x <- read.table("./test/X_test.txt",as.is=TRUE,comment.char = "")
train_x <- read.table("./train/X_train.txt",as.is=TRUE,comment.char = "")

## combine test/train features
common <- rbind(test_x,train_x)
names(common) <- unlist(features[2])

## filter mean/std
agrep <- grep("-mean|-std",unlist(features[2]))
filtered <- common[,agrep]

## Combine activities
test_y <- read.fwf("./test/y_test.txt",width=c(1),comment.char = "",colClasses=c("numeric"))
train_y <- read.fwf("./train/y_train.txt",width=c(1),comment.char = "",colClasses=c("numeric"))
activities <- read.fwf("./activity_labels.txt",width=c(2,20))
filtered_y <- rbind(test_y,train_y)

## Combine to subjects
subject_test <- read.fwf("./test/subject_test.txt",width=c(2),comment.char = "",colClasses=c("numeric"))
subject_train <- read.fwf("./train/subject_train.txt",width=c(2),comment.char = "",colClasses=c("numeric"))
subjects <- rbind(subject_test,subject_train)
subjects$V1 <- unlist(lapply(subjects$V1,as.numeric))
subjects <- rename(subjects, Subject = V1)
filtered_y <- cbind(filtered_y,subjects)


filtered <- cbind(filtered_y,filtered)
names(filtered)[1] <- "Activity"
filtered <- mutate(filtered,Activity = activities$V2[Activity])

## Set appropriate labels tBodyAcc-mean()-X ==> mean of tBodyAcc for X
names(filtered) <- sub("([a-zA-Z]+)-([a-zA-Z]+).{2}-?([a-zA-Z]?)","\\2 of \\1 \\3",names(filtered))

## Summarize
filtered <- arrange(filtered,Subject,Activity)
grouped <- transmute (filtered, Subject.Activity=paste(Subject,Activity))
filtered_t <- select (filtered,-Activity,-Subject)
grouped <- cbind(grouped,filtered_t) %>% group_by(Subject.Activity)
res <- summarise_each(grouped,funs(mean))


