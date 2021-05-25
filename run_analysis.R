library(dplyr)
library(data.table)
library(reshape2)


#load required datasets
features <- read.table("./UCI HAR Dataset/features.txt")[,2]

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")


#Process data (test)
#extract mean and std from features
extract_features <- grepl("mean|std", features)

#variables' names of X_test
names(X_test) <- features

#extract only mean and std from X_test
X_test <- X_test[,extract_features]

#make activity labels readable
activity_labels <- gsub("_","", activity_labels) 
activity_labels <- tolower(activity_labels)

#load activity labels
y_test[,2] <- activity_labels[y_test[,1]]

#variables' names of y_test
names(y_test) <- c("activity_code", "activity_labels")

#name subject_test
names(subject_test) <- "subject"

#bind data (test)
test_data <- cbind(subject_test, y_test, X_test)

#Process data (train)
#variables' names of X_train
names(X_train) <- features

#Extract only mean and std from X_train
X_train <- X_train[,extract_features]

#load activity labels
y_train[,2] <- activity_labels[y_train[,1]]

#variables' names of y_train
names(y_train) <- c("activity_code", "activity_labels")

#name subject_train
names(subject_train) <- "subject"

#bind data (train)
train_data <- cbind(subject_train, y_train, X_train)

#Merge data (test and train)
data <- rbind(test_data, train_data)

#Write a data table containing mean and std of measurements
write.table(data, file = "./mean_and_std_of_measurements.txt")

#calculating mean of each variable for each activity and each subject
for_mean <- group_by(data, activity_labels, subject)
mean_values <- for_mean %>% summarize(across( .cols = everything(), mean))

#Write a tidy data set for average of each variable for each activity and each subject
write.table(mean_values, file = "./mean_values.txt", row.names = FALSE)