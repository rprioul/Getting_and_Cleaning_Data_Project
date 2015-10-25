#' 1) Merges the training and the test sets to create one data set.
#' 2) Extracts only the measurements on the mean and standard deviation for each measurement. 
#' 3) Uses descriptive activity names to name the activities in the data set
#' 4) Appropriately labels the data set with descriptive variable names. 
#' 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#'

require("reshape2")
require("data.table")
require("dplyr")

# Set your working directory as appropriate
setwd("C:/Users/Rémy/Documents/Assignment/UCI HAR Dataset/")

# Load the test and train data
X_test <- read.table("./test/X_test.txt")
X_train <- read.table("./train/X_train.txt")

y_test <- read.table("./test/y_test.txt")
y_train <- read.table("./train/y_train.txt")

subject_test <- read.table("./test/subject_test.txt")
subject_train <- read.table("./train/subject_train.txt")

# Load the activity lavels and the column names and apply to data
features <- read.table("features.txt")[,2]
activity_labels <- read.table("activity_labels.txt")[,2]
colnames(X_test) <- features
colnames(X_train) <- features

# Extract the measurements on the mean and the std
wanted_features <- grepl("mean|std", features)
X_test = X_test[,wanted_features]
X_train = X_train[,wanted_features]

# Load the activity labels
y_test[,2] = activity_labels[y_test[,1]]
names(y_test) = c("Activity_ID", "Activity_Label")
names(subject_test) = "subject"
y_train[,2] = activity_labels[y_train[,1]]
names(y_train) = c("Activity_ID", "Activity_Label")
names(subject_train) = "subject"

# Bind the the test and train data
test_data <- cbind(as.data.table(subject_test), y_test, X_test)
train_data <- cbind(as.data.table(subject_train),y_train, X_train)

# Merge the test and train data to obtain a single complete data frame
Merged_data <- rbind(test_data,train_data)
id_labels <- c("subject","Activity_ID","Activity_Label")
data_labels <- setdiff(colnames(Merged_data),id_labels)
melt_down <- melt(Merged_data, id=id_labels, measure.vars=data_labels)

#create a data set with the average of each variable for each activity and each subject
tidy_data <- dcast(melt_down, subject + Activity_Label ~ variable, mean)

# Write the tidy_data.txt
write.table(tidy_data, file = "./tidy_data.txt", row.name=FALSE)