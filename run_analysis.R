### Getting and Cleaning Data - Course Project

library(dplyr)
library(tidyr)
library(stringr)

# test group sets
test_x <- read.table("~/learnR/getdata_project/UCI_HAR_Dataset/test/X_test.txt")
test_sub <- read.table("~/learnR/getdata_project/UCI_HAR_Dataset/test/subject_test.txt")
test_y <- read.table("~/learnR/getdata_project/UCI_HAR_Dataset/test/y_test.txt")

# read training group sets
training_x <- read.table("~/learnR/getdata_project/UCI_HAR_Dataset/train/X_train.txt")
training_sub <- read.table("~/learnR/getdata_project/UCI_HAR_Dataset/train/subject_train.txt")
training_y <- read.table("~/learnR/getdata_project/UCI_HAR_Dataset/train/y_train.txt")

#create complete test set
test_sub_num <- as.numeric(test_sub$V1)
test_y_num <- as.numeric(test_y$V1)
test <- test_x
test$subject <- test_sub_num
test$activity <- test_y_num

#create complete training set
training_sub_num <- as.numeric(training_sub$V1)
training_y_num <- as.numeric(training_y$V1)
training <- training_x
training$subject <- training_sub_num
training$activity <- training_y_num

#merge training and test sets
complete_set <- rbind(training, test)

# assign column names
features <- read.table("~/learnR/getdata_project/UCI_HAR_Dataset/features.txt", stringsAsFactors = FALSE)
feat_V2 <- features$V2
names(complete_set)[1:561] <- feat_V2

#subset out columns with mean and standard deviation measurements, also retain subjects and activities columns
mean_std <- complete_set[ , grep("mean|std|subject|activity",names(complete_set))]


# Replace activities numbers with descriptive variable names
activities_names <- character()
for(i in 1:nrow(mean_std)){
        if(mean_std$activity[i] == 1) {
                activities_names[i] <- "WALKING"
        }
        if(mean_std$activity[i] == 2) {
                activities_names[i] <- "WALKING UPSTAIRS"
        }
        if(mean_std$activity[i] == 3) {
                activities_names[i] <- "WALKING DOWNSTAIRS"
        }
        if(mean_std$activity[i] == 4) {
                activities_names[i] <- "SITTING"
        }
        if(mean_std$activity[i] == 5) {
                activities_names[i] <- "STANDING"
        }
        if(mean_std$activity[i] == 6) {
                activities_names[i] <- "LAYING"
        }
}

df_3 <- mean_std
df_3$activity_name <- activities_names
df_4 <- df_3[, names(df_3) != "activity"]

# Reorganize and sort columns
df_5 <- df_4
df_5 <- arrange(df_5, subject, activity_name)
df_5_finished <- df_5[,c("subject","activity_name",names(df_5)[1:79])]
df_5_finished <- df_5_finished[, -grep("meanFreq", names(df_5_finished))]

# Descriptive column names
names(df_5_finished) <- sub("BodyBody", "Body", names(df_5_finished))

names(df_5_finished) <- sub("tBodyAcc-", "time.body_acceleration-", names(df_5_finished))
names(df_5_finished) <- sub("tGravityAcc-", "time.gravity_acceleration-", names(df_5_finished))
names(df_5_finished) <- sub("tBodyAccJerk-", "time.body_linear_acceleration-", names(df_5_finished))
names(df_5_finished) <- sub("tBodyGyro-", "time.gyroscope-", names(df_5_finished))
names(df_5_finished) <- sub("tBodyGyroJerk-", "time.body_angular_velocity-", names(df_5_finished))

names(df_5_finished) <- sub("tBodyAccMag-", "time.body_acceleration.magnitude-", names(df_5_finished))
names(df_5_finished) <- sub("tGravityAccMag-", "time.gravity_acceleration.magnitude-", names(df_5_finished))
names(df_5_finished) <- sub("tBodyAccJerkMag-", "time.body_linear_acceleration.magnitude-", names(df_5_finished))
names(df_5_finished) <- sub("tBodyGyroMag-", "time.gyroscope.magnitude-", names(df_5_finished))
names(df_5_finished) <- sub("tBodyGyroJerkMag-", "time.body_angular_velocity.magnitude-", names(df_5_finished))

names(df_5_finished) <- sub("fBodyAcc-", "frequency.body_acceleration-", names(df_5_finished))
names(df_5_finished) <- sub("fGravityAcc-", "frequency.gravity_acceleration-", names(df_5_finished))
names(df_5_finished) <- sub("fBodyAccJerk-", "frequency.body_linear_acceleration-", names(df_5_finished))
names(df_5_finished) <- sub("fBodyGyro-", "frequency.gyroscope-", names(df_5_finished))
names(df_5_finished) <- sub("fBodyGyroJerk-", "frequency.body_angular_velocity-", names(df_5_finished))

names(df_5_finished) <- sub("fBodyAccMag-", "frequency.body_acceleration.magnitude-", names(df_5_finished))
names(df_5_finished) <- sub("fGravityAccMag-", "frequency.gravity_acceleration.magnitude-", names(df_5_finished))
names(df_5_finished) <- sub("fBodyAccJerkMag-", "frequency.body_linear_acceleration.magnitude-", names(df_5_finished))
names(df_5_finished) <- sub("fBodyGyroMag-", "frequency.gyroscope.magnitude-", names(df_5_finished))
names(df_5_finished) <- sub("fBodyGyroJerkMag-", "frequency.body_angular_velocity.magnitude-", names(df_5_finished))

names(df_5_finished) <- sub("mean()", "mean", names(df_5_finished))
names(df_5_finished) <- sub("std()", "standard_deviation", names(df_5_finished))

names(df_5_finished) <- sub("X", "X_axial_direction", names(df_5_finished))
names(df_5_finished) <- sub("Y", "Y_axial_direction", names(df_5_finished))
names(df_5_finished) <- sub("Z", "Z_axial_direction", names(df_5_finished))


# Create data set with average value for each activity and each subject
pt2 <- df_5_finished %>%
        group_by(subject, activity_name) %>%
        summarise_each(funs(mean))




