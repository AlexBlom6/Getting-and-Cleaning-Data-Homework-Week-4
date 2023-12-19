#### Libraries ####
library(dplyr)
library(tidyr)
library(readr)

#### Load Data Sets ####

x_train <- read_table("./UCI HAR Dataset/train/X_train.txt", col_names = FALSE)
y_train <- read_table("./UCI HAR Dataset/train/y_train.txt", col_names = FALSE)
x_test <- read_table("./UCI HAR Dataset/test/X_test.txt", col_names = FALSE)
y_test <- read_table("./UCI HAR Dataset/test/y_test.txt", col_names = FALSE)
column_headers <- read_table("./UCI HAR Dataset/features.txt", col_names = FALSE)
subject_train <- read_table("./UCI HAR Dataset/train/subject_train.txt", col_names = FALSE)
subject_test <- read_table("./UCI HAR Dataset/test/subject_test.txt", col_names = FALSE)


#### Clean-Up / Requirement 2: Extract Relevant Columns ####
old_x_train_names <- as.vector(names(x_train))
old_x_test_names <- as.vector(names(x_test))
new_x_names <- as.vector(column_headers$X2)


## x train ##
mean_columns <- grep({"mean()"}, new_x_names, fixed = TRUE) 
std_columns <- grep("std", new_x_names)
relavent_columns <- as.vector(c(mean_columns, std_columns))


xtrain_cnamed <- x_train %>%
  rename_at(vars(old_x_train_names[relavent_columns]), ~ new_x_names[relavent_columns])

xtrain_filt <- select(xtrain_cnamed, relavent_columns)


## x test ##
xtest_cnamed <- x_test %>%
  rename_at(vars(old_x_test_names[relavent_columns]), ~ new_x_names[relavent_columns])

xtest_filt <- select(xtest_cnamed, relavent_columns)

#### Requirement 1: Merge Data ####
xtrain_merged <- xtrain_filt %>%
  cbind(y_train) %>%
  rename(Activities = X1) %>%
  cbind(subject_train) %>%
  rename(Subject = X1)

xtest_merged <- xtest_filt %>%
  cbind(y_test) %>%
  rename(Activities = X1) %>%
  cbind(subject_test) %>%
  rename(Subject = X1)

complete_data <- rbind(xtrain_merged, xtest_merged)


#### Requirement 3: Descriptive Activity Names ####
complete_data$Activities[complete_data$Activities == '1'] <- 'WALKING'
complete_data$Activities[complete_data$Activities == '2'] <- 'WALKING_UPSTAIRS'
complete_data$Activities[complete_data$Activities == '3'] <- 'WALKING_DOWNSTAIRS'
complete_data$Activities[complete_data$Activities == '4'] <- 'SITTING'
complete_data$Activities[complete_data$Activities == '5'] <- 'STANDING'
complete_data$Activities[complete_data$Activities == '6'] <- 'LAYING'


#### Requirement 4: Descriptive Variable Names ####
complete_data_cnames <- as.vector(names(complete_data))
new_data_cnames <- complete_data_cnames
new_data_cnames <- gsub("^t", "Time", complete_data_cnames)
new_data_cnames <- gsub("Gyro", "Gyroscope", new_data_cnames, fixed = TRUE)  
new_data_cnames <- gsub("Acc", "Acceleration", new_data_cnames, fixed = TRUE)
new_data_cnames <- gsub("Jerk", "JerkSignals", new_data_cnames, fixed = TRUE)
new_data_cnames <- gsub("Mag", "EuclideanNorm", new_data_cnames, fixed = TRUE)
new_data_cnames <- gsub("^f", "FrequencyDomainSignals", new_data_cnames)
new_data_cnames <- gsub("X", "xAxialDirection", new_data_cnames, fixed = TRUE)
new_data_cnames <- gsub("Y", "yAxialDirection", new_data_cnames, fixed = TRUE)
new_data_cnames <- gsub("Z", "zAxialDirection", new_data_cnames, fixed = TRUE)
new_data_cnames <- gsub("std", "StandardDeviation", new_data_cnames, fixed = TRUE)
new_data_cnames <- gsub("()", "", new_data_cnames, fixed = TRUE)
new_data_cnames <- gsub("([a-z])([A-Z])", "\\1 \\2", new_data_cnames)


names(complete_data) <- new_data_cnames


#### Requirement 5: New Tidy Data Set ####

second_tidy_set <- complete_data %>%
  group_by(Activities, Subject) %>%
  summarise_at(vars(1:66), mean) %>%
  write_csv(./"UCI HAR Dataset/Tidy Data Submission.csv")

