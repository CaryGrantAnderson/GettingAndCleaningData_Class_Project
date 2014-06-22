####################################################################################
## Getting and Cleaning Data - Course Project
## Coursera.com Class
## Cary Grant Anderson
## 6/22/2014
#
# File Name:  run_Analysis.R
#
# Description: Tidy up the raw data from the UCI HAR data set.
#
# Data Source:  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#
# Author:  Cary Grant Anderson
#
# Date:  6/22/2014
#
# Notes: 
#         1. For the Class Project of the Coursera course "Getting and Cleaning Data".
#         2. Data table names are lower case.
#         3. Column names are mixed case.
#         4. All measurements with "-mean()", "-meanFreq()", and "-std()" in their names are included.
#
#
# Script Functions and Requirements:
# ---------------------------------
#
# 1. Merge the training and test sets into one data set.
#
# 2. Extract the mean and standard deviation for each measurement.
#
# 3. Creat a second independent tidy data set with the average of each variable
#    for each activity and each subject.
#
# Script Requirements:
# --------------------
#
# 1. Use descriptive activity names to name the activities in the data set.
#
# 2. Appropriately label the data set with descriptive variable names.
#
#####################################################################################

################################################################################
# Step 1. Setup
################################################################################

# 1.1 Clear up the Global Environment.

rm(list = ls())

# 1.2 Set working directory to the raw data source file.

setwd('C:\\Grant\\Google Drive\\Courses - Coursera Data Science Track\\Getting and Cleaning Data\\Class Project\\UCI HAR Dataset');

# 1.3 Install required packages if necesary.

if (!require("data.table")) 
{
  install.packages("data.table")
}

if (!require("reshape2")) 
{
  install.packages("reshape2")
}

require("data.table")
require("reshape2")


################################################################################
# Step 2. Load the data from the raw data files.
################################################################################

# 2.1 Load the Features and Activity data.

features = read.table('.//features.txt', header=FALSE); 
activity_labels = read.table('.//activity_labels.txt', header=FALSE); 


# 2.1 Assign column names to the Features and Activity data.

colnames(features) = c('Feature_ID', 'Feature_Name');
colnames(activity_labels) = c('Activity_ID', 'Activity_Name');


# 2.3 Load the Test data.

subject_test = read.table('.//test//subject_test.txt', header=FALSE); 
x_test = read.table('.//test//x_test.txt', header=FALSE); 
y_test = read.table('.//test//y_test.txt', header=FALSE); 


# 2.4 Assign column names to the Test data.

colnames(subject_test) = "Subject_ID";
colnames(x_test) = features[ , 2];
colnames(y_test) = "Activity_ID";


# 2.5 Load the Train data.

subject_train = read.table('.//train//subject_train.txt', header=FALSE); 
x_train = read.table('.//train//x_train.txt', header=FALSE); 
y_train = read.table('.//train//y_train.txt', header=FALSE); 


# 2.6 Assign column names to the Train data.

colnames(subject_train) = "Subject_ID";
colnames(x_train) = features[ , 2];
colnames(y_train) = "Activity_ID";


################################################################################
# Step 3. Merge the training and test sets into one data set.
################################################################################

# 3.1 Merge the test data.
test_data_set = cbind(y_test, subject_test, x_test);

# 3.2 Merge the training data.
training_data_set = cbind(y_train, subject_train, x_train);

# 3.3 Merge the training and test data.
final_data_set = rbind(training_data_set, test_data_set);


################################################################################
# Step 4. Extract the mean and standard deviation for each measurement.
################################################################################

# 4.1 Create a vector.
column_Names_Vector = colnames(final_data_set);

# 4.2 Create a logical Vector with TRUE values for the ID, mean(), and stddev() columns and FALSE for all the others.
logical_Vector = (grepl("Activity_ID", column_Names_Vector) 
                  | grepl("Subject_ID", column_Names_Vector) 
                  | grepl("-mean()", column_Names_Vector) 
                  | grepl("-std()", column_Names_Vector));

# 4.3 Subset for only the desired output columns.
final_data_set = final_data_set[logical_Vector == TRUE];


################################################################################
# Step 5. Create a second independent tidy data set with the average of each 
#         variable for each activity and each subject.
################################################################################

# 5.1 Add the Activity Labels so as to get descriptive activity names.
final_data_set = merge(final_data_set, activity_labels, by='Activity_ID', all.x = TRUE);

# 5.2 Update the new column names.
column_Names_Vector  = colnames(final_data_set); 

# 5.3 Update the nicer column names in the final data set.
colnames(final_data_set) = column_Names_Vector;

# 5.4 Create a new intermediate table without Activity Names for now.
final_data_set_without_activity_names = final_data_set[, names(final_data_set) != 'Activity_Name'];

# 5.5 Create the final tidy data set.
tidy_data_set = aggregate(final_data_set_without_activity_names[, names(final_data_set_without_activity_names) != c('Activity_ID', 'Subject_ID')], by = list(Activity_ID = final_data_set_without_activity_names$Activity_ID, Subject_ID = final_data_set_without_activity_names$Subject_ID), mean);

# 5.6 Merge the final tidy data set with the Activity Names.
tidy_data_set = merge(tidy_data_set, activity_labels, by='Activity_ID', all.x = TRUE);


################################################################################
# Step 6.  Write out the new tidy data set.
################################################################################

write.table(tidy_data_set, './Tidy_Data.txt', row.names = TRUE, sep='\t');


