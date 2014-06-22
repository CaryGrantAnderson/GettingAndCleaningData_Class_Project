## Course Project - Getting and Cleaning Data

A Coursera Course.
Cary Grant Anderson
6/22/2014

This is the course project files for the Getting and Cleaning Data course by Coursera.

It consists of:

	run_Analysis.R - An R script to tidy a raw data file.
	This README.MD file.
	CodeBook.MD which contains the code book information for this project

	
# Source Data

The raw source data for this project came from the Human Activity Recognition Using Smartphone data set available at:

	https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

	
# Running the Script

To run the run_Analysis.R script load it into RStudio, set your working directory to the directory of the raw source data file that you've downloaded, and run the script.


## Data Transformation Steps

The run_Analysis.R script performs the following data transformations:

	1. Merges the training and the test sets to create one data set.
	2. Extracts only the measurements on the mean and standard deviation for each measurement. 
	3. Uses descriptive activity names to name the activities in the data set
	4. Appropriately labels the data set with descriptive variable names. 
	5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

	
# Output Data

The output of the run_Analysis.R script is place in the working directory and is a file named "Tidy_Data.txt".


