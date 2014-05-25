## FILE: run_analysis.R
## R language file with a script for processing the datat set
## given in https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## as part of the project for the Coursera course Getting and cleaning data
## taught on-line by professor Jeff Leek  

## ASSUMPTIONS: The source file exists in current working directory which is the 
## same directory where the script file runs. The data files are under the
## subdirectory "UCI HAR Dataset"

## First TASK: Merge the training and the test sets to create one data set

## The target text files to read are:
## 'train/X_train.txt': Training set
## 'test/X_test.txt': Test set 

