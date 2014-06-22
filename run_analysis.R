## FILE: run_analysis.R
## R language file with a script for processing the datat set
## given in https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## as part of the project for the Coursera course Getting and cleaning data
## taught on-line by professor Jeff Leek  

## ASSUMPTIONS: The source file exists in current working directory which is the 
## same directory where the script file runs. The data files are under the
## subdirectory "UCI HAR Dataset"

## utility function to clean names
Cleannames <- function( listofnames ) {
  ## remove all underscores '_', '\\.' (escape the \ so that it scapes the '.' as well),
  ## ',', '\\(', '\\)', and convert all characters to lower case
  names <- gsub( pattern="_|\\.|-|\\(|\\)|,",replacement="",x=listofnames) 
  names <- tolower( names ) # standardize to lower case
  names # return the original list or vector
}

  
## First TASK: Merge the training and the test sets to create one data set

## Read all files in data set directory, except the ones that don't have data but context information:
files_in_UCI_HAR_data_set<-dir("./", ".*[.]txt",recursive=T) # assumes local directory has data
filter_TXT_files_without_data <- function(x) !(x %in% c("UCI HAR Dataset/features_info.txt","UCI HAR Dataset/README.txt"))
files_in_UCI_HAR_data_set_with_data<-Filter(f=filter_TXT_files_without_data,x=files_in_UCI_HAR_data_set)
list_data_frames_from_UCI_HAR_data_set <- sapply(files_in_UCI_HAR_data_set_with_data, function(x) read.table(paste0(".//",x)))

## the names of this list of data frames looks as follows:

## > names(list_data_frames_from_UCI_HAR_data_set)
## [1] "UCI HAR Dataset/activity_labels.txt"                          "UCI HAR Dataset/features.txt"                                
## [3] "UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt"    "UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt"   
## ...                                                                ...
## [25] "UCI HAR Dataset/train/X_train.txt"                            "UCI HAR Dataset/train/y_train.txt"  

## Extract only the file names and assign them to the list:

names <- sub( pattern=".*/", # any number of any charcater followed by '/' it is greedy so it matches the longest string
              replacement="", # effectively eliminate everything up to that match...of the last '/'!
              x=names(list_data_frames_from_UCI_HAR_data_set)) # in the list of names shown above
names <- sub( pattern=".txt", replacement="",x=names )
names <- Cleannames(names)
names(list_data_frames_from_UCI_HAR_data_set)<-names

## data frame names look like this:

# [1] "activitylabels" "features"       "bodyaccxtest"   "bodyaccytest"   "bodyaccztest"   "bodygyroxtest"  "bodygyroytest"  "bodygyroztest" 
# [9] "totalaccxtest"  "totalaccytest"  "totalaccztest"  "subjecttest"    "xtest"          "ytest"          "bodyaccxtrain"  "bodyaccytrain" 
# [17] "bodyaccztrain"  "bodygyroxtrain" "bodygyroytrain" "bodygyroztrain" "totalaccxtrain" "totalaccytrain" "totalaccztrain" "subjecttrain"  
# [25] "xtrain"         "ytrain" 

## In preapration to do the data consolidation, filter the feature variables:

df_features <- list_data_frames_from_UCI_HAR_data_set$features

raw_features <- as.character( df_features$V2 )

features_mean_and_std <- grep( pattern = ".*-mean.*\\(\\).*|.*-std\\(\\).*", # extract only the means and std of measurements
                              x = raw_features, 
                              ignore.case = TRUE,
                              value = TRUE ) 

features_mean_and_std <- Cleannames( features_mean_and_std )

cleanfeatures <- Cleannames( list_data_frames_from_UCI_HAR_data_set$features$V2 )
cleanactivitylabels <- Cleannames(list_data_frames_from_UCI_HAR_data_set$activitylabels$V2)

## Assign the clean feature labels to the names of the "X" data sets: changing V___ for actual names
## Note: the "X" data frames were obtained from reading the files 'X_test.txt' and 'X_train.txt'

names(list_data_frames_from_UCI_HAR_data_set$xtest) <- cleanfeatures
names(list_data_frames_from_UCI_HAR_data_set$xtrain) <- cleanfeatures # they are identical now, necessary for merging

## Subset the feature data frames to have only the mean and standard deviation as requested in project goals:

## proxys to shorten lines
df_test <- list_data_frames_from_UCI_HAR_data_set$xtest 
df_train <- list_data_frames_from_UCI_HAR_data_set$xtrain

## actual subsetting with target feature columns
df_test_features <- df_test[, names(df_test) %in% features_mean_and_std ]
df_train_features <- df_train[, names(df_train) %in% features_mean_and_std ]

## clean up the R environment:
rm(df_features); rm(df_test); rm(df_train)

## Create one single data frame with the selected features

features <- do.call( what = "rbind", # append rows at the end of test features, all column labels are identical
                     args = list( df_test_features,
                                  df_train_features ) )

## Prepare clean activity labels:

list_data_frames_from_UCI_HAR_data_set$activitylabels$V2 <-
  Cleannames( list_data_frames_from_UCI_HAR_data_set$activitylabels$V2 ) 

### create a data frame with features, subject and activity ids:

## First make a unique subject vector with test and train subjects back to back:
subjectid <- c( list_data_frames_from_UCI_HAR_data_set$subjecttest$V1, 
                list_data_frames_from_UCI_HAR_data_set$subjecttrain$V1) 

## Then make a unique activity vector with test and train activities back to back:
activityid <- c( list_data_frames_from_UCI_HAR_data_set$ytest$V1,
                   list_data_frames_from_UCI_HAR_data_set$ytrain$V1) # a unique activity vector test and train back to back

## Create the tidy datat frame,
## the observations are 2947 from test + 7352 from training = 10299 with 79+2=81 variables: 

data_frame <-data.frame(subjectid, # 1 variable
                        activityid, # 1 variable
                        features ) # 79 variables, the means and standard deviations of measurements


## Add a column with readable activity names by doing a data frame merge matching on the activity id.
## First put the proper names in the data frame with the activity labels to make the match work:

names(list_data_frames_from_UCI_HAR_data_set$activitylabels)<-c("activityid","activityname") # instead of original V1 and V2

## Add a column of friendly human readable labels for activities 
## In the following the parameter 'all=TRUE' forces the activityname values to be added to the merged data.frame

data_frame <- merge( x = data_frame,
                     y = list_data_frames_from_UCI_HAR_data_set$activitylabels,
                     all=TRUE) # this match is done on the only common column between x and y: "activityid"

activities<- split(data_frame, data_frame$activityname)
means_by_activity <- sapply( activities, function(x) colMeans(x[3:81]) )
write.csv(x=means_by_activity,file="meansbyactivity.csv")

subjects<- split(data_frame, data_frame$subjectid)
means_by_subjectid <- sapply( subjects, function(x) colMeans(x[3:81]) )
write.csv(x=means_by_activity,file="meansbysubject.csv")

# library(gridExtra)
