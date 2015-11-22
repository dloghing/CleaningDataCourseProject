library(dplyr)

#DLL 20151120 Rename the columns of the data sets
#This assumes the feature list has already been loaded.
rename_columns <- function(table_name) {
    counter <- 1
    
    for(cur_name in feature_list[,2]) {
        #DLL 20151120 Remove the () at the end of the descriptions for easier string matching later.
        names(table_name)[counter] <- gsub("\\(\\)","",cur_name)
        counter <- counter + 1
    }    
    
    return (table_name)
}

run_analysis <- function() {
    
    #Set working directory here if needed.
    #setwd("D:\\dlog\\CourseraDataScience\\GettingCleaningData\\CourseProject\\UCI HAR Dataset")
    
    #DLL 20151120 Load the list of feature descriptions
    feature_list <- read.table("features.txt", sep = "", col.names = c("colnum", "colname"))

    #DLL 201520 Read the activity description labels
    activity_labels <- read.table("activity_labels.txt", sep="", col.names = c("activity_id", "activity_name"))
    
    #DLL 20151120 Load the training data and apply the descriptive column names
    xtrain <- read.table("train\\X_train.txt", sep = "")
    xtrain <- rename_columns(xtrain)
    
    #DLL 20151120 Load the activity data for the training set
    training_activity <- read.table("train\\Y_train.txt", sep = "", col.names = c("activity_id"))

    #DLL 20151120 Load the subject IDs for the training set
    training_subjects <- read.table("train\\subject_train.txt", sep = "", col.names = c("subject_id"))
    #colnames(training_subjects)[1] <- "subject_id"
    
    #DLL 201520 add the subject and activity columns to the training table
    xtrain <- cbind(xtrain, training_subjects, training_activity)
    
    #DLL 20151120 Load the test data and apply the descriptive column names
    xtest <- read.table("test\\X_test.txt", sep = "")
    xtest <- rename_columns(xtest)
    
    #DLL 20151120 Load the activity data for the test set
    test_activity <- read.table("test\\Y_test.txt", sep = "", col.names = c("activity_id"))

    #DLL 20151120 Load the subject IDs for the test set
    test_subjects <- read.table("test\\subject_test.txt", sep = "", col.names = c("subject_id"))
    #colnames(test_subjects)[1] <- "subject_id"
    
    #DLL 201520 add the subject and activity columns to the test table
    xtest <- cbind(xtest, test_subjects, test_activity)
    
    #DLL 20151120 Join (concatenate) the training and test data
    x_merged <- rbind(xtrain, xtest)
    
    #DLL 20151120 Pull out the mean and standard deviation (std) columns.  This will have meanFreq columns in it
    small_set <- cbind(x_merged[, c(grep("mean", colnames(x_merged), FALSE))], x_merged[, c(grep("std", colnames(x_merged), FALSE),562, 563)])
    
    #DLL 20151120 Remove the meanFreq columns -- this and the previous step might be condensed now that "()" is stripped out earlier
    mean_std_set <- cbind(small_set[,c(names(select(small_set, contains("_id"))),names(select(small_set, ends_with("mean"))),names(select(small_set, ends_with("std"))))])
    
    #DLL 20151120 Add the activity labels to the data
    mean_std_set <- merge(activity_labels, mean_std_set)
    
    #DLL 20151120 Remove activity data, group by subject and activity name, and calculate the means of the remaining columns
    result_set <- select(mean_std_set, -contains("activity_id")) %>% group_by(subject_id, activity_name) %>% summarise_each(funs(mean))
    
    #DLL 20151120 Write out the results
    #write.table(result_set,"mean_values.txt", row.names = FALSE)
    
    return(result_set)
    
}


