## R Script for the getting and cleaning data course project

# require packages
require(dplyr)

###########################################################################
#                                                                         #
# Step 1: Merges the training and the test sets to create one data set.   #
#                                                                         #
###########################################################################


# read in feature labels to apply to test and training datasets
featurelabels <- read.table(file = "./UCI HAR Dataset/features.txt", header = FALSE)

## read in Test Data

# read in the test data set
testdata <- read.table(file = "./UCI HAR Dataset//test//X_test.txt",header = FALSE)
# add feature labels to test data as column names 
names(x = testdata) <- featurelabels[,2]
# read in the test data's test labels
testlabel <- read.table(file = "UCI HAR Dataset/test/y_test.txt", header = FALSE)
#read in the test data's subject labels
testsubject <- read.table(file = "UCI HAR Dataset/test/subject_test.txt", header = FALSE)
# add the subject labels and test labels to the data
alltestdata <- cbind(testsubject,testlabel,testdata)
# label the subject and activity columns
names(alltestdata)[1] <- "Subject"
names(alltestdata)[2] <- "Activity"


## read in Training Data

# read in the training data set
traindata <- read.table(file = "./UCI HAR Dataset//train/X_train.txt",header = FALSE)
# add feature labels to test data as column names 
names(x = traindata) <- featurelabels[,2]
# read in the training data's training labels
trainlabel <- read.table(file = "./UCI HAR Dataset//train/y_train.txt",header = FALSE)
# read in the training data's subject labels
trainsubject <- read.table(file = "./UCI HAR Dataset//train/subject_train.txt",header = FALSE)
# add the subject lables and train labels to the data
alltraindata <-cbind(trainsubject,trainlabel,traindata)
# label the subject and activity columns
names(alltraindata)[1] <- "Subject"
names(alltraindata)[2] <- "Activity"

## Merge Data
alldata <- rbind(alltestdata, alltraindata)


###################################################################################################
#                                                                                                 #
# Step 2: Extracts only the measurements on the mean and standard deviation for each measurement. #
#                                                                                                 #
###################################################################################################

## using the information from ./UCI HAR Dataset/features_info.txt we exclude variables that
## do not end with mean(): Mean value or std(): Standard deviation

# use grep to find indices of columns that contain mean()
mean <- grep("mean\\(\\)", colnames(alldata))
#use grep to find indices of columns that contain std()
std <- grep("std\\(\\)", colnames(alldata))
# add the first two column indices and then sort the indices
extractcolumns <- c(1,2,mean,std)
extractcolumns <- sort(extractcolumns)

# subset the data, extracting only the indices specified above
extracteddata <- alldata[,extractcolumns]


##################################################################################
#                                                                                #
# Step 3: Uses descriptive activity names to name the activities in the data set #
#                                                                                #
##################################################################################

## using the information from ./UCI HAR Dataset/activity_labels.txt label the values
## of the activity variable

# read in the activity labels
activitylabels <- read.table(file = "./UCI HAR Dataset/activity_labels.txt", header = FALSE)

# apply these labels using match
extracteddata$Activity <- as.character(activitylabels[match(extracteddata$Activity,  
                                activitylabels$V1), 'V2'])


##################################################################################
#                                                                                #
# Step 4: Appropriately labels the data set with descriptive variable names      #
#                                                                                #
##################################################################################

##
## We added the data owner's feature labels at the beginning when reading in the 
## test and train datasets as such we have descriptive variable names and these
## are further defined in the code book.
##

##################################################################################
#                                                                                #
# Step 5: From the data set in step 4, creates a second, independent tidy data   #
# set with the average of each variable for each activity and each subject       #
#                                                                                #
##################################################################################

# create a dataframe wit the average values for each feature for each activity 
# by subject using the dplyr package

# group the dataset by Subject and Activity
bysubjectandactivity <- group_by(.data = extracteddata,... = Subject, Activity)
# generate the mean of each feature for each subject and each activity
summariseddata <- summarise_each(tbl = bysubjectandactivity, funs = funs(mean))
# drop the additional column added by the group_by function
summariseddata[,1] <- NULL
# switch the column order to subject is first
summariseddata <- summariseddata[,c(2,1,3:68)]


## Write out txt file for submission
write.table(x = summariseddata,file = "./tidydata.txt",row.names = FALSE)




