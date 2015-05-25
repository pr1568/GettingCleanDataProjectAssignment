#Download the Data

*Provide the raw URL where the raw data store
rawURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
* Destination File Name
destFile <- "rawdata.zip"
* Download the Data file with a check if it is not already exists
if(!file.exists(paste(getwd(),destFile,sep="/")) ) {
  download.file(rawURL, destFile, method = 'curl')
}

*Unzip the data

* unzip the file with overwrite as true as it may be possible files may be modified and overwriting 
* will renew the file 
unzip(destFile, overwrite = TRUE, exdir = ".")
* get the raw data file location
rawDataPath = paste(getwd(),"UCI HAR Dataset", sep="/")
* check list of files in all directory under the above path recursively 
list.files(rawDataPath,recursive = TRUE)

# Load the data observations in separate data frame for manipulation

* Reading the required files
* Read the Subject (who performed the activity for each window sample) Train and Test files and then convert into 
* Data.table for better operation
subjectTrainPath <- file.path(rawDataPath, "train","subject_train.txt")
subjectTestPath  <- file.path(rawDataPath, "test","subject_test.txt")
dfSubTrain <- read.table(subjectTrainPath)
dfSubTrainData <- data.table(dfSubTrain)
dfSubTest <- read.table(subjectTestPath)
dfSubTestData <- data.table(dfSubTest)


## Read the Training and Test data
XTrainPath <- file.path(rawDataPath, "train","X_train.txt")
XTestPath  <- file.path(rawDataPath, "test","X_test.txt")
dfXTrain <- read.table(XTrainPath)
dfXTrainData <- data.table(dfXTrain)
dfXTest <- read.table(XTestPath)
dfXTestData <- data.table(dfXTest)

## Read the Training and Test Lables
YTrainPath <- file.path(rawDataPath, "train","Y_train.txt")
YTestPath  <- file.path(rawDataPath, "test","Y_test.txt")
dfYTrain <- read.table(YTrainPath)
dfYTrainData <- data.table(dfYTrain)
dfYTest <- read.table(YTestPath)
dfYTestData <- data.table(dfYTest)

#Data Manipulation for final data preparation 

* Do a row bind on all the Labels, Activity and Results and seting name of the data

allDataSubject <- rbind(dfSubTrainData, dfSubTestData )
setnames(allDataSubject,"V1", "Subject")

allDataActivityLables <- rbind(dfYTrainData,dfYTestData)
setnames(allDataActivityLables, "V1", "ActivityLabelNum")

allDataResults <- rbind(dfXTrainData,dfXTestData)

* put together the dataset in columnar fashsion 
allDataSubject <- cbind(allDataSubject,allDataActivityLables)
finalData <- cbind(allDataSubject,allDataResults)

* Seting key for sorting
setkey(finalData,Subject,ActivityLabelNum)



### Read the Features and extract the measurement number where we have Standard Deviation and mean values
dataFeaturesPath <- file.path(rawDataPath,"features.txt")
dataFeatures <- fread(dataFeaturesPath)
setnames(dataFeatures, names(dataFeatures), c("Feature_Number", "Feature_Name"))
dataFeatures <- dataFeatures[grepl("mean\\(\\)|std\\(\\)", Feature_Name)]


### Now get the matching column name from the above subset feature data table so that we 
### can merge the data with FinalData
dataFeatures$fcode <- dataFeatures[, paste0("V",Feature_Number)]
filter <- c(key(finalData), dataFeatures$fcode)
finalData <- finalData[, filter, with=FALSE]

* finalData showing activity as number replace this information 
* with actual activity label so that data look more readable

activityLabelPath <- file.path(rawDataPath,"activity_labels.txt")
dfActivityLabel <- read.table(activityLabelPath)
setnames(dfActivityLabel,names(dfActivityLabel), c("ActivityLabelNum","Activity_Name"))
finalData <- merge(finalData,dfActivityLabel, by ="ActivityLabelNum", all.x=TRUE )

* Now add the key actual activity name for operation by activty name

setkey(finalData,Subject,ActivityLabelNum, Activity_Name)

* Melt the table to reshape it by activity 

finalData <- data.table(melt(finalData, key(finalData),variable.name = "fcode"))
# Merge all activity

finalData <- merge(finalData, dataFeatures[, list(Feature_Number, fcode, Feature_Name)], by="fcode", all.x=TRUE)

* get the activity and feature as factor
finalData$activities <- factor(finalData$Activity_Name)
finalData$features <- factor(finalData$Feature_Name)
* Create a local function to find all different features and get the meaningful lables
greptext <- function (regex) {
  grepl(regex, finalData$features)
}
### Features with 1 category
finalData$feature_Jerk <- factor(greptext("Jerk"), labels=c(NA, "Jerk"))
finalData$feature_Magnitude <- factor(greptext("Mag"), labels=c(NA, "Magnitude"))

### Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(greptext("^t"), greptext("^f")), ncol=nrow(y))
finalData$feature_Domain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(greptext("Acc"), greptext("Gyro")), ncol=nrow(y))
finalData$feature_Instrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(greptext("BodyAcc"), greptext("GravityAcc")), ncol=nrow(y))
finalData$feature_Acceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(greptext("mean()"), greptext("std()")), ncol=nrow(y))
finalData$feature_Variable <- factor(x %*% y, labels=c("Mean", "SD"))

### Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(greptext("-X"), greptext("-Y"), greptext("-Z")), ncol=nrow(y))
finalData$feature_Axis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

#Final Data output
### Set the keys of data in data table
setkey(finalData, Subject, activities, feature_Domain, feature_Acceleration, feature_Instrument, feature_Jerk, feature_Magnitude, feature_Variable, feature_Axis)
### Create the Final Tidy Data Set as per ask
finalTidyData <- finalData[, list(count = .N, average = mean(value)), by=key(finalData)]










