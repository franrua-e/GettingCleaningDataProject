---
title: "CodeBook"
author: "F.R."
date: ""
output: html_document
---
##Features
Feature Selection 
=================

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

mean(): Mean value
std(): Standard deviation
mad(): Median absolute deviation 
max(): Largest value in array
min(): Smallest value in array
sma(): Signal magnitude area
energy(): Energy measure. Sum of the squares divided by the number of values. 
iqr(): Interquartile range 
entropy(): Signal entropy
arCoeff(): Autorregresion coefficients with Burg order equal to 4
correlation(): correlation coefficient between two signals
maxInds(): index of the frequency component with largest magnitude
meanFreq(): Weighted average of the frequency components to obtain a mean frequency
skewness(): skewness of the frequency domain signal 
kurtosis(): kurtosis of the frequency domain signal 
bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean
tBodyAccMean
tBodyAccJerkMean
tBodyGyroMean
tBodyGyroJerkMean

The complete list of variables of each feature vector is available in 'features.txt'

##Process

1. We download the data set in a temporary file. 


2. We start by reading the datasets, namely, train, test, activity labels, 
features and subjects. 

```
#Read the column names
features <- read.table(file="UCI HAR Dataset/features.txt", col.names=c("nber","label"))

#Read the subjects
subject_test<-read.table(file="UCI HAR Dataset/test/subject_test.txt", col.names="subject")
subject_train<-read.table(file="UCI HAR Dataset/train/subject_train.txt", col.names="subject")

#Read the datasets
x_train <- read.table(file="UCI HAR Dataset/train/X_train.txt")
y_train <- read.table(file="UCI HAR Dataset/train/y_train.txt", col.names = "code")
x_test <- read.table(file="UCI HAR Dataset/test/X_test.txt")
y_test <- read.table(file="UCI HAR Dataset/test/y_test.txt", col.names="code"
```
From the features data set we assign the variables for each train and test table.
Then we proceed to merge the train and test dataset as demanded by the project's instructions.

```
x_train_test <- rbind(x_train, x_test)
y_train_test <-rbind(y_train, y_test)
subject <- rbind(subject_train,subject_test)
all_data <- cbind(subject, y_train_test,x_train_test)
```


3. Afterwards, we extract the column names from the dataset 'all_data' to filter out the columns that are not labeled or contain 'mean()' or 'std()'.

```
mean_std<-grep("mean\\(\\)|std\\(\\)|subject|code", column_names)
all_data<-all_data[,mean_std]
We preserve the subject and code column.
```
4. We rename the variables of the merged data set and make it more explicit
and verbose without making it too long.
In order to do it dynamically, we created two vector: pattern and replacement
and with a loop we change iteratively the patterns.

```
for (index in 1:length(pattern_vector)){
  all_data_colnames <- gsub(pattern_vector[index], 
                                replacement_vector[index],
                            all_data_colnames)
```
5. Finally, with the dplyr package installed and loaded, we proceed to group
and summarise the information.

```
final_data <- all_data %>% group_by(subject, activity) %>% 
  summarise(across(where(is.numeric), mean))
```