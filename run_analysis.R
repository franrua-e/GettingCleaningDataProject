## This script assumes the test and train folders are already in the
#directory.


#Read the column names
features <- read.table(file="features.txt", col.names=c("nber","label"))

#Read the subjects
subject_test<-read.table(file="test/subject_test.txt", col.names="subject")
subject_train<-read.table(file="train/subject_train.txt", col.names="subject")

#Read the datasets
x_train <- read.table(file="train/X_train.txt")
y_train <- read.table(file="train/y_train.txt", col.names = "code")
x_test <- read.table(file="test/X_test.txt")
y_test <- read.table(file="test/y_test.txt", col.names="code")


##Assign column names to x_train and x_test
colnames(x_train)<- features$label
colnames(x_test)<-features$label

## 1) Merge train and test set

## Now we merge the train and test sets of X and Y
x_train_test <- rbind(x_train, x_test)
y_train_test <-rbind(y_train, y_test)
subject <- rbind(subject_train,subject_test)
all_data <- cbind(subject, y_train_test,x_train_test)

## 2) Mean and standard deviation

##Extracting column names
column_names <- colnames(all_data)
## Index vector
mean_std<-grep("mean\\(\\)|std\\(\\)|subject|code", column_names)
## Subsetting columns
all_data<-all_data[,mean_std]


## 3) Use descriptive activities name

## Reading file containing activities name
activities_name <- read.table("activity_labels.txt", col.names=c("code","activity"))
## Merge data
all_data <- merge(x=all_data, y=activities_name, by.x="code", by.y="code")
## Reorder the columns for simplicity. We also drop 'code' column.
all_data<- all_data[,c(2,69,3:68)]


## 4) Appropriately label the data set variables

## Extract column names
all_data_colnames <- colnames(all_data)
## Create pattern vector
pattern_vector <-c("^f", "\\(\\)", "-", "Mag", "^t","BodyBody") ##Escape special characters "("
## Create replacement vector
replacement_vector <-c("FFT", "","_","Magnitude", "Time","Body")

#Iterate over pattern_vector. Alternatively one can iterate over
# replacement vector too.
for (index in 1:length(pattern_vector)){
  all_data_colnames <- gsub(pattern_vector[index], 
                                replacement_vector[index],
                            all_data_colnames)
 
}

##Assign new column names
colnames(all_data)<-all_data_colnames


## 5) Create new tidy data set with average per activity and subject

all_data_2 <- all_data
if (!"dplyr" %in% installed.packages()) {
  install.packages("dplyr")
}
library("dplyr")


final_data <- all_data %>% group_by(subject, activity) %>% 
  summarise(across(where(is.numeric), mean))
write.table(final_data, "tidy_data.txt", sep=",", row.names=FALSE)
str(final_data)
