*CODE BOOK* /n 
1) It is assumed that the database was already downloaded and it's in a local
directory.

2) We start by reading the datasets, namely, train, test, activity labels, 
features and subjects. 

From the features data set we assign the variables for each train and test table

colnames(x_train) <- features$label
colnames(x_test) <- features$label

Then we proceed to merge the train and test dataset as demanded by the project's instructions.

x_train_test <- rbind(x_train, x_test)
y_train_test <-rbind(y_train, y_test)
subject <- rbind(subject_train,subject_test)
all_data <- cbind(subject, y_train_test,x_train_test)

3) After that, we extract the column names from the dataset 'all_data' to filter out the columns that are not labeled or contain 'mean()' or 'std()'.

mean_std<-grep("mean\\(\\)|std\\(\\)|subject|code", column_names)
all_data<-all_data[,mean_std]
We preserve the subject and code column.

4) We rename the variables of the merged data set and make it more explicit
and verbose without making it too long.
In order to do it dynamically, we created two vector: pattern and replacement
and with a loop we change iteratively the patterns.

for (index in 1:length(pattern_vector)){
  all_data_colnames <- gsub(pattern_vector[index], 
                                replacement_vector[index],
                            all_data_colnames)
                            
5) Finally, with the dplyr package installed and loaded, we proceed to group
and summarise the information.

final_data <- all_data %>% group_by(subject, activity) %>% 
  summarise(across(where(is.numeric), mean))

