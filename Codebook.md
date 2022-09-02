 # CodeBook for Getting and Cleaning Data Course Project
 
This codebook explains the run_analysis.R script, which performs the data preparation and then followed by the 5 steps required as described in the course project's definition.

1-Dataset downloaded and extracted under the folder called UCI HAR Dataset

2-Assign each data to variables:
i put my directory so you can skip it when you read it
 - features <- features.txt : 561 rows, 2 columns 
   The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ.
   
 - activities <- activity_labels.txt : 6 rows, 2 columns
   List of activities performed when the corresponding measurements were taken and its codes (labels)
   
 - subject_test <- test/subject_test.txt : 2947 rows, 1 column
   contains test data of 9/30 volunteer test subjects being observed
   
 - x_test <- test/X_test.txt : 2947 rows, 561 columns
   contains recorded features test data
   
 - y_test <- test/y_test.txt : 2947 rows, 1 columns
   contains test data of activities'code labels 
   
 - subject_train <- test/subject_train.txt : 7352 rows, 1 column
   contains train data of 21/30 volunteer subjects being observed
   
 - x_train <- test/X_train.txt : 7352 rows, 561 columns
   contains recorded features train data
   
 - y_train <- test/y_train.txt : 7352 rows, 1 columns
   contains train data of activities'code labels

3-Merges the training and the test sets to create one data set

 - ( x ) Bind the files of the test set together by columns by cbind() fun
 - ( y ) Bind the files of the train set together by columns by cbind() fun
 - merged_data is to merge x,y bt rbind() fun


4-Extracts only the measurements on the mean and standard deviation for each measurement

  - target_data we use select() function from dplyr to extract subject,code that contain mean and std from our data

5-Uses descriptive activity names to name the activities in the data set

  - Entire numbers in code column of the target_data replaced with corresponding activity taken from second column of the activities variable


6-Appropriately labels the data set with descriptive variable names

  - code column in TidyData renamed into activities
  - All Acc in column's name replaced by Accelerometer
  - All Gyro in column's name replaced by Gyroscope
  - All BodyBody in column's name replaced by Body
  - All Mag in column's name replaced by Magnitude
  - All start with character f in column's name replaced by Frequency
  - All start with character t in column's name replaced by Time

7-From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

  - req_data (180 rows, 88 columns) is created by sumarizing target_data taking the means of each variable for each activity and each subject, after groupped by subject and activity
  - take a look at req_data into req_data.txt file.









