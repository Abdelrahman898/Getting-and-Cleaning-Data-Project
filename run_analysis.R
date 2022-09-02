
library(dplyr)
library(readr)

#check if file exist

if(!file.exists("projectfiles_UCI HAR Dataset.zip")) {
      
      fileurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
      destfile<-"E:/Abdelrahmane/IMPORTANT/Johns hopkins Data Science - Coursera/abdelrahman/course 3/projectfiles/projectfiles_UCI HAR Dataset.zip"
      download.file(fileurl,destfile)
}

#get out directory ready and name each one so i can easy get it back 

directory1<-"E:/Abdelrahmane/IMPORTANT/Johns hopkins Data Science - Coursera/abdelrahman/course 3/projectfiles/projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt"
activites<-read.table(directory1,col.names = c("code", "activity"))
head(activites)

directory2<-"E:/Abdelrahmane/IMPORTANT/Johns hopkins Data Science - Coursera/abdelrahman/course 3/projectfiles/projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt"
features<-read.table(directory2,col.names = c("n","signals"))
head(features)

directory3<-"E:/Abdelrahmane/IMPORTANT/Johns hopkins Data Science - Coursera/abdelrahman/course 3/projectfiles/projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt"
subject_test<-read.table(directory3,col.names = "subject")
head(subject_test)

directory4<-"E:/Abdelrahmane/IMPORTANT/Johns hopkins Data Science - Coursera/abdelrahman/course 3/projectfiles/projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt"
X_test<-read.table(directory4,col.names = features$signals)
head(X_test)

directory5<-"E:/Abdelrahmane/IMPORTANT/Johns hopkins Data Science - Coursera/abdelrahman/course 3/projectfiles/projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt"
y_test<-read.table(directory5,col.names = "code")
head(y_test)


directory6<-"E:/Abdelrahmane/IMPORTANT/Johns hopkins Data Science - Coursera/abdelrahman/course 3/projectfiles/projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt"
subject_train<-read.table(directory6,col.names = "subject")
head(subject_train)

directory7<-"E:/Abdelrahmane/IMPORTANT/Johns hopkins Data Science - Coursera/abdelrahman/course 3/projectfiles/projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt"
X_train<-read.table(directory7,col.names = features$signals)
head(X_train)

directory8<-"E:/Abdelrahmane/IMPORTANT/Johns hopkins Data Science - Coursera/abdelrahman/course 3/projectfiles/projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt"
y_train<-read.table(directory8,col.names = "code")
head(y_train)

#Q1 - Merges the training and the test sets to create one data set.

# Bind the files of the test set together by columns
x<-cbind(subject_test,X_test,y_test)
#Bind the files of the train set together by columns
y<-cbind(subject_train,X_train,y_train)
#merge our data together
merged_data<-rbind(x,y)
# take a look 
View(merged_data)

#Q2 - Extracts only the measurements on the mean and standard deviation for each measurement. 

#see attributes so you can get an idea what it's look like
names(merged_data)
# we use select function from dplyr to extract subject,code that contain mean and std from our data  
target_data  <- merged_data %>% select(subject, code, contains("mean"), contains("std"))
#see how our data frame look
View(target_data )
names(target_data )

#Q3 - Uses descriptive activity names to name the activities in the data set.

target_data$code <- activites[target_data$code,2]

#Q4 - Appropriately labels the data set with descriptive variable names.

#after step 3 in Q3 we named code to activity variable 
names(target_data)[2] = "activity" 
#name every Acc in our named variable col to Accelerometer 
names(target_data)<-gsub("Acc", "Accelerometer", names(target_data))
#name every Gyro in our named variable col to Gyroscope 
names(target_data)<-gsub("Gyro", "Gyroscope", names(target_data))
#name every BodyBody in our named variable col to Body 
names(target_data)<-gsub("BodyBody", "Body", names(target_data))
#name every Mag in our named variable col to Magnitude 
names(target_data)<-gsub("Mag", "Magnitude", names(target_data))
#every attributes began with t convert it to Time
names(target_data)<-gsub("^t", "Time", names(target_data))
#every attributes began with f convert it to Frequency
names(target_data)<-gsub("^f", "Frequency", names(target_data))
#every attributes with tBody convert it to TimeBody
names(target_data)<-gsub("tBody", "TimeBody", names(target_data))
#every attributes with fBody convert it to FrequencyBody
names(target_data)<-gsub("fBody", "FrequencyBody", names(target_data))
#name every angle in our named variable col to Angle 
names(target_data)<-gsub("angle", "Angle", names(target_data))
#name every gravity in our named variable col to Gravity 
names(target_data)<-gsub("gravity", "Gravity", names(target_data))
#named every .std to STD and here i remove dot, we chose ignore.case to ignore sensitivity
names(target_data)<-gsub(".std", "STD", names(target_data),ignore.case = TRUE)
#named every .mean to Mean and here i remove dot, we chose ignore.case to ignore sensitivity
names(target_data)<-gsub(".mean", "Mean", names(target_data),ignore.case = TRUE)
#named every freq to Frequency, we chose ignore.case to ignore sensitivity
names(target_data)<-gsub("freq", "Frequency", names(target_data), ignore.case = TRUE)

names(target_data)
tidydata<-target_data
View(tidydata)

#Q5 - From the data set in step 4, creates a second, independent tidy data set
# with the average of each variable for each activity and each subject.

req_data<-tidydata %>% group_by(subject,activity) %>% summarise_all(funs(mean))
write.table(req_data, "req_data.txt", row.name=FALSE)

#check and have a look at our data
str(req_data)
View(req_data)


