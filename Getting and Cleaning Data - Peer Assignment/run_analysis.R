## You should create one R script called run_analysis.R that does the following. 
##
## Merges the train and test sets to create one data set.
## Extracts only the measurements on the mean and standard deviation for each measurement. 
## Uses descriptive activity names to name the activities in the data set
## Appropriately labels the data set with descriptive variable names. 
## Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
##
##From the submission instructions:
## Please upload your data set as a txt file created with write.table() using row.name=FALSE
## (do not cut and paste a dataset directly into the text box, as this may cause errors saving your submission).

## We begin by putting the script in the same directory where we have the dataset directory
## (One level up from the actual data files, like features.txt)

## Using R 3.2.1

#Xtrain<-read.table("train/X_train.txt",header=FALSE,sep="")
#train<-read.table("train/y_train.txt",header=FALSE,sep="")
#subject_train<-read.table("train/subject_train.txt",header=FALSE,sep="")
library(downloader)
library(plyr)
library(dplyr)
library(Hmisc) 
library(reshape2)

##READ ALL THE DATA!
#Working directory at the location where the UCI HAR Dataset was unzipped
setwd('~/exdata-data-NEI_data');

##QUESTION 1:  Merges the training and the test sets to create one data set.
#Header
Feature<-read.table("features.txt",
                    header=FALSE,sep="")

#Read XTRAIN  PC
Xtrain<-read.table("train/X_train.txt",
                   header=FALSE,
                   col.names=Feature[,2],
                   sep="")
ytrain<-read.table("train/y_train.txt",
                   header=FALSE,
                   col.names="Activity",
                   sep="")
subject_train<-read.table("train/subject_train.txt",
                          col.names="Subject",
                          header=FALSE,sep="")

#Read XTEST PC
Xtest<-read.table("test/X_test.txt",
                  header=FALSE,
                  col.names=Feature[,2],
                  sep="")
ytest<-read.table("test/y_test.txt",
                  header=FALSE,
                  col.names="Activity",
                  sep="")
subject_test<-read.table("test/subject_test.txt",
                         header=FALSE,
                         col.names="Subject",
                         sep="")

#Merges Xtrain and subject_train
MERGE1<-cbind(Xtrain,ytrain,subject_train)

#Merges Xtest and subject_test
MERGE2<-cbind(Xtest,ytest,subject_test)

#Combines MERGE1 and MERGE2
MERGE3<-rbind(MERGE1,MERGE2)

MERGEDATA<-tbl_df(MERGE3)


##QUESTION 2:  Extracts only the measurements on the mean and standard deviation for each measurement. 

MERGEDATA2<-MERGEDATA%>%select (contains("mean"),
                                contains("Subject"),
                                contains("Activity"))
MERGEDATA3<-MERGEDATA2%>%select (contains("fBodyBodyAcc"),
                                 contains("fBodyBodyGyro"),
                                 contains("Subject"),
                                 contains("Activity"))
MERGEDATA4<-MERGEDATA3%>%select (contains("Mag"),
                                 contains("Subject"),
                                 contains("Activity"))
MERGEDATA5<-MERGEDATA4%>%select (contains("mean"),
                                 contains("Subject"),
                                 contains("Activity"))

##QUESTION 3: Uses descriptive activity names to name the activities in the data set
Activity<-1:6
Activity_Description<-c("WALKING", 
                        "WALKING_UPSTAIRS", 
                        "WALKING_DOWNSTAIRS", 
                        "SITTING", 
                        "STANDING", 
                        "LAYING")
ACTIVITY<-as.data.frame(cbind(Activity,Activity_Description))
MERGEDATA6<-join(MERGEDATA5,ACTIVITY)


##QUESTION 4:  Appropriately labels the data set with descriptive variable names. 
names(MERGEDATA6)<-gsub("fBodyBodyAcc",
                        "frequencyBodyAcceleration",
                        names(MERGEDATA6))
names(MERGEDATA6)<-gsub("fBodyBodyGyro",
                        "frequencyBodyAngularVelocity",
                        names(MERGEDATA6))
names(MERGEDATA6)<-gsub("Mag",
                        "Magnitude",
                        names(MERGEDATA6))


##QUESTION 5: From the data set in step 4, creates a second, independent tidy data set. 
##with the average of each variable for each activity (WALKING...) and each subject (1-2-3) .                        

#Step a: Data will be "melted" to get a narrow set of data.  "Subject" and "Activity_Description" are the ID variables. 
MERGEDATA7 <- melt(MERGEDATA6,id=c("Subject",
                                 "Activity_Description"))

#Step b: Casting data frames.
MERGEDATA8 <- MERGEDATA7%>%group_by(Subject,Activity_Description,variable)%>% summarise_each(funs(mean),value,na.rm=TRUE)

#Please upload the tidy data set created in step 5 of the instructions. Please upload your data set as a txt file created with write.table() using row.name=FALSE
write.table(MERGEDATA8,file="Tidy_DataSet.txt", row.name=FALSE)
