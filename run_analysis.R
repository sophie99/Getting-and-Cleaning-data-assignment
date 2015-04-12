## Read in data sets
trainingData <- read.table("./Dataset/train/X_train.txt") 
str(trainingData)
trainingLabels <- read.table("./Dataset/train/y_train.txt") 
str(trainingLabels)
trainingSubject <- read.table("./Dataset/train/subject_train.txt")
str(trainingSubject)
testData <- read.table("./Dataset/test/X_test.txt") 
testLabels <- read.table("./Dataset/test/y_test.txt")  
testSubject<-read.table("./Dataset/test/subject_test.txt")
Features<-read.table("./Dataset/features.txt")
activityLabels<-read.table("./Dataset/activity_labels.txt")


## Merge the training and the test sets to create one data set

MergeData<-rbind(trainingData,testData)
str(MergeData)
names(MergeData)<-Features$V2
MergeLabels<-rbind(trainingLabels,testLabels)
names(MergeLabels)<-"Activities"
MergeSubject<-rbind(trainingSubject,testSubject)
names(MergeSubject)<-"Subject"
Merge2<-cbind(MergeLabels,MergeData)
MergeAll<-cbind(Merge2,MergeSubject)
str(MergeAll)

## Extracts only the measurements on the mean and standard deviation for each measurement
meanSD <- grep("mean\\(\\)|std\\(\\)", Features[, 2])
str(meanSD)
data.meanSD<-MergeAll[,meanSD]

## Uses descriptive activity names to name the activities in the data set
MergeAll$Activities[MergeAll$Activities == 1] <- "Walking"
MergeAll$Activities[MergeAll$Activities == 2] <- "Walking Upstairs"
MergeAll$Activities[MergeAll$Activities == 3] <- "Walking Downstairs"
MergeAll$Activities[MergeAll$Activities == 4] <- "Sitting"
MergeAll$Activities[MergeAll$Activities == 5] <- "Standing"
MergeAll$Activities[MergeAll$Activities == 6] <- "Laying"


## Appropriately labels the data set with descriptive variable names
names(MergeAll)
names(MergeAll) <- gsub("Acc", "Accelerator", names(MergeAll))
names(MergeAll) <- gsub("Mag", "Magnitude", names(MergeAll))
names(MergeAll) <- gsub("Gyro", "Gyroscope", names(MergeAll))
names(MergeAll) <- gsub("^t", "time", names(MergeAll))
names(MergeAll) <- gsub("^f", "frequency", names(MergeAll))
names(MergeAll) <- gsub("Activities","ActivityLabels",names(MergeAll))
names(MergeAll) <- gsub("-| ","",names(MergeAll))
names(MergeAll) <- gsub("std\\()","_StandardDeviation", names(MergeAll)) 
names(MergeAll) <- gsub("mean\\()","_Mean",names(MergeAll))



## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

library(reshape)
library(dplyr)
measures<- grep("_Mean|_StandardDeviation",names(MergeAll),value=TRUE) 
data.melt<-melt(MergeAll,id.var=c("Subject","ActivityLabels"),measure.vars=measures)
head(data.melt)
data.tidy<-cast(data.melt,Subject + ActivityLabels ~ variable, mean)
str(data.tidy)
tidy.data<-arrange(data.tidy, Subject, ActivityLabels)
write.table(tidy.data,"tidy_data.txt")


