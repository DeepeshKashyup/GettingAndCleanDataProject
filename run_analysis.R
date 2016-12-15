#install packages 
install.packages("dplyr")
install.packages("tidyr")
#load packages
library(dplyr)
library(tidyr)

#Code to download and unzip the data file
if(!file.exists("UCI HAR Dataset")){
    temp <- tempfile()
    download.file(url,temp)
    f<- unzip(temp)
    unlink(temp)
}


activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

features <- read.table("./UCI HAR Dataset/features.txt")
features.req <- grep(".mean|.std",features[,2])
features.req.names <-features[features.req,2]
features.req.names <- gsub(".-mean","Mean",features.req.names)
features.req.names <- gsub(".-std","Std",features.req.names)
features.req.names <- gsub("[-()]","",features.req.names)


#code to read train data set and add other crucial columns such as Subject and Activites
train <- read.table("./UCI HAR Dataset/train/X_train.txt")[features.req]
trainSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
trainActivities <- read.table("./UCI HAR Dataset/train/y_train.txt")
train<-cbind(trainSubject,trainActivities,train)

#code to read test data set and add other crucial columns such as Subject and Activites
test <- read.table("./UCI HAR Dataset/test/X_test.txt")[features.req]
testSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
testActivities <- read.table("./UCI HAR Dataset/test/y_test.txt")
test<-cbind(testSubject,testActivities,test)

#code to merge test and train data
completeData <- rbind(train,test)
colnames(completeData)<- c("Subject","Activities",features.req.names)

#code to modify data 
completeData$Subject<-as.factor(completeData$Subject)
completeData$Activities <- factor(completeData$Activities,levels=activity_labels[,1],labels = activity_labels[,2])

#code to clean data in order to determine mean of each variable
completeData.gathered<- gather(completeData,variables,values,-Subject,-Activities)
completedData.gathered.grouped <- group_by(completeData.gathered,Subject,Activities,variables)
completeData.mean<- summarise(completedData.gathered.grouped,mean =mean(values))
completeData.tidy <- arrange(spread(completeData.mean,variables,mean),Subject,Activities)

#code to write the tidy data to a file 
write.table(completeData.tidy, "tidyData.txt", row.names = FALSE, quote = FALSE)