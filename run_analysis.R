library(dplyr)

#get the current path
path<-getwd()

#set the path to files
path_train<-"course/project4/UCI HAR Dataset/train"
path_test<-"course/project4/UCI HAR Dataset/test"

#read the train data and test data
subject_train<-read.csv(paste(path,paste(path_train,"subject_train.txt",sep="/"),sep="/"),header = FALSE,sep = "")
subject_test<-read.csv(paste(path,paste(path_test,"subject_test.txt",sep="/"),sep="/"),header = FALSE,sep = "")
x_train<-read.csv(paste(path,paste(path_train,"X_train.txt",sep="/"),sep="/"),header = FALSE,sep = "")
x_test<-read.csv(paste(path,paste(path_test,"X_test.txt",sep="/"),sep="/"),header = FALSE,sep = "")
y_train<-read.csv(paste(path,paste(path_train,"y_train.txt",sep="/"),sep="/"),header = FALSE,sep = "")
y_test<-read.csv(paste(path,paste(path_test,"y_test.txt",sep="/"),sep="/"),header = FALSE,sep = "")

#read the variable list from the file
feature_list<-read.table("D:\\PMG\\ANALYTICS\\rcode\\course\\project4\\UCI HAR Dataset\\features.txt",header = FALSE,sep = "")

#selecting the second column in the file to get the variable list
feature_name<-as.vector(feature_list[,2])

#read the activity lables from the file
activity_label<-read.table("D:\\PMG\\ANALYTICS\\rcode\\course\\project4\\UCI HAR Dataset\\activity_labels.txt",header = FALSE,sep = "")

#assign column names
colnames(subject_train)<-c("subject_id")
colnames(subject_test)<-c("subject_id")

#combine train and test subject id 
subject_train<-rbind(subject_train,subject_test)

#replacing , to _
feature_name<-gsub(",","_",feature_name)

#assiging variable names to x and y data
colnames(x_train)<-feature_name
colnames(y_train)<-c("activity_id")

colnames(x_test)<-feature_name
colnames(y_test)<-c("activity_id")

colnames(activity_label)<-c("activity_id","activity_name")

# merging y data and activity label
merged_y<-merge(y_train,activity_label)
merged_test_y<-merge(y_test,activity_label)

colnames(merged_y)<-c("id","activity_name")
colnames(merged_test_y)<-c("id","activity_name")

#find the columns with mean and std
match_col<-feature_name[grep("mean\\(\\)|std\\(\\)",feature_name)]

#binding x train and x test
x_train<-rbind(x_train,x_test)

#binding y train and y test
merged_y<-rbind(merged_y,merged_test_y)

#selecting the data with matched columns (mean and std)
merge_data<-x_train[,match_col]
merge_data<-cbind(subject_train,merged_y[2],merge_data)

#grouping based on subject and activity and creating a tidy data set
tidy_data<-merge_data%>%group_by(.dots = c("subject_id","activity_name"))%>%summarize_all(funs(mean))