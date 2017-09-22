###1. Merges the training and the test sets to create one data set.###
###4. Appropriately labels the data set with descriptive variable names.###

##Set work directory##
directory<-"YOUR/WOPK/PATH"
setwd(directory)

##Read training and test sets, and labels the data set with descriptive variable names##
train_set_raw<-read.table("./train/X_train.txt",header=F,row.names=NULL)
test_set_raw<-read.table("./test/X_test.txt",header=F,row.names=NULL)
feature<-read.table("./features.txt",row.names=1,header=F)
colnames(train_set_raw)<-t(feature)
colnames(test_set_raw)<-t(feature)

##Read training and test activity and subject labels##
train_ActivityLabel<-read.table("./train/y_train.txt",header=F,row.names=NULL)
test_ActivityLabel<-read.table("./test/y_test.txt",header=F,row.names=NULL)
train_SubjectLabel<-read.table("./train/subject_train.txt",header=F,row.names=NULL)
test_SubjectLabel<-read.table("./test/subject_test.txt",header=F,row.names=NULL)

##Add activity and subject labels to training and test data##
train_set_final<-cbind(train_ActivityLabel,train_SubjectLabel,train_set_raw)
colnames(train_set_final)[1:2]<-c("ActivityLabel","SubjectLabel")
test_set_final<-cbind(test_ActivityLabel,test_SubjectLabel,test_set_raw)
colnames(test_set_final)[1:2]<-c("ActivityLabel","SubjectLabel")

##Merge the training and the test sets to create one data set##
train_test_set_final<-rbind(train_set_final,test_set_final)

###2. Extracts only the measurements on the mean and standard deviation for each measurement.###

mean_std_feature<-grep("mean|std",colnames(train_test_set_final),ignore.case = T)
train_test_mean_std<-train_test_set_final[,c(1:2,(mean_std_feature))]

###3. Uses descriptive activity names to name the activities in the data set.###

activity_contrast<-read.table("./activity_labels.txt",header=F,row.names=NULL)
ActivityName<-activity_contrast[train_test_mean_std$ActivityLabel,2]
train_test_mean_std$ActivityLabel<-ActivityName
colnames(train_test_mean_std)[1]<-"ActivityName"

###5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.###

avg_train_test_mean_std<-aggregate(x = train_test_mean_std[,-1:-2], by = list(train_test_mean_std$ActivityName, train_test_mean_std$SubjectLabel), FUN = "mean")
colnames(avg_train_test_mean_std)[1:2]<-c("ActivityName","SubjectLabel")
avg_train_test_mean_std_final<-cbind(avg_train_test_mean_std$SubjectLabel,avg_train_test_mean_std$ActivityName,avg_train_test_mean_std[,-1:-2])
colnames(avg_train_test_mean_std_final)[1:2]<-c("SubjectLabel","ActivityName")

write.table(avg_train_test_mean_std_final,"tidy_data.txt",quote=F,sep="\t",row.names=F,col.names=T)
