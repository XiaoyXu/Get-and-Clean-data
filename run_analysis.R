#Read the data 
sub_train <- read.table("subject_train.txt")
x_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")
sub_test <- read.table("subject_test.txt")
x_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
activity_lables<- read.table("activity_labels.txt")
features <- read.table("features.txt")
#Merge the training and test data
x_all<- rbind(x_train,x_test)
y_all<- rbind(y_train,y_test)
#calculate mean and std
colnames(x_all) <- c(as.character(features[,2]))
mean<- grep("mean()", colnames(x_all), fixed = TRUE)
std <- grep("std()", colnames(x_all), fixed = TRUE)
MeanSTD<- x_all[,c(mean,std)]
activity_all<-cbind(y_all,MeanSTD)
colnames(activity_all)[1] <- "Activity"
#reshaping the data sets
activity_lables[,2]<-as.character(activity_lables[,2])
for(i in 1:length(activity_all[,1])){
  activity_all[i,1]<-activity_lables[activity_all[i,1],2]
}

subject_all<-rbind(sub_train,sub_test)
all<-cbind(subject_all, activity_all)
colnames(all)[1] <- "Subject"
tidydata <- aggregate( all[,3] ~ Subject+Activity, data = all, FUN= "mean" )
for(i in 4:ncol(all)){
  tidydata[,i] <- aggregate( all[,i] ~ Subject+Activity, data = all, FUN= "mean" )[,3]
}
colnames(tidydata)[3:ncol(tidydata)] <- colnames(MeanSTD)
#output
write.table(tidydata, file = "TidyData.txt")