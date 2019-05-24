# Getting-and-Cleaning-Data
## Read test data
xtest<-read.delim("./UCI HAR Dataset/test/X_test.txt", header=FALSE,sep="",quote="")
ytest<-read.delim("./UCI HAR Dataset/test/y_test.txt", header=FALSE,sep="",quote="")
stest<-read.delim("./UCI HAR Dataset/test/subject_test.txt", header=FALSE,sep="",quote="")

## Read train data
xtrain<-read.delim("./UCI HAR Dataset/train/X_train.txt", header=FALSE,sep="",quote="")
ytrain<-read.delim("./UCI HAR Dataset/train/y_train.txt", header=FALSE,sep="",quote="")
strain<-read.delim("./UCI HAR Dataset/train/subject_train.txt", header=FALSE,sep="",quote="")

## Create one complete data set
train<-cbind(strain,ytrain,xtrain)
test<-cbind(stest,ytest,xtest)
data<-rbind(train,test)

## Read Column Names and apply to data set
col<-read.delim("UCI HAR Dataset/features.txt", header=FALSE,sep="",quote="",colClasses="character")
colNames<-c("subject","activity",col$V2)
names(data)<-colNames

##Choose only mean and standard deviation
mean<-data[,grepl("mean",names(data))]
std<-data[,grepl("std",names(data))]
selectdata<-cbind(data[,c(1,2)],mean,std)

## Rewrite activity names
act<-read.delim("./UCI HAR Dataset/activity_labels.txt", header=FALSE,sep="",quote="",colClasses="character")
act<-act$V2
for (i in 1:6) {
    selectdata$activity<-gsub(i,act[i],selectdata$activity)
}

View(selectdata)

##make new table
table<-data.frame(rep(0,79))
##create the means for each subject based on activity
for (i in 1:30) {
    stand<-filter(selectdata,activity=="STANDING",subject==i)
    standmean<-as.data.frame(sapply(stand[,3:81],mean))
    
    sit<-filter(selectdata,activity=="SITTING",subject==i)
    sitmean<-as.data.frame(sapply(sit[,3:81],mean))
    
    lay<-filter(selectdata,activity=="LAYING",subject==i)
    laymean<-as.data.frame(sapply(lay[,3:81],mean))
    
    walk<-filter(selectdata,activity=="WALKING",subject==i)
    walkmean<-as.data.frame(sapply(walk[,3:81],mean))
    
    walk_down<-filter(selectdata,activity=="WALKING_DOWNSTAIRS",subject==i)
    walk_downmean<-as.data.frame(sapply(walk_down[,3:81],mean))
    
    walk_up<-filter(selectdata,activity=="WALKING_UPSTAIRS",subject==i)
    walk_upmean<-as.data.frame(sapply(walk_up[,3:81],mean))
    
    table<-cbind(table,standmean,sitmean,laymean,walkmean,walk_downmean,walk_upmean)
}

##create descriptive column names for each of the computed means
names<-c()
for(i in 1:30) {
    names<-c(names,paste(i,"standing",sep="."))
    names<-c(names,paste(i,"sitting",sep="."))
    names<-c(names,paste(i,"laying",sep="."))
    names<-c(names,paste(i,"walking",sep="."))
    names<-c(names,paste(i,"walking downstairs",sep="."))
    names<-c(names,paste(i,"walking upstairs",sep="."))
}
table<-table[,-1]
names(table)<-names
View(table)
