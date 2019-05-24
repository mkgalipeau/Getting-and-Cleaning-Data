# Getting-and-Cleaning-Data
## Read test data. I create three different files that read each of the three documents in the test folder
    xtest<-read.delim("./UCI HAR Dataset/test/X_test.txt", header=FALSE,sep="",quote="")
    ytest<-read.delim("./UCI HAR Dataset/test/y_test.txt", header=FALSE,sep="",quote="")
    stest<-read.delim("./UCI HAR Dataset/test/subject_test.txt", header=FALSE,sep="",quote="")
## Read train data. I then use the same functions to read each of the three documents in the train folder
    xtrain<-read.delim("./UCI HAR Dataset/train/X_train.txt", header=FALSE,sep="",quote="")
    ytrain<-read.delim("./UCI HAR Dataset/train/y_train.txt", header=FALSE,sep="",quote="")
    strain<-read.delim("./UCI HAR Dataset/train/subject_train.txt", header=FALSE,sep="",quote="")
## Create one complete data set
### I first create one data set for each folder by combining the columns of the train and test data respectively. Then I bind the rows of the two data sets.

    train<-cbind(strain,ytrain,xtrain)
    test<-cbind(stest,ytest,xtest)
    data<-rbind(train,test)

## Read Column Names and apply to data set
### This creates the column names for the data set. The column names start with the Subject(1-30), then activity (1-6), then the x,y,z data names read from the file.

    col<-read.delim("UCI HAR Dataset/features.txt", header=FALSE,sep="",quote="",colClasses="character")
    colNames<-c("subject","activity",col$V2)
    names(data)<-colNames

## Choose only mean and standard deviation
### This part of the code searches and finds all columns that contain "mean" and "std" in their names. "selectdata" data frame is created with only the columns that contain these words in their names.\

    mean<-data[,grepl("mean",names(data))]
    std<-data[,grepl("std",names(data))]
    selectdata<-cbind(data[,c(1,2)],mean,std)

## Rewrite activity names
### This first reads and extracts just a vector of the activity names in order. It then replaces all 1's in the data set with "WALKING", all 2's in the data set with "WALKING_UPSTAIRS", etc. 

    act<-read.delim("./UCI HAR Dataset/activity_labels.txt", header=FALSE,sep="",quote="",colClasses="character")
    act<-act$V2
    for (i in 1:6) {
        selectdata$activity<-gsub(i,act[i],selectdata$activity)
    }
    View(selectdata)

## Make new table
### I first create an empty data frame in order for each iteration to bind columns from the previous iteration.

    table<-data.frame(rep(0,79))

### Create the means for each subject based on activity. Each iteration considers one subject at a time, filters just one activity, and then takes the column means as data frames. Then at the end it will add 6 more columns to the table for each activity.
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

### Create descriptive column names for each of the computed means. Examples of column names that will be created from this: 1.standing, 5.laying, 10.walking upstairs.
    names<-c()
    for(i in 1:30) {
        names<-c(names,paste(i,"standing",sep="."))
        names<-c(names,paste(i,"sitting",sep="."))
        names<-c(names,paste(i,"laying",sep="."))
        names<-c(names,paste(i,"walking",sep="."))
        names<-c(names,paste(i,"walking downstairs",sep="."))
        names<-c(names,paste(i,"walking upstairs",sep="."))
    }
### This will remove the extra column that was created when inititializing the first for loop, then assign the column names, and finally display the table. 
    table<-table[,-1]
    names(table)<-names
    View(table)
