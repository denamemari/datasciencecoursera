#  Part1: a dataset has been created and a working directory to the local directory of Dena has been setup:
Setwd (`C: /Users/Dena/Desktop/Documents/R/test` )
# Data has been read and in next section columns names has been created: 
trainX<-read.table('./train/x_train.txt',header=FALSE)
trainY<-read.table('./train/y_train.txt',header=FALSE)
Feat<-read.table('./features.txt',header=FALSE)
ActType<-read.table('./activity_labels.txt',header=FALSE)
SubTrain<-read.table('./train/subject_train.txt',header=FALSE)
ColumnName(ActType)<-c('ActID','ActType');
ColumnName(SubTrain)<-"SubID";
ColumnName(trainX)<-Feat[,2]; 
ColumnName(trainY)<-"ActID";
#Merge trainY, trainX and SubTrain:
TrainData <-cbind(trainY,trainX,SubTrain);
# Test data has been read:
SubTest <-read.table('./test/subject_test.txt',header=FALSE);
testX<-read.table('./test/x_test.txt',header=FALSE); 
testY<-read.table('./test/y_test.txt',header=FALSE); 
# Create Test data column Name: 
ColumnName(SubTest)<-"SubID";
ColumnName(testX)<- Feat[,2]; 
ColumnName(testY)<-"ActID";
# Merge testX, testY and SubTest : 
DataTest <-cbind( testX,testY,SubTest)
# Merge DataTest and DataTeraining and call it FinalData( final data set after reading and merging all Training and Test data):
FinalData<-rbind(TrainData,DataTest);
ColumnName<-ColumnName(FinalData);
# Part 2: Claculate mean and standard deviation for FinalData: 
logVect<-(grepl("Act..",ColumnNames) | grepl("Sub..",ColumnName) | grepl("-mean..",ColumnName) & !grepl("-meanFreq..",ColumnName) & !grepl("mean..-",ColumnName) | grepl("-std..",ColumnName) & !grepl("-std()..-",ColumnName));
# Create FinalData1 as a subset for FinalData 
FinalData1<-FinalData[logVec==TRUE]
# Part 3: Descriptive activity: 
FinalData1<- merge(FinalData1,ActType,by='ActID',all.x=TRUE);
ColumnName<-ColumnName(FinalData1); 
# Part 4: name data set by descriptive names : 
for (i in 1:length(ColumnName)) 
{
ColumnName[i]<-gsub("\\()","",ColumnName[i])
ColumnName[i]<-gsub("-std$","StdDev",ColumnName[i])
ColumnName[i]<-gsub("-mean","Mean",ColumnName[i])
ColumnName[i]<-gsub("^(t)","time",ColumnName[i])
ColumnName[i]<-gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",ColumnName[i])
ColumnName[i]<-gsub("[Gg]yro","Gyro",ColumnName[i])
ColumnName[i]<-gsub("AccMag","AccMagnitude",ColumnName[i])
ColumnName[i]<-gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",ColumnName[i])
ColumnName[i]<-gsub("JerkMag","JerkMagnitude",ColumnName[i])
ColumnName[i]<-gsub("GyroMag","GyroMagnitude",ColumnName[i])
ColumnName[i]<-gsub("^(f)","freq",ColumnName[i])
ColumnName[i]<-gsub("([Gg]ravity)","Gravity",ColumnName[i])
};
ColumnName(FinalData1)<-ColumnName;
# Part5:  a tidy data set by average of variables based on each activity and subject has been created: (create new table)
FinalDataActType<- FinalData1 [, names (FinalData1)! = 'ActType']
FinalTidyData<-aggregate(FinalDataActType[,names(FinalDataActType) != c('ActID','SubID')],by=list(ActID=FinalDataActyType$ActID,SubID= FinalDataActType$SubID),mean); # summarizing the new table.
FinalTidyData=merge (FinalTidyData,ActTypr,by='ActID',all.x=TRUE)
# Export the FinalTidyData: 
write.table(FinalTidayData, './FinalTidyData.txt',row.names=TRUE,sep='\t')
