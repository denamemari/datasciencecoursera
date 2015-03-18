
#  Part1: a dataset has been created and a working directory to the local directory of Dena has been setup:
setwd ("C:/Users/demari/Documents/R/Run" )
# Part 2: Data has been read and in next section columns names has been created. 
## Read subjects and combine and remove missing data from dataset:
testX <- read.table("test/X_test.txt")
trainX <- read.table("train/X_train.txt")
X <- rbind(testX, trainX)
rm(testX)
rm(trainX)

testSub <- read.table("test/subject_test.txt")
trainSub <- read.table("train/subject_train.txt")
S <- rbind(testSub, trainSub)
rm(testSub)
rm(trainSub)

testY <- read.table("test/y_test.txt")
trainY <- read.table("train/y_train.txt")
Y <- rbind(testY, trainY)
rm(testY)
rm(trainY)

## Read Features List
FL<- read.table("features.txt", stringsAsFactors=FALSE)
# Part 3: Descriptive activity: 
## Use only names from features list and Keep STD and Mean columns only(in COL):
FeatureList <-FL$V2
COL <- grepl("(std|mean[^F])",FeatureList, perl=TRUE)

## get ride of extra data and change the names:
X <- X[, COL]
names(X) <- FeatureList[COL]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

## Read ActivityList:
Act <- read.table("activity_labels.txt")
Act[,2] = gsub("_", "", tolower(as.character(Act[,2])))
Y[,1] = Act[Y[,1], 2]
## Add activity label
names(Y) <- "Activity"  

# Part 4: name data set by descriptive names : 
names(S) <- "subject"
TidyDataSet <- cbind(S, Y, X)
write.table(TidyDataSet, "TidyDataSet.txt")

# Part5:  a tidy data set by average of variables based on each activity and subject has been created: (create new table)
uS = unique(S)[,1]
nS = length(uS)
nA = length(Act[,1])
nC = length(names(TidyDataSet))
td = tidyData[ 1:(nS*nA), ]

row = 1
for (s in 1:nS) {
  for (a in 1:nA) {
    td[row,1] = uS[s]
    td[row,2] = activities[a, 2]
    tmp <- TidyDataSet[TidyDataSet$subject==s & TidyDataSet$Activity==Act[a,2],]
    td[row, 3:nC] <- colMeans(tmp[, 3:nC])
    row = row + 1
  }
}

write.table(td, "TidyDataPart5.txt")
