#define and choose the dataset
AmazonsSaleData1<-read.csv(file="/Users/Sharp_Col/Downloads/AmazonSalesCleaned.csv" ,header=T)
#view the dimensions of the dataset
dim(AmazonsSaleData1)
AmazonsSaleData1$GP.Ratio<-NULL
AmazonsSaleData1$Profit<-NULL
AmazonsSaleData1$Sales<-NULL
AmazonsSaleData1$Ship.Mode<-NULL
AmazonsSaleData1$Quarter<-NULL
AmazonsSaleData1$Postal.Code<-NULL
AmazonsSaleData1$Sub.Category<-NULL
#show the top few rows to see view the data
head (AmazonsSaleData1)
names (AmazonsSaleData1)
set.seed(123)
smp_size <- floor(0.7 * nrow(AmazonsSaleData1))



train_ind <- sample(seq_len(nrow(AmazonsSaleData1)),size=smp_size)
train <- AmazonsSaleData1[train_ind, ]
test <- AmazonsSaleData1[-train_ind, ]
write.csv(train,file="/Users/Sharp_Col/Downloads/TrainDataMining.csv")
write.csv(test,file="/Users/Sharp_Col/Downloads/PredictDataMining.csv")
train <- read.csv(file="/Users/Sharp_Col/Downloads/TrainDataMining.csv",header=T)
test <- read.csv(file="/Users/Sharp_Col/Downloads/PredictDataMining.csv",header=T)
train[1]<-NULL
test[1]<-NULL
dim(train)
dim(test)
train[sapply(train, is.character)] <- lapply(train[sapply(train, is.character)], as.factor)
test[sapply(test, is.character)] <- lapply(test[sapply(test, is.character)],as.factor)
#test$Profitabilty <- as.logical(test$Profitabilty)
str(train)
str(test)




#install.packages("e1071")
library(e1071)
AmazonDataNB <-naiveBayes(Profitabilty ~., data=train)
AmazonDataNB
DataScore <-predict(AmazonDataNB,test,type="raw")
DataScore
summary(DataScore)



Score <- data.frame(DataScore)
colnames(Score) <-c("No", "Yes")
dim(Score)
NBpredict <- ifelse(Score$Yes>0.5 ,1, 0)
dim(NBpredict)





table(NBpredict, test[["Profitabilty"]])
typeof (NBpredict)
#write.csv(DataScore,file="/Users/Sharp_Col/Downloads/DataScore.csv")
#caret; if you encounter a prob
typeof (test[["Profitabilty"]])
#The confusionmatrix function requires factors with the same level
#you can use table() to generate
NB_class<-as.factor(NBpredict)
confusionMatrix(NB_class, as.factor(test[["Profitabilty"]]),positive="1")




