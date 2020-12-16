
#install.packages("party")
library("party")

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
write.csv(train,file="/Users/Sharp_Col/Downloads/TrainDataMiningDT.csv")
write.csv(test,file="/Users/Sharp_Col/Downloads/PredictDataMiningDT.csv")
train <- read.csv(file="/Users/Sharp_Col/Downloads/TrainDataMiningDT.csv",header=T)
test <- read.csv(file="/Users/Sharp_Col/Downloads/PredictDataMiningDT.csv",header=T)
train[1]<-NULL
test[1]<-NULL
dim(train)
dim(test)
train[sapply(train, is.character)] <- lapply(train[sapply(train, is.character)], as.factor)
test[sapply(test, is.character)] <- lapply(test[sapply(test, is.character)],as.factor)
train$Profitabilty<-as.factor(train$Profitabilty)
#test$Profitabilty <- as.logical(test$Profitabilty)
str(train)
str(test)
attach(train)


DataTree2<-ctree(Profitabilty~ Year+D.Q1+D.Q2+D.Q3+D.Q4+Ship.Time+ Segment+Region+Category+Quantity+Discount,data=train)
DataTree2
plot(DataTree2)
DataScore <- predict(DataTree2,test)
DataScore




dim(DataScore)
table(DataScore, test[["Profitabilty"]])
typeof (DataScore)
#write.csv(DataScore,file="/Users/Sharp_Col/Downloads/DataScore.csv")
#caret; if you encounter a prob
typeof (test[["Profitabilty"]])
#The confusionmatrix function requires factors with the same level
#you can use table() to generate
DTp_class<-as.factor(DataScore)
confusionMatrix(DTp_class, as.factor(test[["Profitabilty"]]),positive="1")












