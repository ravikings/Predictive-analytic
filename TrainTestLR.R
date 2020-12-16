#-----------------LOGISTIC REGRESSION -------------------------------------------------------
#define and choose the dataset
AmazonsSaleData1<-read.csv(file="/Users/Sharp_Col/Downloads/AmazonSalesCleaned.csv" ,header=T)
#view the dimensions of the dataset
dim(AmazonsSaleData1)
#AmazonsSaleData1[18]<-NULL
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
test$Profitabilty <- as.factor(test$Profitabilty)
train$Profitabilty <- as.factor(train$Profitabilty)
str(train)
str(test)

#loading required libraries
library("e1071")
library("caret")
#build a logistic regression model using the train set
LRmodel<-glm(train$Profitabilty ~., family = "binomial", train)

#apply the model to the test set; probabilities are generated
LRp<-predict (LRmodel, test, type = "response")

#check the summary of those probabilities
summary(LRp)
names(LRp)
LRp
write.csv(LRp,file="/Users/Sharp_Col/Downloads/LRP.csv")
dim(LRp)

sum(LRp>0.4)
#because the probabilities are quite small, we reduce the threshold to 0.2
LRpredict<-ifelse (LRp > 0.40, 1,0)
LRpredict
dim(LRpredict)
summary(LRpredict)

#generate a simple confusion matrix using the table function
table(LRpredict, test[["Profitabilty"]])
#alternatively we can use confusionmatrix function to get more details.
#check the type of both LRpredict and Target
#The confusion Matrix function
typeof (LRpredict)
#caret; if you encounter a prob
typeof (test[["Profitabilty"]])

#The confusionmatrix function requires factors with the same level
#you can use table() to generate
LRp_class<-as.factor(LRpredict)
#write.csv(LRp_class,file="/Users/Sharp_Col/Downloads/LRp_class.csv")
confusionMatrix(LRp_class, as.factor(test[["Profitabilty"]]),positive="1")   






body(predict.lm)

