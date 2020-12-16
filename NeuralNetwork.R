#----------------------------NEURAL NETWORK--------------------------------------------
AmazonsSaleData1<-read.csv(file="/Users/Sharp_Col/Downloads/AmazonSalesCleaned.csv" ,header=T)
AmazonsSaleData1$GP.Ratio<-NULL
AmazonsSaleData1$Profit<-NULL
AmazonsSaleData1$Sales<-NULL
AmazonsSaleData1$Ship.Mode<-NULL
AmazonsSaleData1$Quarter<-NULL
AmazonsSaleData1$Postal.Code<-NULL
AmazonsSaleData1$Sub.Category<-NULL
names (AmazonsSaleData1)
set.seed(123)
smp_size <- floor(0.7 * nrow(AmazonsSaleData1))
train_ind <- sample(seq_len(nrow(AmazonsSaleData1)),size=smp_size)
train <- AmazonsSaleData1[train_ind, ]
test <- AmazonsSaleData1[-train_ind, ]
write.csv(train,file="/Users/Sharp_Col/Downloads/TrainDataMiningNN.csv")
write.csv(test,file="/Users/Sharp_Col/Downloads/PredictDataMiningNN.csv")
Amazontrain <- read.csv(file="/Users/Sharp_Col/Downloads/TrainDataMiningNN.csv",header=T)
Amazonpredict <- read.csv(file="/Users/Sharp_Col/Downloads/PredictDataMiningNN.csv",header=T)
Amazontrain[1]<-NULL
Amazonpredict[1]<-NULL
Amazontrain[sapply(Amazontrain, is.character)] <- lapply(Amazontrain[sapply(Amazontrain, is.character)], as.factor)
Amazonpredict[sapply(Amazonpredict, is.character)] <- lapply(Amazonpredict[sapply(Amazonpredict, is.character)],as.factor)
#Amazonpredict$Profitabilty = as.logical(Amazonpredict$Profitabilty)
#Amazontrain$Profitabilty = as.factor(Amazontrain$Profitabilty)
dim(Amazontrain)
dim(Amazonpredict)
str(Amazontrain)
str(Amazonpredict)
#install.packages("nnet")
library(nnet)
set.seed(1000)
NN <- nnet(Profitabilty ~ ., data=Amazontrain, size=8, maxit=10000)
library(NeuralNetTools)
plotnet(NN)
R_NNP <- predict(NN,Amazonpredict)
R_NN <- as.factor(ifelse(R_NNP<0.6,1,0))
summary(R_NN)

table(R_NN, test[["Profitabilty"]])
typeof (R_NN)
typeof (test[["Profitabilty"]])
#NN_class<-as.factor(R_NN)

confusionMatrix(R_NN, as.factor(test[["Profitabilty"]]),positive="1")   




