library(dplyr)
library(tidyverse)
library(ggplot2)
library(randomForest)
library(caTools)
library(gbm)
#Loading the Libraries

getwd()
House<- read.csv('C:/Users/jasdeepa/Documents/Mine/Study/R Course/Test/house-prices-advanced-regression-techniques/train.csv',stringsAsFactors = FALSE)
head(House)
#Reading the data


str(House)
summary(House)
glimpse(House)
#Understanding the structure of the data


House<- House[,-1]
head(House)
MissingData<-colSums(is.na(House))
length(MissingData)
#Understanding Missing data


head(sort(MissingData, decreasing = TRUE),4 )


for (i in 1:80)
{
  if(is.character(House[,i]) == FALSE)
  {
    next
  }
  print(i)
  print(colnames(House)[i])
  print(House %>% group_by(House[,i]) %>% count())
}

#Understanding Missing data in Character Data

House$MasVnrType[is.na(House$MasVnrType)] <- "No alley access" 
House<-House[-which(is.na(House$MasVnrType)),] 
House$BsmtQual[is.na(House$BsmtQual)] <- "None"
House$BsmtCond[is.na(House$BsmtCond)] <- "None"
House$BsmtExposure[is.na(House$BsmtExposure)] <- "None"
House$BsmtFinType1[is.na(House$BsmtFinType1)] <- "None"
House$BsmtFinType2[is.na(House$BsmtFinType2)] <- "None"
House<-House[-which(is.na(House$Electrical)),]
House$FireplaceQu[is.na(House$FireplaceQu)] <- "None"
House$GarageType[is.na(House$GarageType)] <- "None"
House$GarageFinish[is.na(House$GarageFinish)] <- "None"
House$GarageQual[is.na(House$GarageQual)] <- "None"
House$GarageCond[is.na(House$GarageCond)] <- "None"
House$PoolQC[is.na(House$PoolQC)] <- "None"
House$Fence[is.na(House$Fence)] <- "None"
House$MiscFeature[is.na(House$MiscFeature)] <- "None"

#Managing NAs


par(mfrow =c(10,8))

CharCols<-sapply(House,is.character)
CharCols2<-names(which(CharCols == TRUE))

House2<-House

for (j in CharCols2)
{
 House2[,j]<- as.factor(House2[,j])
}

for(j in CharCols2)
{
  print(ggplot(House2, aes_string(j, fill = j)) + geom_bar(alpha=0.5))
}

#Univariate Analysis


MissingData2<-colSums(is.na(House))
MissingData2[which(MissingData2 > 0)]

#Only 2 Numerical Columns have Missing Values


ggplot(House, aes(LotFrontage)) + geom_boxplot(color="blue") +coord_flip() #Right Tailed so lets impute Median
House$LotFrontage[which(is.na(House$LotFrontage) == TRUE)] <- median(House$LotFrontage, na.rm = TRUE)

ggplot(House, aes(GarageYrBlt)) + geom_bar(color="blue") #Right Tailed so lets impute Median
House$GarageYrBlt[which(is.na(House$GarageYrBlt) == TRUE)] <- median(House$GarageYrBlt, na.rm = TRUE)

House3<-House

for (j in CharCols2)
{
  House3[,j]<- as.factor(House3[,j])
}

str(House3)

House3$MoSold <-as.factor(House3$MoSold)

House3$GarageYrBlt[which(House3$GarageCond == "None" & House3$GarageYrBlt > 0)] <-0

House3$TotalBath <- House3$FullBath + 1.5*House3$HalfBath
House3$TotalBsmtBath <- House3$BsmtFullBath + 1.5*House3$BsmtHalfBath
House3$FullBath <- NULL
House3[,"HalfBath"] <- NULL
House3[,"BsmtFullBath"] <- NULL
House3[,"BsmtHalfBath"] <- NULL
str(House3)
House3$MSSubClass   <- as.factor(House3$MSSubClass)

samples<-sample.split(House3$SalePrice, SplitRatio = 0.7)
train<- House3[samples == TRUE,]
test<- House3[samples ==FALSE,]

Mod4<- gbm(SalePrice~., data = train, n.trees = 5000, shrinkage = 0.01, cv.folds = 4 )


plot(Mod4)

varImp(Mod4,num_trees = 5000)
summary.gbm(Mod4)

CV_RSq <- (cor(pred, test$SalePrice))^2

test<- read.csv('C:/Users/jasdeepa/Documents/Mine/Study/R Course/Test/house-prices-advanced-regression-techniques/test.csv',stringsAsFactors = FALSE)
test$MSSubClass   <- as.factor(test$MSSubClass)
test$MoSold   <- as.factor(test$MoSold)
str(test)
test<-test[-1]

test$TotalBath <- test$FullBath + 1.5*test$HalfBath
test$TotalBsmtBath <- test$BsmtFullBath + 1.5*test$BsmtHalfBath
test$FullBath <- NULL
test[,"HalfBath"] <- NULL
test[,"BsmtFullBath"] <- NULL
test[,"BsmtHalfBath"] <- NULL


test$MasVnrType[is.na(test$MasVnrType)] <- "None" 
test<-House[-which(is.na(test$MasVnrType)),] 
test$BsmtQual[is.na(test$BsmtQual)] <- "None"
test$BsmtCond[is.na(test$BsmtCond)] <- "None"
test$BsmtExposure[is.na(test$BsmtExposure)] <- "None"
test$BsmtFinType1[is.na(test$BsmtFinType1)] <- "None"
test$BsmtFinType2[is.na(test$BsmtFinType2)] <- "None"
test<-House[-which(is.na(test$Electrical)),]
test$FireplaceQu[is.na(test$FireplaceQu)] <- "None"
test$GarageType[is.na(test$GarageType)] <- "None"
test$GarageFinish[is.na(test$GarageFinish)] <- "None"
test$GarageQual[is.na(test$GarageQual)] <- "None"
test$GarageCond[is.na(test$GarageCond)] <- "None"
test$PoolQC[is.na(test$PoolQC)] <- "None"
test$Fence[is.na(test$Fence)] <- "None"
test$MiscFeature[is.na(test$MiscFeature)] <- "None"
test$Alley[is.na(test$Alley)] <- "None"
test$LotFrontage[which(is.na(test$LotFrontage) == TRUE)] <- median(test$LotFrontage, na.rm = TRUE)

sort(colSums(is.na(test)),decreasing = TRUE)

Charcol<- sapply(test, is.character)
CharCols3<-names(which(CharCols == TRUE))



str(test)
str(train)


Charcol<- sapply(train, is.character)
CharCols2<-names(which(CharCols == TRUE))

for (j in CharCols3)
{
  test[,j]<- as.factor(test[,j])
}


pred1<-predict(Mod4, test)

test2<- read.csv('C:/Users/jasdeepa/Documents/Mine/Study/R Course/Test/house-prices-advanced-regression-techniques/test.csv',stringsAsFactors = FALSE)

test2$Id
test3<- cbind(test2$id,pred1)
test4<- write.csv(test3, "output.csv")
