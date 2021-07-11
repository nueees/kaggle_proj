#setwd("C:/Users/Administrator/Documents/R/Kaggle/kaggle_proj/002_HousePrice")
library(knitr);library(ggplot2);library(plyr);library(dplyr);library(corrplot);library(caret);library(gridExtra);library(scales);library(Rmisc);library(ggrepel);library(randomForest);library(psych);library(xgboost);
read.csv("./data/train.csv", stringsAsFactors=F)-> train
read.csv("./data/test.csv", stringsAsFactors=F)-> x_test
str(train) #1460 obs. of  81
str(x_test) #1459 obs. of  80

#넘 많으니 변수 10개만
all <- bind_rows(train,x_test)
str(all[,c(1:10, 81)])
summary(all[,c(1:10, 81)])


# EDA
ggplot(data=all[!is.na(all$SalePrice),],aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks=seq(0,800000, by=100000), labels=comma)

summary(all$SalePrice)

# 문자형들은 전처리 작업이 필요하니 numeric 위주로 상관관계 확인
numericVars <- which(sapply(all, is.numeric))
all_numVar <- all[,numericVars]
str(all_numVar) #38개
cor_all_numVar <- cor(all_numVar, use="pairwise.complete.obs") #상관계수 계산된 애들만 확인
cor_all_numVar[,'SalePrice']
cor_all_numVar['SalePrice',]
cor_all_numVar_desc <- as.matrix(cor_all_numVar['SalePrice',] %>% sort(decreasing=T))
cor_all_numVar_desc[cor_all_numVar_desc[,1]>0.5,] %>% names() -> cor_numVar_desc_high # "SalePrice"~ "OverallQual", "GrLivArea", "GarageCars", "GarageArea", "TotalBsmtSF", "X1stFlrSF", "FullBath", "TotRmsAbvGrd", "YearBuilt", "YearRemodAdd"
cor_all_numVar[cor_numVar_desc_high,cor_numVar_desc_high] %>% corrplot(tl.col="black")









# N/A 전처리




















