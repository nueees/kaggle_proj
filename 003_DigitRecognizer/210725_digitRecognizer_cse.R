#setwd("C:/Users/Administrator/Documents/R/Kaggle/kaggle_proj/003_DigitRecognizer")
library(knitr);library(ggplot2);library(plyr);library(dplyr);library(corrplot);library(caret);library(gridExtra);library(scales);library(Rmisc);library(ggrepel);library(randomForest);library(psych);library(xgboost);library(recipes);library(keras);library(tidyverse);
read.csv("./data/train.csv", stringsAsFactors=F) %>%  mutate(index = "train", .before=1)-> train 
read.csv("./data/test.csv", stringsAsFactors=F) %>%  mutate(index = "test", .before=1) -> x_test

glimpse(train) #42,000 Col 786-1(index) # label, pixel0~783
glimpse(x_test) #28,000 Col 785-1(index) # , pixel0~783
all<-bind_rows(train,x_test) 

# EDA
head(train$label, 1000)
unique(train$label)
# y값 확인
prop.table(table(train$label))*100

train$label <- as.factor(train$label)
ggplot(train, aes(x=label, fill=label))+
  geom_bar(stat="count")

# x값 확인 50개만
head50 <- train[,-c(1:2)] %>% head(50)
#sam50<- train[,-c(1:2)] %>% sample_n(50)

par(mfrow=c(5,10), mar=c(0.1,0.1,0.1,0.1))

class(head50)
class(unlist(head50))
mat <- matrix(unlist(sam50[1,]), nrow=28, byrow=F)
m <- t(apply(mat, 2, rev))
image(m, col=grey.colors(255), axes=F)



