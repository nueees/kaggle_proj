#setwd("C:/Users/Administrator/Documents/R/Kaggle/kaggle_proj/003_DigitRecognizer")
library(knitr);library(ggplot2);library(plyr);library(dplyr);library(corrplot);library(caret);library(gridExtra);library(scales);library(Rmisc);library(ggrepel);library(randomForest);library(psych);library(xgboost);library(recipes);library(tidyverse);
library(keras);
library(Rcpp);


read.csv("./data/train.csv", stringsAsFactors=F) %>%  mutate(index = "train", .before=1)-> train 
read.csv("./data/test.csv", stringsAsFactors=F) %>%  mutate(index = "test", .before=1) -> test

glimpse(train) #42,000 Col 786-1(index) # label, pixel0~783 = 784(28^28)
glimpse(test) #28,000 Col 785-1(index) # , pixel0~783
all<-bind_rows(train,test) 
glimpse(all)

# EDA
head(train$label, 1000)
unique(train$label)
# y값 확인
prop.table(table(train$label))*100

# train$label <- as.factor(train$label) # 바꾸면 to_categorical 에러남
# all$label <- as.factor(all$label)
ggplot(train, aes(x=label, fill=label))+
  geom_bar(stat="count")

# EDA 
# x값 확인 50개만
head50 <- train[,-c(1:2)] %>% head(50)
#sam50<- train[,-c(1:2)] %>% sample_n(50)

par(mfrow=c(5,10), mar=c(0.1,0.1,0.1,0.1))

head50[2,]
head50[2,(14*28):((15*28)-1)] #가장 가운데 열 확인
sam50[2,(14*28):((15*28)-1)] #가장 가운데 열 확인

head50[2,((14*28)+1):(15*28)]
unlist(head50[2,])[((14*28)+1):(15*28)]
mat <- matrix(unlist(head50[50,]), nrow=28, byrow=F)
image(mat, col=grey.colors(255), axes=F) # 옆으로 누워있음
m <- t(apply(mat, 2, rev))
image(m, col=grey.colors(255), axes=F)
#####################


# 전처리


all %>% filter(index=="train") %>% select(label) -> train_y
all %>% filter(index=="test") %>% select(label) -> test_y

train[,-c(1:2)] -> train_x0
test[,-1] -> test_x0

## 데이터 정규화(2D 배열 --> 1D 배열)
# unlist(head50)
# as.numeric(unlist(head50))
# array(as.numeric(unlist(head50)), dim = c(dim(head50)[[1]], 784))

train_x <- array(as.numeric(unlist(train_x0)), dim = c(dim(train_x0)[[1]], 784))
test_x <- array(as.numeric(unlist(test_x0)), dim = c(dim(test_x0)[[1]], 784))

# RGB 값을 [0,1] 범위로 변환
train_x <- train_x/255
test_x <- test_x/255

## 종속변수 one hot encoding
class(train_y)
# to_categorical(train_y,10)
train_y <- to_categorical(train_y[,1],10)
#test_y  <- to_categorical(test_y, 10)



### 모델링 
install_keras()

model <- keras_model_sequential()

model %>%
  layer_dense(units=256, activation="relu", input_shape=c(784))  %>%
  layer_dropout(rate=0.4) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate=0.4) %>%
  layer_dense(units=10,activation="softmax")

summary(model)


# 컴파일

model %>%
  compile(loss ="categorical_crossentropy",
          optimizer = "adam",
          metrics= c("accuracy"))

## 피팅(학습)
history <- model %>% 
  fit(train_x, train_y, 
      epochs = 10, 
      batch_size = 128,
      callbacks = callback_tensorboard(log_dir = "logs/run_b"),
      validation_split = 0.2) 

history$metrics
# $loss
# [1] 0.57002926 0.24822310 0.18937431 0.16099571 0.13585225 0.12061233 0.10326406
# [8] 0.09557459 0.09071551 0.08393272
# 
# $accuracy
# [1] 0.8239583 0.9261310 0.9436905 0.9512500 0.9581547 0.9629762 0.9676488 0.9706250
# [9] 0.9721131 0.9733036
# 
# $val_loss
# [1] 0.19849733 0.14812204 0.11950421 0.10753379 0.10381225 0.09271869 0.08731139
# [8] 0.08499738 0.08478194 0.09283895
# 
# $val_accuracy
# [1] 0.9404762 0.9554762 0.9636905 0.9673809 0.9673809 0.9720238 0.9752381 0.9735714
# [9] 0.9753571 0.9735714



# 평가
#model %>% evaluate(x_validate, y_validate) -> score.model# train데이터 8:2로 나눈거 validate넣어야함
#print(score.model)

# 실제 예측치
model %>% predict_classes(test_x, batch_size = 128) -> pred.model

# result
test[,-1]
result.model <- bind_cols(ImageId=1:nrow(test),Label=pred.model)

write.csv(result.model, "./data/digitRecognizer_submission1.csv", row.names = FALSE)
# Accuracy 0.97142 /  4724등
result.model %>% head()
# ImageId Label
#    1     2
#    2     0
#    3     9
#    4     9
#    5     3
#    6     7









