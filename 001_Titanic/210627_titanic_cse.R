#getwd()
#setwd("C:/Users/Administrator/Documents/R/Kaggle/kaggle_proj/001_Titanic")
library(dplyr);library(magrittr);library(caret);library(recipes);
rm(list=ls())
read.csv("./data/gender_submission.csv") -> y_test
read.csv("./data/train.csv") %>%  mutate(index = "train")-> train 
read.csv("./data/test.csv")  %>%  mutate(index = "test") -> x_test

str(train)
str(y_test)
str(x_test)
names(train)
names(y_test)
names(x_test)
names(train)<- tolower(names(train))
names(y_test)<- tolower(names(y_test))
names(x_test)<- tolower(names(x_test))

bind_rows(train, x_test) -> full 
names(full)

str(y_test)
str(full)
summary(full)

head(full)

full$survived <- ifelse(full$survived == 1, "Y", "N")
full$survived %<>% as.factor()
y_test$survived <- ifelse(y_test$survived == 1, "Y", "N")
y_test$survived %<>% as.factor()

full$index    %<>%  as.factor()
full$embarked %<>%  as.factor()
full$sex      %<>%  as.factor()
full$pclass <-  factor(full$pclass, levels=c(1,2,3))

summary(full)
full %>% filter(is.na(full$fare)) # 1044 승객
full %>% filter(is.na(full$age)) # 6 승객

str(full) #pclass-> 명목으로, ordered로
recipe(survived~., data=full) %>% 
  step_center(all_numeric_predictors(),-passengerid) %>% 
  step_scale(all_numeric_predictors(),-passengerid) %>% 
  step_BoxCox(all_numeric_predictors(),-passengerid) %>% 
  step_zv(all_numeric_predictors(),-passengerid) %>% 
  step_impute_knn(age,fare) %>% # knn으로 예측..
  #step_impute_linear(fare) %>% #선형회귀로 예측 -> err debug
  #step_dummy(all_nominal_predictors(),-index,-name,-ticket,-cabin) %>% 
  prep() %>% juice() -> full2
summary(full2)

full2 %>% filter(full2$passengerid==1044) %>% select(fare) # 1044 승객
full2 %>% filter(full2$passengerid==6) %>% select(age) # 6 승객

full2 %>% filter(index=="train") %>% select(-index) -> train2
full2 %>% filter(index=="test") %>% select(-index) -> test2
str(train2)
str(test2)
summary(train2)


#trctrl1 <- trainControl(method="cv", summaryFunction=twoClassSummary, classProbs=TRUE)
trctrl2 <- trainControl(method="cv")

set.seed(2106)
#rpart1 <- train(survived~., data=train2, method="rpart", trControl=trctrl1, metric="ROC")
rpart2 <- train(survived~., data=train2[,c(2,4:12)], method="rpart", trControl=trctrl2)
rpart2$results # 0.7957493 
#rpart2$control$number
varImp(rpart2)

rpart2.pred <- predict(rpart2, newdata=test2)
rpart2.pred.res <- ifelse(rpart2.pred == "Y", 1, 0)
result.test2 <- bind_cols(PassengerId=test2$passengerid, Survived=rpart2.pred.res)
#cm.test2 <- confusionMatrix(result.test2$survived, y_test$survived)
write.csv(result.test2, "./data/gender_submission1.csv", row.names = FALSE)




