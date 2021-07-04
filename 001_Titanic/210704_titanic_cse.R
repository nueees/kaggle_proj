#setwd("C:/Users/Administrator/Documents/R/Kaggle/001_Titanic")
library(dplyr);library(magrittr);library(caret);library(recipes);library(mice);
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

full$name %>% head()
full_bak <- full
#full <- full_bak


# title 남여에서 좀 더 세분화된 단위 파악 -> 정확도 더떨어짐
full <- full %>% mutate(title=gsub('(.*, )|(\\..*)', '', full$name))
table(full$sex,full$title)
s_title <- full %>%  group_by(title) %>% summarise(n=n()) %>% arrange(-n)
rare_title <- s_title$title[s_title$n<10]

full$title[full$title=='Mlle'] <- 'Miss'
full$title[full$title=='Ms'] <- 'Miss'
full$title[full$title=='Mme'] <- 'Mrs'
full$title[full$title %in% rare_title] <- 'rare title'
full$title %<>% as.factor()

# strsplit("Braund, Mr. Owen Harris",'(.*, )')  #"", "Mr. Owen Harris"
# strsplit("Braund, Mr. Owen Harris",'(., )')   #"Braun", "Mr. Owen Harris"
# strsplit("Braund, Mr. Owen. Harris",'(\\..*)') #"Braund, Mr"
# strsplit("Braund, Mr. Owen. Harris",'(\\..)')  #"Braund, Mr", "Owen Harris"

# surname 성 추출 가족단위 파악 -> 정확도 더떨어짐
sapply(full$name, function(x) strsplit(x,'[,.]')[[1]][1]) %>% as.data.frame() %>% bind_cols(full) -> full.t
names(full.t)[1] <- 'surname'
full <- full.t
full %>%  group_by(surname) %>% summarise(n=n()) %>% arrange(-n)
full$surname %<>% as.factor()

# 가족 단위 파악
full$fsize <- full$sibsp + full$parch + 1
full$family <- paste(full$surname, full$fsize, sep='_')


summary(full)
#help(step_impute_linear)
recipe(survived~., data=full) %>% 
  step_center(all_numeric_predictors(),-passengerid) %>% 
  step_scale(all_numeric_predictors(),-passengerid) %>% 
  step_BoxCox(all_numeric_predictors(),-passengerid) %>% 
  step_zv(all_numeric_predictors(),-passengerid) %>% 
  step_impute_linear(age, impute_with = imp_vars(pclass,sex,sibsp,parch,embarked,title)) %>%
  step_impute_linear(fare, impute_with = imp_vars(pclass,sex,sibsp,parch,embarked,title)) %>%
  #step_dummy(all_nominal_predictors(),-index,-name,-ticket,-cabin) %>% 
  prep() %>% juice() -> full2
summary(full2)
#full2 %>% group_by(age) %>% summarise(n=n())

# 선형회귀로 결측치처리 효과 없음
## 결측치 처리 mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 


full2 %>% filter(index=="train") %>% select(-index) -> train2
full2 %>% filter(index=="test") %>% select(-index) -> test2
str(train2)
str(test2)
summary(train2)

#####################################rpart######################################

#trctrl1 <- trainControl(method="cv", summaryFunction=twoClassSummary, classProbs=TRUE)
trctrl2 <- trainControl(method="cv")

set.seed(2106)
#rpart1 <- train(survived~., data=train2, method="rpart", trControl=trctrl1, metric="ROC")
rpart2 <- train(survived~., data=train2[,!names(train2) %in% c("passengerid","name")], method="rpart", trControl=trctrl2)
rpart2$results
rpart2$control$number

rpart2.pred <- predict(rpart2, newdata=test2)
rpart2.pred <- ifelse(rpart2.pred=="Y",1,0)
result.test2 <- bind_cols(passengerid=test2$passengerid, survived=rpart2.pred)
write.csv(result.test2, "./data/gender_submission2.csv", row.names = FALSE)
# 0.77990  0.77751
result.test2 %>% group_by(survived) %>% summarise(n=n())
########################################gbm#####################################

set.seed(2106)
gbm1 <- train(survived~., data=train2[,!names(train2) %in% c("passengerid","name")], method="gbm", trControl=trctrl2)
gbm1.pred <- predict(gbm1, newdata=test2)
gbm1.pred <- ifelse(gbm1.pred=="Y",1,0)
result.test3 <- bind_cols(passengerid=test2$passengerid, survived=gbm1.pred)
write.csv(result.test3, "./data/gender_submission3.csv", row.names = FALSE)
# 0.77990 0.77511

#############################################################################



#bind_cols(rpart2=rpart2.pred, gbm1=gbm1.pred) %>% View()

# 
# result.test1 <- bind_cols(passengerid=test2$passengerid, survived=rpart2.pred)
# #str(y_test)
# cm.test2 <- confusionMatrix(result.test1$survived, y_test$survived)
