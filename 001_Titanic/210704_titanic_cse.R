setwd("C:/Users/Administrator/Documents/R/Kaggle/kaggle_proj/001_Titanic")
library(dplyr);library(magrittr);library(caret);library(recipes);library(mice);
rm(list=ls())
read.csv("./data/gender_submission.csv") -> y_test
read.csv("./data/train.csv") %>%  mutate(index = "train")-> train 
read.csv("./data/test.csv")  %>%  mutate(index = "test") -> x_test

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
full$pclass <- factor(full$pclass, levels=c(1,2,3))

full$name %>% head()
full_bak <- full
#full <- full_bak


# ticket 전처리
str(full$ticket)
# ticket1 <- full %>% group_by(ticket) %>% select() 
# full %>% select(ticket) %>% unique() %>% count()
ticket.unique <- rep(0, nrow(full))
tickets <- unique(full$ticket)
length(full$ticket)
length(unique(full$ticket))
for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(full$Ticket == current.ticket)
  for (k in 1:length(party.indexes)) {
    ticket.unique[party.indexes[k]] <- length(party.indexes) 
  }
}
full$ticket.unique <- ticket.unique
full$ticket.size[full$ticket.unique == 1]   <- 'Single'
full$ticket.size[full$ticket.unique < 5 & full$ticket.unique>= 2]   <- 'Small'
full$ticket.size[full$ticket.unique >= 5]   <- 'Big'

str(full)


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

full <- full %>% select(-sex) #타이틀 넣고 sex제외외
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
#full$family <- paste(full$surname, full$fsize, sep='_')

# NA 파악
sapply(names(full), function(n) { sum(is.na(full.na[, n])) })
# survived 제외, age 263 fare 1 

summary(full)
#help(step_impute_linear)
recipe(survived~., data=full) %>% 
  step_center(all_numeric_predictors(),-passengerid) %>% 
  step_scale(all_numeric_predictors(),-passengerid) %>% 
  step_BoxCox(all_numeric_predictors(),-passengerid) %>% 
  step_zv(all_numeric_predictors(),-passengerid) %>% 
  step_impute_linear(age, impute_with = imp_vars(pclass,sibsp,parch,embarked,title)) %>%
  step_impute_linear(fare, impute_with = imp_vars(pclass,sibsp,parch,embarked,title)) %>%
  #step_dummy(all_nominal_predictors(),-index,-name,-ticket,-cabin) %>% 
  prep() %>% juice() -> full2
summary(full2)
#full2 %>% group_by(age) %>% summarise(n=n())

# 선형회귀로 결측치처리 효과 없음
## 결측치 처리 mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 


full2 %>% filter(index=="train") %>% select(-index) -> train2
full2 %>% filter(index=="test") %>% select(-index) -> test2

# recipe로 전처리안한 데이터셋
full %>% filter(index=="train") %>% select(-index) -> train0
full %>% filter(index=="test") %>% select(-index) -> test0

train0 %>% 
  group_by(survived, sex, pclass, title) %>%
  summarise(N = n()) %>% 
  ungroup 


#####################################rpart######################################

#trctrl1 <- trainControl(method="cv", summaryFunction=twoClassSummary, classProbs=TRUE)
trctrl2 <- trainControl(method="cv")

set.seed(2106)
#rpart1 <- train(survived~., data=train2, method="rpart", trControl=trctrl1, metric="ROC")
rpart2 <- train(survived~., data=train2[,!names(train2) %in% c("passengerid","name")], method="rpart", trControl=trctrl2)
print(rpart2)
rpart2$finalModel

rpart2.pred <- predict(rpart2, newdata=test2)
rpart2.pred <- ifelse(rpart2.pred=="Y",1,0)
result.test2 <- bind_cols(passengerid=test2$passengerid, survived=rpart2.pred)
write.csv(result.test2, "./data/gender_submission2.csv", row.names = FALSE)
# 0.77990  0.77751
result.test2 %>% group_by(survived) %>% summarise(n=n())
# 0 254 1 164

library(rpart)
str(train0)
train00<-train0[!is.na(train0),]
rpart3 <- rpart(survived~., data=train00[,!names(train00) %in% c("passengerid","name")], method="class")
rpart3$variable.importance
# ticket, surname, title, cabin, fare, pclass, fsize, age, parch, sibsp
rpart3$splits
print(rpart3)
rpart.plot::rpart.plot(rpart3, extra=3, fallen.leaves=T)
rpart3.pred <- predic(rpart3, data=test2, type="class")
confusionMatrix(rpart3.pred,train2$survived)


########################################gbm#####################################

set.seed(2106)
gbm1 <- train(survived~., data=train2[,!names(train2) %in% c("passengerid","name")], method="gbm", trControl=trctrl2)
gbm1.pred <- predict(gbm1, newdata=test2)
gbm1.pred <- ifelse(gbm1.pred=="Y",1,0)
result.test3 <- bind_cols(passengerid=test2$passengerid, survived=gbm1.pred)
write.csv(result.test3, "./data/gender_submission3.csv", row.names = FALSE)
# 0.77990 0.77511
result.test3 %>% group_by(survived) %>% summarise(n=n())
# 0 269 1 149


#############################################################################



#bind_cols(rpart2=rpart2.pred, gbm1=gbm1.pred) %>% View()

# 
# result.test1 <- bind_cols(passengerid=test2$passengerid, survived=rpart2.pred)
# #str(y_test)
# cm.test2 <- confusionMatrix(result.test1$survived, y_test$survived)
