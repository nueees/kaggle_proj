#setwd("C:/Users/Administrator/Documents/R/Kaggle/kaggle_proj/002_HousePrice")
library(knitr);library(ggplot2);library(plyr);library(dplyr);library(corrplot);library(caret);library(gridExtra);library(scales);library(Rmisc);library(ggrepel);library(randomForest);library(psych);library(xgboost);
read.csv("./data/train.csv", stringsAsFactors=F) %>%  mutate(index = "train")-> train 
read.csv("./data/test.csv", stringsAsFactors=F) %>%  mutate(index = "test") -> x_test
str(train) #1460 obs. of  81
str(x_test) #1459 obs. of  80

all<-bind_rows(train,x_test)

# EDA
ggplot(data=all[!is.na(all$SalePrice),],aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks=seq(0,800000, by=100000), labels=comma)

summary(all$SalePrice)

# 문자형들은 전처리 작업이 필요하니 numeric 먼저 상관관계 확인
numericVars <- which(sapply(all, is.numeric))
all_numVar <- all[,numericVars]
str(all_numVar) #38개-1개(Id)
cor_all_numVar <- cor(all_numVar[,2:38], use="pairwise.complete.obs") #상관계수 계산된 애들만 확인
# cor_all_numVar[,'SalePrice']
# cor_all_numVar['SalePrice',]
cor_all_numVar_desc <- as.matrix(cor_all_numVar['SalePrice',] %>% sort(decreasing=T))

cor_all_numVar_desc[cor_all_numVar_desc[,1]>0.5,] %>% names() -> cor_numVar_desc_high 
cor_all_numVar[cor_numVar_desc_high,cor_numVar_desc_high] %>% corrplot(tl.col="black")
corrplot.mixed(cor_all_numVar[cor_numVar_desc_high,cor_numVar_desc_high],
               tl.col="black",
               tl.pos="lt")
# "SalePrice"~ "OverallQual", "GrLivArea", "GarageCars", "GarageArea", "TotalBsmtSF", "X1stFlrSF", "FullBath", "TotRmsAbvGrd", "YearBuilt", "YearRemodAdd" 상관관계 높은 순




# 1번 OverallQual: Rates the overall material and finish of the house
# 10	Very Excellent
# 9	Excellent
# 8	Very Good
# 7	Good
# 6	Above Average
# 5	Average
# 4	Below Average
# 3	Fair
# 2	Poor
# 1	Very Poor
str(all$OverallQual) (1~10점)
all$OverallQual <- as.factor(all$OverallQual)

ggplot(data=all[!is.na(all$SalePrice),], aes(x=OverallQual, y=SalePrice))+
  geom_boxplot()
# 전반적인 양의 상관관계



# 2번 GrLivArea: Above grade (ground) living area square feet (334~5642)
str(all$GrLivArea)
summary(all$GrLivArea)
boxplot(all$GrLivArea)
ggplot(data=all[!is.na(all$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_text_repel(aes(label=ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500,rownames(all),'')))
all[c(524,1299),c("SalePrice","GrLivArea","OverallQual")]
# 전반적인 양의 상관관계


# 3번 GarageCars: Size of garage in car capacity
summary(all$GarageCars)
ggplot(data=all[!is.na(all$SalePrice),], aes(x=as.factor(GarageCars)))+
  geom_histogram(stat="count")






# 문자형 상관관계 확인
nominalVars <- which(sapply(all, is.character))
all_nomVar <- all[,nominalVars]
str(all_nomVar)
length(all_nomVar) # 44개-1개(index) nominal변수
all_nomVar %>% names()

# N/A 전처리
sapply(names(all), function(n) { sum(is.na(all[, n])) }) %>% sort(decreasing=T)
# naVars <- which(colSums(is.na(all))>0)
# all_naVar <- all[,naVars]
# colSums(is.na(all_naVar)) %>% sort(decreasing=T) 
# PoolQC, MiscFeature, Alley, Fence, SalePrice... 순인데 SalePrice 빼야함


# 4번 PoolQC: Pool quality
# Ex	Excellent
# Gd	Good
# TA	Average/Typical
# Fa	Fair
# NA	No Pool

summary(factor(all$PoolQC)) # NA->No Pool
all$PoolQC[is.na(all$PoolQC)] <- "None"
# 다른애들도 품질수준 동일하게 쓰므로 
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
all$PoolQC <- (revalue(all$PoolQC, Qualities))
table(all$PoolQC)



# 5번 MiscFeature: Miscellaneous feature not covered in other categories
# Elev	Elevator
# Gar2	2nd Garage (if not described in garage section)
# Othr	Other
# Shed	Shed (over 100 SF)
# TenC	Tennis Court
# NA	None
summary(factor(all$MiscFeature)) # NA -> 기타옵션없음
all$MiscFeature[is.na(all$MiscFeature)] <- "None"
table(all$MiscFeature)
all$MiscFeature <- as.factor(all$MiscFeature)

# 6번 Alley: Type of alley access to property
# Grvl	Gravel
# Pave	Paved
# NA 	No alley access
summary(factor(all$Alley)) # NA -> 입구 자갈이나 포장X.
all$Alley[is.na(all$Alley)] <- "None"
table(all$Alley)
all$Alley <- as.factor(all$Alley)


# 7번 Fence: Fence quality
# GdPrv	Good Privacy
# MnPrv	Minimum Privacy
# GdWo	Good Wood
# MnWw	Minimum Wood/Wire
# NA	No Fence
summary(factor(all$Fence)) # NA -> 펜스 없음
all$Fence[is.na(all$Fence)] <- "None"
table(all$Fence)
all[!is.na(all$SalePrice),] %>% 
  group_by(Fence) %>%  #Fence 그룹핑
  summarise(median = median(SalePrice), counts = n()) # fence없는애들이 비쌈
all$Fence <- factor(all$Fence)


# 8번 FireplaceQu: Fireplace quality
# Ex	Excellent - Exceptional Masonry Fireplace
# Gd	Good - Masonry Fireplace in main level
# TA	Average - Prefabricated Fireplace in main living area or Masonry Fireplace in basement
# Fa	Fair - Prefabricated Fireplace in basement
# Po	Poor - Ben Franklin Stove
# NA	No Fireplace
summary(factor(all$FireplaceQu)) # NA -> 소방시설없음
all$FireplaceQu[is.na(all$FireplaceQu)] <- "None"
all$FireplaceQu <- (revalue(all$FireplaceQu, Qualities))
table(all$FireplaceQu)

# 9번 LotFrontage: Linear feet of street connected to property
summary(as.integer(all$LotFrontage)) # NA -> imputation 필요할 거 같다..
# all_LotFrontage_rmna <-all[!is.na(all$LotFrontage),c("LotFrontage","SalePrice")]
# cor(all_LotFrontage_rmna, use="pairwise.complete.obs") # 0.35  엄청 높진 않음.
all$LotFrontage <- as.integer(all$LotFrontage)
# recipe할때 전처리 step_impute_median(LotFrontage) 할 예정 # median : 68


# 10번 GarageYrBlt: Year garage was built
summary(all$GarageYrBlt) # NA -> YearBuilt로 imputation
summary(all$YearBuilt)
all$GarageYrBlt[is.na(all$GarageYrBlt)]<-all$YearBuilt[is.na(all$GarageYrBlt)]

# 11번 GarageFinish: Interior finish of the garage
# Fin	Finished
# RFn	Rough Finished	
# Unf	Unfinished
# NA	No Garage
summary(factor(all$GarageFinish)) # NA -> 차고 X
all$GarageFinish[is.na(all$GarageFinish)] <- "None"
all$GarageFinish <- as.integer(revalue(all$GarageFinish, c('None' = 0, 'Unf' = 1, 'RFn' = 2, 'Fin' = 3)))
table(all$GarageFinish)

# 12번 GarageQual: Garage quality
# Ex	Excellent
# Gd	Good
# TA	Typical/Average
# Fa	Fair
# Po	Poor
# NA	No Garage
summary(factor(all$GarageQual)) # NA -> 차고 X
all$GarageQual[is.na(all$GarageQual)] <- "None"
all$GarageQual <- as.integer(revalue(all$GarageQual, Qualities))
table(all$GarageQual)

# 13번 GarageCond: Garage condition
# Ex	Excellent
# Gd	Good
# TA	Typical/Average
# Fa	Fair
# Po	Poor
# NA	No Garage
summary(factor(all$GarageCond)) # NA -> 차고 X
all$GarageCond[is.na(all$GarageCond)] <- "None"
all$GarageCond <- as.integer(revalue(all$GarageCond, Qualities))
table(all$GarageCond)

# 14번 GarageType: Garage location
# 2Types	More than one type of garage
# Attchd	Attached to home
# Basment	Basement Garage
# BuiltIn	Built-In (Garage part of house - typically has room above garage)
# CarPort	Car Port
# Detchd	Detached from home
# NA	No Garage
summary(factor(all$GarageType)) # NA -> 차고 X
all$GarageType[is.na(all$GarageType)] <- "None"
all$GarageType <- as.integer(revalue(all$GarageType, c('None' = 0, 'Detchd' = 1, 'CarPort' = 2, 'BuiltIn' = 3, 'Basment' = 4, 'Attchd' = 5, '2Types' = 6)))
table(all$GarageType)
#all$GarageType <- all_bak$GarageType


# 15번 BsmtCond: Evaluates the general condition of the basement
# Ex	Excellent
# Gd	Good
# TA	Typical - slight dampness allowed
# Fa	Fair - dampness or some cracking or settling
# Po	Poor - Severe cracking, settling, or wetness
# NA	No Basement
summary(factor(all$BsmtCond)) # NA -> 지하 X
all$BsmtCond[is.na(all$BsmtCond)] <- "None"
all$BsmtCond <- as.integer(revalue(all$BsmtCond, Qualities))
table(all$BsmtCond)

# 16번 BsmtExposure: Refers to walkout or garden level walls
# Gd	Good Exposure
# Av	Average Exposure (split levels or foyers typically score average or above)	
# Mn	Mimimum Exposure
# No	No Exposure
# NA	No Basement
summary(factor(all$BsmtExposure)) # NA -> 지하 X
all$BsmtExposure[is.na(all$BsmtExposure)] <- "None"
all$BsmtExposure <- as.integer(revalue(all$BsmtExposure, c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)))
table(all$BsmtExposure)

# 17번 BsmtQual: Evaluates the height of the basement
# Ex	Excellent (100+ inches)	
# Gd	Good (90-99 inches)
# TA	Typical (80-89 inches)
# Fa	Fair (70-79 inches)
# Po	Poor (<70 inches
# NA	No Basement
summary(factor(all$BsmtQual)) # NA -> 지하 X
all$BsmtQual[is.na(all$BsmtQual)] <- "None"
all$BsmtQual <- as.integer(revalue(all$BsmtQual, Qualities))
table(all$BsmtQual)

# 18번 BsmtFinType2: Rating of basement finished area (if multiple types)
# GLQ	Good Living Quarters
# ALQ	Average Living Quarters
# BLQ	Below Average Living Quarters	
# Rec	Average Rec Room
# LwQ	Low Quality
# Unf	Unfinshed
# NA	No Basement
summary(factor(all$BsmtFinType2)) # NA -> 지하 X
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- "None"
all$BsmtFinType2 <- as.integer(revalue(all$BsmtFinType2, c('None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)))
table(all$BsmtFinType2)


# 19번 BsmtFinType1 : Rating of basement finished area
# GLQ	Good Living Quarters
# ALQ	Average Living Quarters
# BLQ	Below Average Living Quarters	
# Rec	Average Rec Room
# LwQ	Low Quality
# Unf	Unfinshed
# NA	No Basement
summary(factor(all$BsmtFinType1)) # NA -> 지하 X
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- "None"
all$BsmtFinType1 <- as.integer(revalue(all$BsmtFinType1, c('None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)))
table(all$BsmtFinType1)


# 20번 MasVnrArea: Masonry veneer area in square feet
summary(as.integer(all$MasVnrArea)) # NA -> price랑 상관관계보자
# all_MasVnrArea_rmna <-all[!is.na(all$MasVnrArea),c("MasVnrArea","SalePrice")]
# cor(all_MasVnrArea_rmna, use="pairwise.complete.obs") # 0.47  엄청 높진 않음.
# ggplot(data=all[!is.na(all$SalePrice),], aes(x=MasVnrArea, y=SalePrice))+
#   geom_point()
all$MasVnrArea <- as.integer(all$MasVnrArea)
summary(all$MasVnrArea)
# impute median 예정

# 21번 Utilities: Type of utilities available
# AllPub	All public Utilities (E,G,W,& S)	
# NoSewr	Electricity, Gas, and Water (Septic Tank)
# NoSeWa	Electricity and Gas Only
# ELO	Electricity only
summary(factor(all$Utilities)) # NA -> price랑 관계봐야함
all$SalePrice[is.na(all$Utilities)] # NA니 일단 예측에서 제외
all$Utilities <- as.factor(all$Utilities)
 
# 22번 BsmtFullBath: Basement full bathrooms
summary(factor(all$BsmtFullBath)) # NA -> price랑 관계봐야함
all$OverallQual[is.na(all$BsmtFullBath)] # median으로
all$BsmtFullBath <- as.numeric(all$BsmtFullBath)

# 23번 BsmtHalfBath: Basement half bathrooms
summary(factor(all$BsmtHalfBath)) # NA -> price랑 관계봐야함
all$OverallQual[is.na(all$BsmtHalfBath)] # median으로
all$BsmtHalfBath <- as.numeric(all$BsmtHalfBath)


# 24번 Functional: Home functionality (Assume typical unless deductions are warranted)
# Typ	Typical Functionality
# Min1	Minor Deductions 1
# Min2	Minor Deductions 2
# Mod	Moderate Deductions
# Maj1	Major Deductions 1
# Maj2	Major Deductions 2
# Sev	Severely Damaged
# Sal	Salvage only
summary(factor(all$Functional)) # NA 
all$OverallQual[is.na(all$Functional)] #1,4라서 전처리..음 그냥 median
all$Functional <- as.integer(revalue(all$Functional, c('Sal' = 0, 'Sev' = 1, 'Maj2' = 2, 'Maj1' = 3, 'Mod' = 4, 'Min2' = 5, 'Min1' = 6, 'Typ' = 7)))
summary(all$Functional) 

# 25번 Exterior1st: Exterior covering on house
# AsbShng	Asbestos Shingles
# AsphShn	Asphalt Shingles
# BrkComm	Brick Common
# BrkFace	Brick Face
# CBlock	Cinder Block
# CemntBd	Cement Board
# HdBoard	Hard Board
# ImStucc	Imitation Stucco
# MetalSd	Metal Siding
# Other	Other
# Plywood	Plywood
# PreCast	PreCast	
# Stone	Stone
# Stucco	Stucco
# VinylSd	Vinyl Siding
# Wd Sdng	Wood Siding
# WdShing	Wood Shingles
summary(factor(all$Exterior1st)) # NA -> price랑 관계봐야함
all$OverallQual[is.na(all$Exterior1st)] # 5라서 최빈값 대체
all$Exterior1st <- as.factor(all$Exterior1st)

# 26번 Exterior2nd: Exterior covering on house (if more than one material)
# AsbShng	Asbestos Shingles
# AsphShn	Asphalt Shingles
# BrkComm	Brick Common
# BrkFace	Brick Face
# CBlock	Cinder Block
# CemntBd	Cement Board
# HdBoard	Hard Board
# ImStucc	Imitation Stucco
# MetalSd	Metal Siding
# Other	Other
# Plywood	Plywood
# PreCast	PreCast
# Stone	Stone
# Stucco	Stucco
# VinylSd	Vinyl Siding
# Wd Sdng	Wood Siding
# WdShing	Wood Shingles
summary(factor(all$Exterior2nd)) # NA -> price랑 관계봐야함
all$OverallQual[is.na(all$Exterior2nd)] # 5라서 최빈값 대체
all$Exterior2nd <- as.factor(all$Exterior2nd)

# 31번 Electrical: Electrical system
# SBrkr	Standard Circuit Breakers & Romex
# FuseA	Fuse Box over 60 AMP and all Romex wiring (Average)	
# FuseF	60 AMP Fuse Box and mostly Romex wiring (Fair)
# FuseP	60 AMP Fuse Box and mostly knob & tube wiring (poor)
# Mix	Mixed
summary(factor(all$Electrical)) # NA -> price랑 관계봐야함
all$OverallQual[is.na(all$Electrical)] # 5라서 최빈값 대체
all$Electrical <- as.factor(all$Electrical)
 
# 32번 KitchenQual: Kitchen quality
# Ex	Excellent
# Gd	Good
# TA	Typical/Average
# Fa	Fair
# Po	Poor
summary(factor(all$KitchenQual)) # NA -> price랑 관계봐야함
all$OverallQual[is.na(all$KitchenQual)] # 5라서 median
all$Electrical <- as.integer(revalue(all$Electrical,Qualities))


# 35번 SaleType: Type of sale
# WD 	Warranty Deed - Conventional
# CWD	Warranty Deed - Cash
# VWD	Warranty Deed - VA Loan
# New	Home just constructed and sold
# COD	Court Officer Deed/Estate
# Con	Contract 15% Down payment regular terms
# ConLw	Contract Low Down payment and low interest
# ConLI	Contract Low Interest
# ConLD	Contract Low Down
# Oth	Other
summary(factor(all$SaleType)) # NA -> price랑 관계봐야함
all$OverallQual[is.na(all$SaleType)] # 5라서 최빈값
all$SaleType <- as.factor(all$SaleType)


# 36번 MasVnrType: Masonry veneer type
# BrkCmn	Brick Common
# BrkFace	Brick Face
# CBlock	Cinder Block
# None	None
# Stone	Stone
summary(factor(all$MasVnrType)) # NA -> price랑 관계봐야함
all$SalePrice[is.na(all$MasVnrType)] # 최빈값 대체
all$MasVnrType <- as.factor(all$MasVnrType)


# 37번 MSZoning: Identifies the general zoning classification of the sale.
# A	Agriculture
# C	Commercial
# FV	Floating Village Residential
# I	Industrial
# RH	Residential High Density
# RL	Residential Low Density
# RP	Residential Low Density Park 
# RM	Residential Medium Density
summary(factor(all$MSZoning)) # NA -> price랑 관계봐야함
all$OverallQual[is.na(all$MSZoning)] # 최빈값 대체
all$MSZoning <- as.factor(all$MSZoning)

# 그외 N/A없는 character변수 -> factor화
nominalVars2 <- which(sapply(all, is.character))
all[,nominalVars2] %>% names() # 22개
library(purrr)
all2<-all
all2[,nominalVars2]<- map_df(all2[,nominalVars2], as.factor)
all2
str(all2)



#  전처리 끝 ...



summary(all2)
recipe(SalePrice~., data=all2) %>% 
  step_impute_median(all_numeric_predictors(),-Id) %>%
  # step_center(all_numeric_predictors(),-Id) %>% 
  # step_scale(all_numeric_predictors(),-Id) %>% 
  # step_BoxCox(all_numeric_predictors(),-Id) %>% 
  # step_zv(all_numeric_predictors(),-Id) %>% 
  step_impute_mode(all_nominal_predictors()) %>% 
  # step_dummy(all_nominal_predictors(),-index) %>% 
  prep() %>% juice() -> tobeall
str(tobeall) # 82개
# 모델링하고 예측할 때 제외 할 것들
tobeall %>% select(-c(Utilities)) -> full
full %>% filter(index=="train") %>% select(-index) -> train0
full %>% filter(index=="test") %>% select(-index) -> test0


sapply(names(full), function(n) { sum(is.na(full[, n])) }) %>% sort(decreasing=T)





########################모델링##########################################

# N/A빼려고 했으나 recipe에서 imputation함
# na.omit(train0)-> train_rm_na
# na.omit(subset(test0,select=-c(SalePrice))) -> test_rm_na
# train_rm_na %>% nrow() # 1460->1338
# colSums(is.na(train_rm_na))
# test_rm_na %>% nrow() # 1459->1319
# colSums(is.na(test_rm_na))


trctrl1 <- trainControl(method="repeatedcv", repeats=5)

set.seed(2106)

rpart1 <- train(SalePrice~., data=train0[2:length(train0)], method="rpart", trControl=trctrl1)
print(rpart1)
# cp          RMSE      Rsquared   MAE     
# 0.06327058  56025.05  0.5083356  39077.14
rpart1$finalModel

rpart1.pred <- predict(rpart1, newdata=test0[2:length(test0)])

result.rpart1 <- bind_cols(Id=test0$Id, SalePrice=rpart1.pred)
str(result.rpart1)
write.csv(result.rpart1, "./data/housePrice_submission1.csv", row.names = FALSE)
# RMSLE 0.31155 /  12831등
result.rpart1$SalePrice %>% summary()
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 137587  137587  198841  184600  198841  305752 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 132299  132299  195146  180983  195146  306491 

result.rpart1$SalePrice %>% head()










