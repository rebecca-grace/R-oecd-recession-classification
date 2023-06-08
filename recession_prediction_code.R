OECD_ALL<-read.csv(file="OECD_data.csv")

library(dplyr)
library(ggplot2)
library(e1071)
library(naniar)
library(mlbench)
library(caret)
library(gridExtra)
library(corrplot)
library(tidyr)

head(OECD_ALL)

dim(OECD_ALL)
names(OECD_ALL)
factor(OECD_ALL$Time)
factor(OECD_ALL$Country)
# All quarterly variables, 22 countries

# Transform data
# disable scientific notation
options(scipen = 999)
OECD_transform <- OECD %>% spread(VARIABLE, Value)
# Remove with missing response GDP growth data
OECD_transform <- OECD_transform %>% filter(!is.na(OECD_transform$GDP_ANNPCT),)

# Remove rows with all NA values, then change remaining NA values to 0
# Was going to remove columns with >80% nulls
OECD_transform <- OECD_transform[, which(colMeans(!is.na(OECD_transform)) > 0.10)]
OECD_transform <- OECD_transform[, colSums(is.na(OECD_transform)) != nrow(OECD_transform)]
i <- (colSums(OECD_transform[,3:164], na.rm=T) != 0)
OECD_transform <- OECD_transform[, i]
OECD_transform[is.na(OECD_transform)] <- 0
# included variables
variables <- as.data.frame(colnames(OECD_transform))

# Transform time variable
class(OECD_transform$Time)
library("stringr")
OECD_transform$Time_Year <- str_sub(OECD_transform$Time, - 4, - 1)
OECD_transform$Time_Quarter <- str_sub(OECD_transform$Time, 2, 2)
OECD_transform$TimeYQ <- paste0(OECD_transform$Time_Year,OECD_transform$Time_Quarter)

# Create Recession Flag
OECD_transform$REC_FLAG <- ifelse(OECD_transform$GDP_ANNPCT < -5, 2, ifelse(OECD_transform$GDP_ANNPCT < 0, 1, 0))
OECD_transform$REC_FLAG_2 <- ifelse(OECD_transform$GDP_ANNPCT < 0,1, ifelse(OECD_transform$GDP_ANNPCT > 0, 0, 0))
OECD_transform$Decade_Split <- ifelse(OECD_transform$Time_Year <= 2010, "First", ifelse(OECD_transform$GDP_ANNPCT < 2020, "Second", "Neither"))

# Plot flag over time
library(ggplot2)
library(gridExtra)
ggplot(OECD_transform, aes(x = GDP_ANNPCT, color = Decade_Split)) + geom_histogram(fill="white", binwidth=0.5) + xlim(-75, 75) + ggtitle("GDP Growth by Decade 2000-2010 and 2010-2020")
ggplot(OECD_transform, aes(x = Time_Year, y = GDP_ANNPCT)) + geom_point()+ggtitle("GDP Growth by Year")

# Remove outlier for GDP growth > 75%
OECD_transform[GDP_ANNPCT > 75,]
OECD_transform <- subset(OECD_transform,OECD_transform$GDP_ANNPCT <75)

# Transformations/feature engineering
# Take log values

# Result 1: Is Unemployment Different use t-test
OECD_transform$REC_FLAG_2 <- as.factor(OECD_transform$REC_FLAG_2)
ggplot(OECD_transform[OECD_transform$UNR > 0,], aes(x = UNR, colour=REC_FLAG_2)) + geom_histogram(fill = "white") + ggtitle("Unemployment Rate by Recession Flag 2000-2019")
ggplot(OECD_transform[OECD_transform$LF > 0,], aes(x = LF, colour=REC_FLAG_2)) + geom_histogram(fill = "white")

head(transform)
sapply(transform, class)
x <- transform$REC_FLAG_0
y <- transform$REC_FLAG_1
res <- wilcox.test(x, y, paired = TRUE)
res
summary(y-x)

# Hypothesis: Using p values to get the correct answer as to the effects of deeper recessions
# Mean increase in unemployment rate is 0.71%

# Correlation
corr_simple <- function(data=OECD_transform,sig=0.5){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
}

corr <- corr_simple()
# Place the outcome variable GDP_ANNPCT at the end of the dataset
OECD_transform$GDP_ANNPCT_FINAL <- OECD_transform$GDP_ANNPCT
OECD_corr = cor(OECD_transform[,3:164])
hc = findCorrelation(OECD_corr, cutoff=0.90) # putt any value as a "cutoff" 
hc = sort(hc)
OECD_transform_corr = OECD_transform[,-c(hc)]
print(OECD_transform_corr)

# Remove variables that are obvious confounders
OECD_transform_corr <- OECD_transform_corr %>% select(-c("PGDP_ANNPCT","PGDP","GDP_USD"))
table(OECD_transform_corr$TimeYQ)

# Create ratios of financial variables
OECD_transform_corr$SAV_RATIO <- (OECD_transform_corr$SAVH/OECD_transform_corr$SAVG)*100
OECD_transform_corr$EMP_RATIO <- (OECD_transform_corr$ES/OECD_transform_corr$ET)*100

# Plots before transformation
ggplot(OECD_transform_corr, aes(x = PXGSD, y=TYB, colour = REC_FLAG)) + geom_point()
ggplot(OECD_transform_corr, aes(x = ET_ANNPCT, y=CPIH_YTYPCT, colour = REC_FLAG)) + geom_point()
ggplot(OECD_transform_corr, aes(x = CGAA, y=PMNW, colour = REC_FLAG)) + geom_point()
# First 2 predictors
ggplot(OECD_transform_corr, aes(x = CBD, y=CGAA, colour = REC_FLAG)) + geom_point()
ggplot(OECD_transform_corr, aes(x = TYH, y=TYB, colour = REC_FLAG)) + geom_point()
ggplot(OECD_transform_corr, aes(x = TYH, y=REC_FLAG)) + geom_point()

OECD_transform_corr$REC_FLAG_2 <- as.factor(OECD_transform_corr$REC_FLAG_2)
ggplot(OECD_transform_corr[OECD_transform_corr$RPMGS > 0,], aes(x = RPMGS, color=REC_FLAG_2)) + geom_bar(fill = "white")
ggplot(OECD_transform_corr[OECD_transform_corr$RPXGS > 0,], aes(x = RPXGS, color=REC_FLAG_2)) + geom_bar(fill = "white")

OECD_transform_corr$REC_FLAG_3 <- ifelse(OECD_transform_corr$GDP_ANNPCT < -5,1, ifelse(OECD_transform_corr$GDP_ANNPCT > -4, 0, 0))
OECD_transform_corr$REC_FLAG_4 <- ifelse(OECD_transform_corr$GDP_ANNPCT > 0, 0, ifelse(OECD_transform_corr$GDP_ANNPCT > -4, 1, 0))
table(OECD_transform_corr$REC_FLAG_3)
table(OECD_transform_corr$REC_FLAG_4)

# Transformations/feature engineering
# Take log values
  OECD_transform_corr$NLGX <- log(1+abs(OECD_transform_corr$NLGX))
  OECD_transform_corr$SAVG <- log(1+abs(OECD_transform_corr$SAVG))
  OECD_transform_corr$BSII <- log(1+abs(OECD_transform_corr$BSII))
  OECD_transform_corr$NTR <- log(1+abs(OECD_transform_corr$NTR))
  OECD_transform_corr$CB <- log(1+abs(OECD_transform_corr$CB))
  OECD_transform_corr$CBD <- log(1+abs(OECD_transform_corr$CBD))
  OECD_transform_corr$NTRD <- log(1+abs(OECD_transform_corr$NTRD))
  OECD_transform_corr$ET <- log(1+abs(OECD_transform_corr$ET))
  OECD_transform_corr$ES <- log(1+abs(OECD_transform_corr$ES))
  OECD_transform_corr$ET_NA <- log(1+abs(OECD_transform_corr$ET_NA))
  OECD_transform_corr$IHV <- log(1+abs(OECD_transform_corr$IHV))
  OECD_transform_corr$GGINTR <- log(1+abs(OECD_transform_corr$GGINTR))
 
  OECD_transform_corr$ULCDR <- log(1+abs(OECD_transform_corr$ULCDR))
  OECD_transform_corr$WSST <- log(1+abs(OECD_transform_corr$WSST))
  OECD_transform_corr$ITISKV <- log(1+abs(OECD_transform_corr$ITISKV))
  OECD_transform_corr$WSSS <- log(1+abs(OECD_transform_corr$WSSS))
  OECD_transform_corr$XGSD <- log(1+abs(OECD_transform_corr$XGSD))
  OECD_transform_corr$CP <- log(1+abs(OECD_transform_corr$CP))
  OECD_transform_corr$MGSD <- log(1+abs(OECD_transform_corr$MGSD))
  OECD_transform_corr$XGS <- log(1+abs(OECD_transform_corr$XGS))
  OECD_transform_corr$MGS <- log(1+abs(OECD_transform_corr$MGS))
  OECD_transform_corr$XGSVD <- log(1+abs(OECD_transform_corr$XGSVD))
  OECD_transform_corr$TGSVD <- log(1+abs(OECD_transform_corr$TGSVD))
  OECD_transform_corr$XMKT <- log(1+abs(OECD_transform_corr$XMKT))
  OECD_transform_corr$SAVH <- log(1+abs(OECD_transform_corr$SAVH))
  OECD_transform_corr$CPV <- log(1+abs(OECD_transform_corr$CPV))
  OECD_transform_corr$MGSVD <- log(1+abs(OECD_transform_corr$MGSVD))
  OECD_transform_corr$XGSV <- log(1+abs(OECD_transform_corr$XGSV))
  OECD_transform_corr$FDD <- log(1+abs(OECD_transform_corr$FDD))
  OECD_transform_corr$CAPOG <- log(1+abs(OECD_transform_corr$CAPOG))
  OECD_transform_corr$TKPG <- log(1+abs(OECD_transform_corr$TKPG))
  OECD_transform_corr$IGV <- log(1+abs(OECD_transform_corr$IGV))
  OECD_transform_corr$YPERG <- log(1+abs(OECD_transform_corr$YPERG))
  OECD_transform_corr$YPEPG <- log(1+abs(OECD_transform_corr$YPEPG))
  OECD_transform_corr$TDDV <- log(1+abs(OECD_transform_corr$TDDV))
  OECD_transform_corr$GDPV <- log(1+abs(OECD_transform_corr$GDPV))
  OECD_transform_corr$GDPVD <- log(1+abs(OECD_transform_corr$GDPVD))
  OECD_transform_corr$CFKG <- log(1+abs(OECD_transform_corr$CFKG))
  OECD_transform_corr$IBV <- log(1+abs(OECD_transform_corr$IBV))
  OECD_transform_corr$TOCR <- log(1+abs(OECD_transform_corr$TOCR))
  OECD_transform_corr$TEVD <- log(1+abs(OECD_transform_corr$TEVD))
  OECD_transform_corr$IGAA <- log(1+abs(OECD_transform_corr$IGAA))
  OECD_transform_corr$GNP <- log(1+abs(OECD_transform_corr$GNP))
  OECD_transform_corr$GNINTP <- log(1+abs(OECD_transform_corr$GNINTP))
  OECD_transform_corr$GNPV <- log(1+abs(OECD_transform_corr$GNPV))
  OECD_transform_corr$GGINTP <- log(1+abs(OECD_transform_corr$GGINTP))
  OECD_transform_corr$EQPF <- log(1+abs(OECD_transform_corr$EQPF))
  OECD_transform_corr$ISHV <- log(1+abs(OECD_transform_corr$ISHV))
  OECD_transform_corr$TYB <- log(1+abs(OECD_transform_corr$TYB))
  OECD_transform_corr$YPOTG <- log(1+abs(OECD_transform_corr$YPOTG))
  OECD_transform_corr$SSRG <- log(1+abs(OECD_transform_corr$SSRG))
  OECD_transform_corr$SSPG <- log(1+abs(OECD_transform_corr$SSPG))
  OECD_transform_corr$YPOTH <- log(1+abs(OECD_transform_corr$YPOTH))
  OECD_transform_corr$SAVH_G <- log(1+abs(OECD_transform_corr$SAVH_G))
  OECD_transform_corr$YSE <- log(1+abs(OECD_transform_corr$YSE))
  OECD_transform_corr$TIND <- log(1+abs(OECD_transform_corr$TIND))
  OECD_transform_corr$YROTH <- log(1+abs(OECD_transform_corr$YROTH))
  OECD_transform_corr$TYH <- log(1+abs(OECD_transform_corr$TYH))
  OECD_transform_corr$CGAA <- log(1+abs(OECD_transform_corr$CGAA))
  OECD_transform_corr$TY <- log(1+abs(OECD_transform_corr$TY))
  OECD_transform_corr$IOILV <- log(1+abs(OECD_transform_corr$IOILV))
  OECD_transform_corr$GDPVCSA <- log(1+abs(OECD_transform_corr$GDPVCSA))
  OECD_transform_corr$YPH <- log(1+abs(OECD_transform_corr$YPH))
  OECD_transform_corr$TDD <- log(1+abs(OECD_transform_corr$TDD))
  OECD_transform_corr$IOBV <- log(1+abs(OECD_transform_corr$IOBV))
  OECD_transform_corr$YSE_G <- log(1+abs(OECD_transform_corr$YSE_G))
  OECD_transform_corr$YPGX <- log(1+abs(OECD_transform_corr$YPGX))
  OECD_transform_corr$YPG <- log(1+abs(OECD_transform_corr$YPG))
  OECD_transform_corr$YPGT <- log(1+abs(OECD_transform_corr$YPGT))
  OECD_transform_corr$YRGX <- log(1+abs(OECD_transform_corr$YRGX))
  OECD_transform_corr$YRG <- log(1+abs(OECD_transform_corr$YRG))
  OECD_transform_corr$YRGT <- log(1+abs(OECD_transform_corr$YRGT))
  OECD_transform_corr$CPAA <- log(1+abs(OECD_transform_corr$CPAA))
  OECD_transform_corr$GDPOFS <- log(1+abs(OECD_transform_corr$GDPOFS))
 
  OECD_transform_corr[is.na(OECD_transform_corr)] <- 0
  is.na(OECD_transform_corr)<-sapply(OECD_transform_corr, is.infinite)
  OECD_transform_corr[is.na(OECD_transform_corr)]<-0
  
# Split before selecting significance
# Training 75% Q12000 to Q42014
# Test 25% Q12015 to Q12020
set.seed(123)
OECD_transform_corr$REC_FLAG <- as.factor(OECD_transform_corr$REC_FLAG)
# Remove REC_FLAG_2
inTrain <- createDataPartition(OECD_transform_corr$REC_FLAG, p = .75)[[1]]
OECD.train <- OECD_transform_corr[inTrain,]
OECD.test <- OECD_transform_corr[-inTrain,]

sapply(OECD.train, class)
# Class imbalance
ggplot(OECD.train, aes(x = REC_FLAG, label = Frequency)) + geom_bar( fill = "lightblue") + ggtitle("Training Set Classes")
ggplot(OECD.test, aes(x = REC_FLAG, label = Frequency)) + geom_bar( fill = "lightblue") + ggtitle("Testing Set Classes")

ggplot(OECD.train$REC_FLAG)
ggplot(OECD.test$REC_FLAG)

# Removing label data for analysis
OECD.train <- OECD.train %>% select(-c("TimeYQ","Decade_Split","Country","Time","Time_Year","REC_FLAG_2","GDP_ANNPCT_FINAL"))
OECD.test <- OECD.test %>% select(-c("TimeYQ","Decade_Split","Country","Time","Time_Year","REC_FLAG_2","GDP_ANNPCT_FINAL"))

# Structure of predictors
ggplot(OECD_transform_corr, aes(x = PXGSD, y=TYB, colour = REC_FLAG)) + geom_point()
ggplot(OECD_transform_corr, aes(x = ET_ANNPCT, y=CPIH_YTYPCT, colour = REC_FLAG)) + geom_point()
ggplot(OECD_transform_corr, aes(x = CGAA, y=PMNW, colour = REC_FLAG)) + geom_point()
ggplot(OECD_transform_corr, aes(x = CBD, y=CGAA, colour = REC_FLAG)) + geom_point()
ggplot(OECD_transform_corr, aes(x = TYH, y=TYB, colour = REC_FLAG)) + geom_point()
ggplot(OECD_transform_corr, aes(x = TYH, y=REC_FLAG)) + geom_point()

# Result 3: SVM Prediction Model
models.10.fold.cv <- lapply(c("linear", "polynomial", "radial"),
                            function(x) svm(REC_FLAG ~ ., data = OECD.train, kernel = x,
                                            cross = 10, type = "C-classification"))
model.accuracies <- lapply(models.10.fold.cv, "[[", "accuracies")

linear.model <- svm(REC_FLAG ~ ., data = OECD.train, kernel = "linear", cost = 5, cross = 10, type = "C-classification", decision.values = TRUE,
                    trControl = trainControl(method = "repeatedcv", number = 10))
mean(linear.model$accuracies)
linear.pred <- predict(linear.model, newdata = OECD.test)
linear.confusion <- confusionMatrix(linear.pred, OECD.test$REC_FLAG)
linear.confusion$table
linear.confusion

# Degree2
poly2.model <- svm(REC_FLAG ~ ., data = OECD.train, kernel= "polynomial", degree =2, cost = 100, cross = 10, type = "C-classification", decision.values = TRUE,
                   trControl = trainControl(method = "repeatedcv", number = 10))
poly2.pred <- predict(poly2.model, newdata = OECD.test)
poly2.confusion <- confusionMatrix(poly2.pred, OECD.test$REC_FLAG)
poly2.confusion$table
poly2.confusion

# Polynomial kernel degree3
poly.model <- svm(REC_FLAG ~ ., data = OECD.train, kernel= "polynomial", cost = 100, cross = 10, type = "C-classification", decision.values = TRUE,
                  trControl = trainControl(method = "repeatedcv", number = 10))
poly.pred <- predict(poly.model, newdata = OECD.test)
poly.confusion <- confusionMatrix(poly.pred, OECD.test$REC_FLAG)
poly.confusion$table
poly.confusion

# Plot ROC curve for cubic polynomial
svm.pol <- svm(REC_FLAG ~ ., data = OECD.train, kernel= "polynomial", probability=TRUE,cost = 100, cross = 10, type = "C-classification", decision.values = TRUE,
               trControl = trainControl(method = "repeatedcv", number = 10))
plot(svm.pol, OECD.train)
svmpol.preds<-predict(svm.pol, newdata = OECD.test, probability=TRUE)
confmat.pol <- table(pred = svmpol.preds, true = OECD.test$REC_FLAG)

library(ROCR)
svmpol.rocr<-prediction(attr(svmpol.preds,"probabilities")[,2], OECD.test$REC_FLAG == 2)
svmpol.perf<-performance(svmpol.rocr, measure = "tpr", x.measure = "fpr")
plot(svmlinear.perf,col="BLUE")
plot(svmpol.perf,add=TRUE,col="RED")
plot(svmpol.perf)

# Quadratic Polynomial
poly4.model <- svm(REC_FLAG ~ ., data = OECD.train, kernel= "polynomial", degree =4, cost = 100, cross = 10, type = "C-classification", decision.values = TRUE,
                  trControl = trainControl(method = "repeatedcv", number = 10))
poly4.pred <- predict(poly4.model, newdata = OECD.test)
poly4.confusion <- confusionMatrix(poly4.pred, OECD.test$REC_FLAG)
poly4.confusion$table
poly4.confusion

#Radial basis kernel 
rad.model <- svm(REC_FLAG ~ ., data = OECD.train, kernel= "radial", cost = 100, cross = 10, type = "C-classification", decision.values = TRUE,
                 trControl = trainControl(method = "repeatedcv", number = 10))
rad.pred <- predict(rad.model, newdata = OECD.test)
rad.confusion <- confusionMatrix(rad.pred, OECD.test$REC_FLAG)
rad.confusion$table
rad.confusion
# decision.values = TRUE to get fitted values

# Best parameters 
# tune.out <- tune(svm, REC_FLAG~., data = OECD.train, kernel= "polynomial", degree =3, ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)), type = "C-classification")
# (bestmod <- tune.out$best.model)

# Overfitting charts for linear and radial.
# http://www.science.smith.edu/~jcrouser/SDS293/labs/lab15-r.html

# Predictive Model
# Random Forest
library(randomForest)
library(ranger)
fit_control <- trainControl(method = "repeatedcv", number = 10)
rf_grid <- expand.grid(mtry = c(2,3, 4, 5,6,7,8,9,10),
                       splitrule = c("gini", "extratrees")
                       ,min.node.size = c(1, 3, 5))
# run a random forest model
RF.model <- train(REC_FLAG ~ ., data = OECD.train,
                method = "ranger",
                ntree = 50,
                trControl = fit_control,
                tuneGrid = rf_grid)
RF.model$bestTune
plot(RF.model)
# Plot random forest

model <- randomForest(REC_FLAG ~ ., data = OECD.train, importance=TRUE, ntree=50, mtry = 10, splitrule = "gini", min.node.size = 5,
                      trControl = trainControl(method = "repeatedcv", number = 10))
plot(model)
# Predict on training data
rf.pred <- predict(model, newdata = OECD.test)
rf.confusion <- confusionMatrix(rf.pred, OECD.test$REC_FLAG)
rf.confusion

# Logistic Regression multinomial
library(nnet)
logit.model <- train(REC_FLAG ~ ., data = OECD.train, method = "multinom",
                     trControl = trainControl(method = "repeatedcv", number = 10))
logit.pred <- predict(logit.model, newdata = OECD.test)
logit.confusion <- confusionMatrix(logit.pred, OECD.test$REC_FLAG)
logit.confusion
print(logit.model)
