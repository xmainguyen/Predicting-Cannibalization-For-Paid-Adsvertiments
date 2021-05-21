setwd("G:\\2021 Spring\\Industry")
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lightgbm)
library(tidyr)
library(forecast)
library(lubridate)
library(DescTools)
library(randomForest)
library(e1071)
library(doParallel)
library(purrrlyr)
library(rpart)
library(rpart.plot)
library(gbm)
library(caret)
options(scipen = 999)


#Data Loading
data <- readRDS("data_0305.rds")


names(data)[111] <- "y" # Rename the cannibalization as Y
data$y[!is.finite(data$y)] <- 0 # set any inf- value to be 0

# Filter organic traffic, create holiday and covid variable

d <- data %>%
#  filter(Date >= '2019-08-06' & Date <= '2020-11-22') %>%
  filter( !(Date >= as.Date('2020-11-01') & Date <= as.Date('2020-11-11')) &
            !(Date >= as.Date('2019-11-01') & Date <= as.Date('2019-11-11')) &
            !(Date >= as.Date('2020-06-01') & Date <= as.Date('2020-06-18'))&
            !(Date >= as.Date('2020-12-01') & Date <= as.Date('2020-12-12'))&
            !(Date >= as.Date('2019-12-01') & Date <= as.Date('2019-12-12')))%>%
  select(-c(`bus_Number of Views`,org_today,org_yesterday,
            paid_today,change_in_cost,` Yesterday Paid Traffic`,
            `Total Paid Traffic`,`Total Cost`,`Yesterday Organic Traffic`,
            `ep_Indirect Order Line`,Date))%>%
  select(-c("Jd Paid Traffic", "ep Paid Traffic", 
           "con Paid Traffic","shop Paid Traffic"))

# Make target column name "y"
d <- d[,c(100,1:99)]

############################# Data-Preprocessing ###############################



# Remove near Zero variation 
nzv <- nearZeroVar(d, saveMetrics = TRUE)
d <- d[, c(TRUE,!nzv$zeroVar[2:ncol(d)])]
rm(nzv)

# Remove high correlated variables 
# calculate correlation matrix using Pearson's correlation formula
descrCor <-  cor(d[,2:ncol(d)])                           # correlation matrix
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > 0.8) # num Xs with cor > t


# which columns in your correlation matrix have a correlation greater than some
# specified absolute cutoff. Find them and remove them
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.8)
filteredDescr <- d[,2:ncol(d)][,-highlyCorDescr] # remove those specific columns
descrCor2 <- cor(filteredDescr)

# Merge back three cost variables and renamed into more readable foramte
d <- cbind(d$y, filteredDescr,d$`ep_Total Cost`,d$`con_Total Cost`,d$`shop_Total Cost`)
names(d)[1] <- "y"
names(d)[47] <- "ep_Total Cost"
names(d)[48] <- "con_Total Cost"
names(d)[49] <- "shop_Total Cost"


rm(filteredDescr, descrCor, descrCor2, highCorr, highlyCorDescr)# clean up


#############################  Partition data ##################################
set.seed(1234)
inTrain <- createDataPartition(y = d$y,
                               p = .8,
                               list = F)
tr <- d[inTrain,]
te <- d[-inTrain,]



# setting up for Cross validation 5 folds
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=5,        # k number of times to do k-fold
                     classProbs = F,  # if you want probabilities
                     #summaryFunction = twoClassSummary, # for classification
                     summaryFunction = defaultSummary,  # for regression
                     allowParallel=T)


#############################  Stepweise - Regression ##################################

full<-lm(y~., data=tr)
null<-lm(y~1, data = tr)
step<-step(null,list(lower= formula(null),upper=formula(full)),data=tr,direction="both",trace = 0)
summary(step)


#############################  Regression - No Variable Selection ##################################
# Train Regression model
lm_no_var_selection <- lm(y~.,data=tr)
lm_pred <- predict(lm_no_var_selection, newdata=te)

summary(lm_no_var_selection)


defaultSummary(data=data.frame(obs=tr$y, pred=predict(lm_no_var_selection, newdata=tr)), model=lm_no_var_selection)
defaultSummary(data=data.frame(obs=te$y, pred=predict(lm_no_var_selection, newdata=te)), model=lm_no_var_selection)
defaultSummary(data=data.frame(obs=d$y, pred=predict(lm_no_var_selection, newdata=d)), model=lm_no_var_selection)



SMAPE(d$y, predict(glm,newdata = d))*100



#############################  Random Forest ##################################


rf <- rpart(
  formula = y ~ .,
  data    = tr,
  method  = "anova", 
  control = rpart.control(maxdepth = 20,cp=0.001,xval = 10),
)



defaultSummary(data=data.frame(obs=tr$y, pred=predict(rf, newdata=tr)), model=rf)
defaultSummary(data=data.frame(obs=te$y, pred=predict(rf, newdata=te)), model=rf)
defaultSummary(data=data.frame(obs=d$y, pred=predict(rf, newdata=d)), model=rf)

#Getting top 20 variables
rf_var_importance<-as.data.frame(rf$variable.importance)
var_names<-rownames(rf_var_importance)
rf_var_select<-var_names [0:20]
train1<- tr%>%
  select(c(rf_var_select,y,jd_Cost,y))



#############################  Regression - Random Forest as Variable Selection ##################################
lm_rf_var_selection <- lm(y~.,data = train1)
defaultSummary(data=data.frame(obs=tr$y, pred=predict(lm_rf_var_selection, newdata=tr)), model=lm_rf_var_selection)
defaultSummary(data=data.frame(obs=te$y, pred=predict(lm_rf_var_selection, newdata=te)), model=lm_rf_var_selection)
defaultSummary(data=data.frame(obs=d$y, pred=predict(lm_rf_var_selection, newdata=d)), model=lm_rf_var_selection)

summary(lm_rf_var_selection)

#############################  Neural Network ##################################

cvindx<-createFolds(index, k=10, returnTrain = TRUE)

tunegrid<-expand.grid( .size=1:10, .decay= c(0, 0.1, 0.5 ,1))
maxSize<-max(tunegrid$.size)

numWts<-100

x<-train1[,-1]
cl <- makePSOCKcluster(6) #Starts the parallel computing
registerDoParallel(cl)
nnetFit.rf<-train(x=x, y=train1$y, 
                  method="nnet", 
                  metric="RMSE", 
                  linout=FALSE,
                  preProcess = c("range"), 
                  tuneGrid = tunegrid, 
                  trace=FALSE,
                  maxit=100,
                  MaxNWts=numWts,
                  trControl=ctrl)


stopCluster(cl) 
nnetFit.rf

p.rf<-predict(rf, newdata=te)
RMSE(p.rf, te$y)
defaultSummary(data=data.frame(obs=d$y, pred=predict(rf, newdata=d)), model=rf)

p.nnet.rf<-predict(nnetFit.rf, newdata=te)
RMSE(p.nnet.rf, te$y)
defaultSummary(data=data.frame(obs=tr$y, pred=predict(nnetFit.rf, newdata=tr)), model=nnetFit.rf)
defaultSummary(data=data.frame(obs=te$y, pred=predict(nnetFit.rf, newdata=te)), model=nnetFit.rf)
defaultSummary(data=data.frame(obs=d$y, pred=predict(nnetFit.rf, newdata=d)), model=nnetFit.rf)


#############################  Boosted Tree ##################################
Grid <- expand.grid( n.trees = seq(50,200,25), interaction.depth = c(10, 20), shrinkage = c(0.1, 0.01), n.minobsinnode=c(25))

cl <- makePSOCKcluster(6) #starts the cluster
registerDoParallel(cl)
 
gb.tree <- train(y~., data=tr, method = 'gbm', trControl=ctrl, tuneGrid=Grid, metric='RMSE')

stopCluster(cl)

defaultSummary(data=data.frame(obs=tr$y, pred=predict(gb.tree, newdata=tr)), model=gb.tree)
defaultSummary(data=data.frame(obs=te$y, pred=predict(gb.tree, newdata=te)), model=gb.tree)
defaultSummary(data=data.frame(obs=d$y, pred=predict(gb.tree, newdata=d)), model=gb.tree)


