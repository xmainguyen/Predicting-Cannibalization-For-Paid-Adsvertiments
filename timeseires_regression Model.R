setwd("C:/Users/kevin/Desktop/Purdue_Stuff/COlgate/file")
data <- readRDS("data_0305.rds")

#library set up 
library(forecast)
library(dplyr)
library(tidyr)
library(lubridate)
library(Metrics)

########Set the training/ Validation Period
start_time = decimal_date(as.Date("2019-08-06"))
end_time = decimal_date(as.Date("2021-01-01"))
train_start = decimal_date(as.Date("2019-08-06"))
train_end =  decimal_date(as.Date("2020-11-22"))
valid_start = decimal_date(as.Date("2020-11-22"))
valid_end = decimal_date(as.Date("2021-01-01"))
valid_Period = 40




########Total Data Filtering
#Filtering out the time data of overall cannibalization rate 
data$`Cannibalization Rate`[!is.finite(data$`Cannibalization Rate`)] <- 0
data1 <- data %>%
  filter(Date != "2019-08-05")%>%
  select(Date,`Cannibalization Rate`)



#Conversion
canni.ts = msts(data1$`Cannibalization Rate`,
                start = start_time,
                end = end_time,
                seasonal.periods=c(7,365.25))
#summary(canni.ts)
#Partition
train <- window(canni.ts,start=train_start
                ,end=train_end)
#length(train)
valid <- window(canni.ts, start=valid_start,
                end=valid_end)
#length(valid)


#Building the ARIMA model
fit_1 = train %>% Arima(order=c(1,1,1), seasonal=c(0,1,0))
#acf(fit_1$residuals)
#length(fit_1$residuals)
#summary(fit_1)


#Make prediction & Validation with the ts.object 
p_canni = forecast(fit_1, h =valid_Period, level = 0)
mae(valid,p_canni$mean)

#buildout the ts residuals
canni_residuals = data.frame(Date= seq(as.Date("2019-08-06"),as.Date("2020-11-21"),by = "day"),
                             canni_residuals = fit_1$residuals)




#To plot the prediction result and the actual distribution
plot(fit_1$residuals, xlab = 'Time', ylab = 'Cannibalization Rate',
     xaxt = "n", col = "blue",main = "Cannibalization Rate Residuals by Time")
#lines(fit_1$fitted, col = 'Red')
#lines(p_canni$mean, col = "blue")
lines(fit_1$residuals, col = 'Green')




############################## Ensemble with GLM ###############################
holidays <- read.csv("holidays.csv")

holidays$Date <- as.Date(holidays$ï..Date,format = "%m/%d/%Y")

#Join residuals and filter out total view

d_train <- data %>%
  filter(Date >= '2019-08-01' & Date <= '2020-11-22') %>%
  #filter( !(Date >= as.Date('2020-11-01') & Date <= as.Date('2020-11-11')) &
            #!(Date >= as.Date('2019-11-01') & Date <= as.Date('2019-11-01')) &
            #!(Date >= as.Date('2020-06-01') & Date <= as.Date('2020-06-18')) )%>%
  inner_join(canni_residuals,d, by="Date")%>%
  mutate(Holiday = ifelse(Date %in% holidays$Date,1,0))%>%
  select(-c(`bus_Number of Views`,Date,org_today,org_yesterday,
            paid_today,change_in_cost,` Yesterday Paid Traffic`,
            `Total Paid Traffic`,`Total Cost`,`Yesterday Organic Traffic`,
            `ep_Indirect Order Line`,`Cannibalization Rate`))
  



#Build the validation dataset
d_valid <- data %>%
  filter(Date >= '2020-11-22' & Date <= '2020-12-31') %>%
  mutate(Holiday = ifelse(Date %in% holidays$Date,1,0)) %>%
  select(-c(`bus_Number of Views`,Date,org_today,org_yesterday,
            paid_today,change_in_cost,` Yesterday Paid Traffic`,
            `Total Paid Traffic`,`Total Cost`,`Yesterday Organic Traffic`,
            `ep_Indirect Order Line`,`Cannibalization Rate`))
  




############################# Data-Preprocessing ###############################
library(randomForest)
library(caret)
library(e1071)
library(doParallel)
library(tidyverse)
library(purrrlyr)
library(rpart)
library(rpart.plot)
#install.packages("gbm")
#library(gbm)


rf <- rpart(
  formula = canni_residuals ~ .,
  data    = d_train,
  method  = "anova", 
  control = rpart.control(maxdepth = 30,cp=0.001,xval = 10),
)

rf_var_importance<-as.data.frame(rf$variable.importance)
var_names<-rownames(rf_var_importance)
rf_var_select<-var_names [0:15]


#Choose cost option
d_train1<- d_train %>%
  select(c(rf_var_select,`con_Total Cost`,`shop_Total Cost`,
           `ep_Total Cost`,canni_residuals))

d_train$canni_residuals

rf_reg<-lm(canni_residuals~., data=d_train1)
summary(rf_reg)


#Validation process
d_valid1 <- d_valid %>%
  select(c(rf_var_select,`con_Total Cost`,`shop_Total Cost`,
           `ep_Total Cost`))
 

rf_predict <- predict(rf_reg, newdata = d_valid1)


#Create Validation for combine lm object
p_ensemble = rf_predict + p_canni$mean
mae(valid, rf_predict)
rmse(valid, rf_predict)


###################Solo Regression Model

d_train <- data %>%
  filter(Date >= '2019-08-01' & Date <= '2020-11-22') %>%
  filter( !(Date >= as.Date('2020-11-01') & Date <= as.Date('2020-11-11')) &
            !(Date >= as.Date('2019-11-01') & Date <= as.Date('2019-11-01')) &
            !(Date >= as.Date('2020-06-01') & Date <= as.Date('2020-06-18')) )%>%
  #inner_join(canni_residuals,d, by="Date")%>%
  mutate(Holiday = ifelse(Date %in% holidays$Date,1,0))%>%
  select(-c(`bus_Number of Views`,Date,org_today,org_yesterday,
            paid_today,change_in_cost,` Yesterday Paid Traffic`,
            `Total Paid Traffic`,`Total Cost`,`Yesterday Organic Traffic`,
            `ep_Indirect Order Line`,Date))

d_valid <- data %>%
  filter(Date >= '2020-11-22' & Date <= '2020-12-31') %>%
  mutate(Holiday = ifelse(Date %in% holidays$Date,1,0)) %>%
  select(-c(`bus_Number of Views`,Date,org_today,org_yesterday,
            paid_today,change_in_cost,` Yesterday Paid Traffic`,
            `Total Paid Traffic`,`Total Cost`,`Yesterday Organic Traffic`,
            `ep_Indirect Order Line`,Date))



rf <- rpart(
  formula =  `Cannibalization Rate`~ .,
  data    = d_train,
  method  = "anova", 
  control = rpart.control(maxdepth = 30,cp=0.001,xval = 10),
)

rf_var_importance<-as.data.frame(rf$variable.importance)
#top<-rf_var_importance%>%
#  filter(rf$variable.importance >1)
var_names<-rownames(rf_var_importance)
rf_var_select<-var_names [0:15]

d_train$jd_Cost

#Choose cost option
d_train1<- d_train %>%
  select(c(rf_var_select,`con_Total Cost`,`shop_Total Cost`,
           `ep_Total Cost`,`Cannibalization Rate`))


reg<-lm(`Cannibalization Rate`~., data=d_train1)
summary(rf_reg)


#Validation process
d_valid1 <- d_valid %>%
  select(c(rf_var_select,`con_Total Cost`,`shop_Total Cost`,
           `ep_Total Cost`))


reg_predict <- predict(reg, newdata = d_valid1)


#Create Validation for combine lm object
p_ensemble = rf_predict + p_canni$mean
mae(d_valid$`Cannibalization Rate`, reg_predict)
rmse(d_valid$`Cannibalization Rate`, reg_predict)


