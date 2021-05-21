#install.packages("readxl")
library(lubridate)
# input all data
setwd("/home/cao373/IP/Data/")
library(readxl)
library(dplyr)

######################### Read in Data #########################
jd <- read_excel("JD Booth.xlsx",sheet="Translated")
ep <- read_excel("JD Express.xlsx",sheet="Translated")
con <- read_excel("Contract Booth.xlsx",sheet="Translated")
shop <- read_excel("Shopping Contact.xlsx",sheet="Translated")
bus <- read_excel("Business Purchase Brand Category report.xlsx",sheet="Translated")
bus_diff_channel<-read_excel("京东智商 Business Purchase Brand Category.xlsx",sheet="Translated")
Op <- read_excel("Operating conditions Store Ranking report.xlsx",sheet="Translated")

shop_5<-read_csv("/home/cao373/IP/Data/Untitled Folder/高露洁ecmax投放1_购物触点账户报表20200501_20200531_0227.csv")
ep_5<-read_csv("/home/cao373/IP/Data/Untitled Folder/高露洁ecmax投放1_快车账户报表20200501_20200531.csv")
shop_5<-shop_5[,-39]
colnames(shop_5)<-colnames(shop)
ep_5<-ep_5[,-30]
colnames(ep_5)<-colnames(ep)

shop<-rbind(shop,shop_5)
ep<-rbind(ep,ep_5)

######################### pre processing and merge of four source channel data ######################### 

#Convert Column names by source channel
names(jd)[1]<- "Date"
colnames(jd) <- paste("jd",colnames(jd),sep="_")
names(jd)[1]<- "Date"
names(ep)[1] <- "Date"
colnames(ep) <- paste("ep", colnames(ep),sep="_")
names(ep)[1] <- "Date"

names(con)[1] <- "Date"
colnames(con) <- paste("con",colnames(con),sep="_")
names(con)[1] <- "Date"

colnames(shop) <- paste("shop",colnames(shop),sep="_")
names(shop)[1] <- "Date"


# Convert date into lubridate Date 
con$Date<-ymd(con$Date)
ep$Date<-ymd(ep$Date)
shop$Date<-ymd(shop$Date)
jd$Date<-ymd(jd$Date)
bus$Date<-ymd(bus$Date)
Op$Time<-as_date(Op$Time)

# Covert categorical data into factor
con<-con %>% 
  mutate_if(is.character,as.factor)
ep<-ep%>% 
  mutate_if(is.character,as.factor)
shop<-shop%>% 
  mutate_if(is.character,as.factor)
jd<-jd%>% 
  mutate_if(is.character,as.factor)
bus<-bus%>% 
  mutate_if(is.character,as.factor)



#Delete Columns with all 0s
con<-con[, colSums(con != 0) > 0]
ep<-ep[-516,]
ep<-ep[, colSums(ep != 0) > 0]
#shop<-shop[, colSums(shop != 0) > 0]
jd<-jd[, colSums(jd != 0) > 0]
bus<-bus[, colSums(bus != 0) > 0]
Op<-Op[, colSums(Op != 0) > 0]


#merge data
join1 <- jd %>% full_join(ep, by="Date")

join2 <- join1 %>% full_join(con,by="Date")

join3 <- join2 %>% full_join(shop, by ="Date")

join3 <- join3[order(join3$Date),]



######################### Join two store summary data #########################
# Recode bus table into paid and organic traffic
bus<-bus %>% 
  mutate(`First Level Source Channel` = recode_factor(`First Level Source Channel`,
                                                      "Natural Traffic Inside the Platform"="Organic Traffic", 
                                                      .default = 'Paid Traffic'))

# Summarize data into day level
bus1 <-bus %>% 
  select(-c("Brand","First Level Category","Second Level Category","Third Level Category"))%>%
  group_by(Date,`First Level Source Channel`)%>%
  summarise_if(is.numeric, sum, na.rm = TRUE)%>%
  ungroup()

#add source name into column names 
colnames(bus1) <- paste("bus",colnames(bus1),sep="_")

colnames(Op) <- paste("Op",colnames(Op),sep="_")
names(bus1)[1] <- "Date"
names(Op)[1] <- "Date"


#Group OP data also
Op1 <-Op %>% 
  select(-c(Op_店铺名称))

#merge data
BUS1 <- bus1 %>% full_join(Op, by="Date")

BUS1 <- BUS1[order(BUS1$Date),]



Masterdata <- BUS1 %>% full_join(join3, by="Date")


#Replace NAs with 0, delete unmeaningful columns
Masterdata1 <- Masterdata%>%
  select(-c(`jd_Account Name`,`jd_Device of Promotion`,`ep_Account Name`,`ep_Promoted Device`,
            `con_广告主Pin`,`con_Promoted Device`,`shop_Account Name`,`shop_Number of Likes`,
            `shop_Number of Comments`,`shop_Number of Shares`,`shop_Number of Subcription`,
            `shop_Number of Interactions`,`shop_Rate of Interactions`,`shop_Number of Views`,
            `shop_View Length`,`shop_Average View Length`))%>%
  replace(is.na(.), 0)



######################### Prepare data for model  #########################

#make the numebr of view specifically to organic traffic.
finaldata1<-Masterdata1%>%
  filter(`bus_First Level Source Channel` == "Paid Traffic" )%>%
  select(-colnames(Masterdata1)[5:23])%>%
  select(-c("bus_First Level Source Channel","bus_Number of Visitor","bus_Number of Views"))

finaldata2<-Masterdata1%>%
  filter(`bus_First Level Source Channel` == "Organic Traffic")%>%
  select(Date,`bus_Number of Views`,`Op_Revenue by Completed Orders`)

dateformodel<-merge(finaldata2,finaldata1,by=c("Date"))

#add source traffic and convert them into wide table
source_traffic<-bus_diff_channel%>%
  filter(`Second Level Source Channel` == "京东快车" | `Second Level Source Channel` == "独立海投" |
           `Second Level Source Channel` == "购物触点" |  `Second Level Source Channel` == "京东展位") %>%
  group_by(Time,`Second Level Source Channel`)%>%
  summarise(`Paid Traffic` = sum(`Number of Views`))%>%
  spread(`Second Level Source Channel`,`Paid Traffic`)%>%
  replace(is.na(.), 0)

colnames(source_traffic)<-c("Date","Jd Paid Traffic", "ep Paid Traffic", 
                            "con Paid Traffic","shop Paid Traffic")
source_traffic$Date<-as.Date(source_traffic$Date)

#Calcualte the organic traffic, and paid traffic for yesterday
dateformodel<-merge(source_traffic,dateformodel,by=c("Date"))
dateformodel<-filter(dateformodel,Date>=as.Date('2019-08-05') & Date<as.Date('2021-01-01'))
dataformodel1<-dateformodel%>%
  mutate(`Total Paid Traffic`= `Jd Paid Traffic` +`ep Paid Traffic`+ 
           `con Paid Traffic` +`shop Paid Traffic`,
         `Total Cost` = jd_Cost+`ep_Total Cost`+`con_Total Cost`+`shop_Total Cost`,
         `Cannibalization Rate` = -(`bus_Number of Views` - 71828)/`Total Paid Traffic`)

df_org<-dataformodel1%>%
  select(c("Date","Total Paid Traffic","bus_Number of Views","Total Cost"))
df_after1<-dataformodel1%>%
  select(c("Date","Total Paid Traffic","bus_Number of Views"))%>%
  mutate(Date= Date + 1)
colnames(df_after1)<-c("Date", " Yesterday Paid Traffic","Yesterday Organic Traffic")

rate_data<-right_join(df_org,df_after1,by=c("Date"))
#> dataformodel1$`bus_Number of Views`[which.min(dataformodel1$`Total Cost`)]
#[1] 71828
#> dataformodel1$`Total Paid Traffic`[which.min(dataformodel1$`Total Cost`)]
#[1] 288  

#Calculate the cannablization rate
rate<-rate_data%>%
  mutate(`Cannibalization Rate`=( (`bus_Number of Views` - 71828)/71828 - 
                                    (`Yesterday Organic Traffic` -71828)/71828 )/((`Total Paid Traffic` - 288)/288),
         org_today = (`bus_Number of Views` - 71828)/71828 , 
         org_yesterday = (`Yesterday Organic Traffic` -71828)/71828,
         paid_today = (`Total Paid Traffic` - 288)/288,
         change_in_cost = `Total Cost`-802.75)%>%
  select(-`bus_Number of Views`)

df_0305<-left_join(dateformodel,rate,by=c("Date"))

# using emailed method calculate cannibalization rate
rate_emailed<-rate_data%>%
  mutate(`Cannibalization Rate`=(-( `bus_Number of Views` -71828 )/ `Total Paid Traffic`))%>%
  select(`Cannibalization Rate`,Date)

df_0315_emailed_can_rate<-left_join(dateformodel,rate_emailed,by=c("Date"))
saveRDS(df_0315_emailed_can_rate,"df_0315_emailed_can_rate.rds")
saveRDS(df_0305,"df_0305.rds")

#> dataformodel1$`bus_Number of Views`[which.min(dataformodel1$`Total Cost`)]
#[1] 71828
#> dataformodel1$`Total Paid Traffic`[which.min(dataformodel1$`Total Cost`)]
#[1] 288         

#dataformodel1$`bus_Number of Views`[which.min(dataformodel1$`Total Cost`)]



saveRDS(dataformodel1,"data for model_305.rds")






