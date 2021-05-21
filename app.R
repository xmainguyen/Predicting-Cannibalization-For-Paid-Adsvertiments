setwd("/Users/mainguyen/Downloads/Shinyapp")

#############load the required packages################
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(gbm)
options(scipen = 999)

######################### Loading Dataset ##########################
data <- readRDS("data_0305.rds")
data1 <- readRDS("data_0305.rds")
data2 <- read_excel("EDA.xlsx")
data3 <- read_excel("HolidayCannibalization.xlsm")
data4 <- read_excel("nonHolidayCannibalization.xlsm")
df3 <- read.csv("df.csv")
gb <- readRDS("gb+tree3+rmse=+0.03603248,+r+square+=+0.18296408.rds")

######################### Data Pre-processing  #####################
names(data)[names(data) == "Jd Paid Traffic"] <- "Banner_ads_Paid_Traffic"
names(data)[names(data) == "ep Paid Traffic"] <- "Search_ads_Paid_Traffic"
names(data)[names(data) == "con Paid Traffic"] <- "Automatedsearch_ads_Paid_Traffic"
names(data)[names(data) == "shop Paid Traffic"] <- "Feed_ads_Paid_Traffic"

names(data)[names(data) == "Total Paid Traffic"] <- "Total_Paid_Traffic"
names(data)[names(data) == "bus_Number of Views"] <- "Total_Organic_Traffic"

names(data)[names(data) == "jd_Conversion rates"] <- "Banner_ads_ROI"
names(data)[names(data) == "ep_ROI"] <- "Search_ads_ROI"
names(data)[names(data) == "con_ROI"] <- "Automatedsearch_ads_ROI"
names(data)[names(data) == "shop_ROI"] <- "Feed_ads_Paid_ROI"

names(data)[names(data) == "jd_Click Rate"] <- "Banner_ads_Clickrate"
names(data)[names(data) == "ep_Click Rate"] <- "Search_ads_Clickrate"
names(data)[names(data) == "con_Click Rate"] <- "Automatedsearch_ads_Clickrate"
names(data)[names(data) == "shop_Click Rate"] <- "Feed_ads_Paid_Clickrate"

names(data)[names(data) == "jd_Cost"] <- "Banner_ads_Totalcost"
names(data)[names(data) == "ep_Total Cost"] <- "Search_ads_Totalcost"
names(data)[names(data) == "con_Total Cost"] <- "Automatedsearch_ads_Totalcost"
names(data)[names(data) == "shop_Total Cost"] <- "Feed_ads_Paid_Totalcost"

# Create Data
v1 <- sum(data[,'Banner_ads_Paid_Traffic'], na.rm = TRUE)
v2 <- sum(data[,'Search_ads_Paid_Traffic'], na.rm = TRUE)
v3 <- sum(data[,'Automatedsearch_ads_Paid_Traffic'], na.rm= TRUE)
v4 <- sum(data[,'Feed_ads_Paid_Traffic'], na.rm=TRUE)

va1 <- sum(data[,'Banner_ads_Totalcost'], na.rm = TRUE)
va2 <- sum(data[,'Search_ads_Totalcost'], na.rm = TRUE)
va3 <- sum(data[,'Automatedsearch_ads_Totalcost'], na.rm= TRUE)
va4 <- sum(data[,'Feed_ads_Paid_Totalcost'], na.rm=TRUE)

names(data2)[names(data2) == "Organic_Total_Number of views"] <- "Organic_Total_Number_of_views"
names(data2)[names(data2) == "Paid_Total_Number of views"] <- "Paid_Total_Number_of_views"

names(data3)[names(data3) == "Marginal Change...3"] <- "Investment_Marginal_Change"
names(data3)[names(data3) == "Cannibalization Rate"] <- "Cannibalization_Rate"

names(data4)[names(data4) == "Marginal...3"] <- "Investment_Marginal_Change"
names(data4)[names(data4) == "Cannibalization"] <- "Cannibalization_Rate"

# Prepare Dataframe for prediction

d<-colMeans(data1[ ,-1],na.rm = TRUE)
d<-as.data.frame(d)
d<-cbind(d,colnames(data1)[-1])
colnames(d)<-c("Value","ColNames")
d1<-d%>%
  spread(ColNames,Value) %>%
  select(-c(jd_Cost,`ep_Total Cost`,`shop_Total Cost`,`con_Total Cost`))

# Slider limits
slider1limit <- 80
slider2limit <- 50
slider3limit <- 25
slider4limit <- 5

##################### R-Shiny App UI ###########################

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Paid Advertisement Optimization")

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Dashboard",
    tabName = "dashboard",
    icon = icon("dashboard")
  ),
  menuItem(
    "Model Statistics",    tabName = "Stats",    icon = icon("calendar")
  ),
  menuItem("Predict!", tabName = "predict", icon = icon("book"))
))

tbs <- tabItems(
######################################################
#### First tab content #####
  tabItem(
    tabName = "dashboard",
    fluidRow(
      valueBoxOutput("value1", width = 3)      ,
      valueBoxOutput("value2", width = 3)      ,
      valueBoxOutput("value3", width = 3)      ,
      valueBoxOutput("value4", width = 3)
    ),
    fluidRow(
      box(
        title = "Traffic From 4 Channels Ratio"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("paidTrafficRatio", height = "300px")
      ),
      box(
        title = "Investment From 4 Channels Ratio",
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("investmentRatio", height = "300px")
      )),
    fluidRow(
      box(
      width=12,
      title = "Paid vs Organic Traffic By Month"        ,
      status = "primary"        ,
      solidHeader = TRUE        ,
      collapsible = TRUE        ,
      plotOutput("trafficRatio", height = "300px")
    ))
  ),
######################################################
#### Second tab content #####
  tabItem(
    tabName = "Stats",
    fluidRow(
      box(
        title = "Model Info (Gradient Boosting Machine)",
        width = 12,
        verbatimTextOutput("modelinfo")
      )),
    fluidRow(
      box(title = "Variable Importance (Gradient Boosting Machine)",
          width = 12,
          status = "primary"        ,
          solidHeader = TRUE        ,
          collapsible = TRUE        ,
          plotOutput("varImp", height = "300px")
      )),
    fluidRow(
      box(
        width=12,
        title = "Model Accuracy"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("predPlot", height = "300px")
      )
    )
  ),
######################################################
#### Third tab content #####
tabItem(tabName = "predict",
        fluidRow(
          box(
            helpText("Please Enter Total Investment and Investment Allocation"),
            numericInput("totalinvestment","Enter Total Investment",value=10000, min=0),
            sliderInput("slider1", "Banner Ads Percentage: ", min = 0, max = 100, value = 25, step=1),
            uiOutput("slider2"),
            uiOutput("slider3"),
            actionButton(inputId = "predict", label = "Predict Cannibalization!")
          ),
          tableOutput("restable")),
        fluidRow(
          infoBoxOutput("approvalBox", width = 12)
        ))
)

# combine the two fluid rows to make the body
body <- dashboardBody(tbs,
                      tags$head(
                      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      ))

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Paid Advertisement Optimization', header, sidebar, body, 
                    skin = 'red')

############################## R-Shiny App Server ##############################
server <- function(input, output) {

######################################################
#### First tab content ##### 
  
#some data manipulation to derive the values of KPI boxes
  Average_Cost <- round(mean(mean(data$Banner_ads_Totalcost),mean(data$Search_ads_Totalcost),
                             mean(data$Automatedsearch_ads_Totalcost), mean(data$Feed_ads_Paid_Totalcost)),0)
  Average_ROI <- round(mean(mean(data$Banner_ads_ROI), mean(data$Search_ads_ROI),
                            mean(data$Automatedsearch_ads_ROI), mean(data$Feed_ads_Paid_ROI)),2)
  Average_CickRate <- round(mean(mean(data$Banner_ads_Clickrate),mean(data$Search_ads_Clickrate),
                            mean(data$Automatedsearch_ads_Clickrate), mean(data$Feed_ads_Paid_Clickrate)),2)
  Average_Cannibalization <- round(mean(mean(data3$Cannibalization_Rate), 
                                        mean(data4$Cannibalization_Rate))*100,2)
  
#creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(Average_Cost, format = "d", big.mark = ',')
      ,
      paste('Average Cost',"$",Average_Cost)
      ,
      icon = icon("stats", lib = 'glyphicon')
      ,
      color = "purple",
      width = 3
    )
  })
  
  output$value2 <- renderValueBox({
    valueBox(
      formatC(Average_ROI, format = "d", big.mark = ',')
      ,
      paste('Average ROI', Average_ROI, "%")
      ,
      icon = icon("gbp", lib = 'glyphicon')
      ,
      color = "green",
      width = 3
    )
    
  })
  
  output$value3 <- renderValueBox({
    valueBox(
      formatC(Average_CickRate, format = "d", big.mark = ',')
      ,
      paste('Average CickRate',Average_CickRate,"%")
      ,
      icon = icon("menu-hamburger", lib = 'glyphicon')
      ,
      color = "yellow",
      width = 3
    )
    
  })
  
  output$value4 <- renderValueBox({
    valueBox(
      formatC(Average_Cannibalization, format = "d", big.mark = ',')
      ,
      paste('Average Cannibalization', Average_Cannibalization, "%")
      ,
      icon = icon("menu-hamburger", lib = 'glyphicon')
      ,
      color = "blue"
    )
    
  })
  
# creating plots content
output$paidTrafficRatio <- renderPlot({
  pie <- data.frame(
    group=c("Banner_ads_Paid_Traffic","Search_ads_Paid_Traffic","Automatedsearch_ads_Paid_Traffic", "Feed_ads_Paid_Traffic"),
    value=c(v1,v2,v3,v4))
  
  ggplot(pie, aes(x="", y=value, fill=group))+
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    theme_void() +
    geom_text(aes(label = paste0(scales::percent(value / sum(value)))),
              position = position_stack(vjust = 0.5))
})

output$investmentRatio <- renderPlot ({
  pie2 <- data.frame(
    group=c("Banner_ads_Totalcost","Search_ads_Totalcost","Automatedsearch_ads_Totalcost", "Feed_ads_Paid_Totalcost"),
    value=c(va1,va2,va3,va4))
  
  ggplot(pie2, aes(x="", y=value, fill=group))+
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    theme_void() +
    geom_text(aes(label = paste0(scales::percent(value / sum(value)))),
      position = position_stack(vjust = 0.5))
})

output$trafficRatio <- renderPlot({
  data <- data %>%
    mutate(month = format(Date, "%m"))
  
  trafficRatio_df <- data %>%
    select(month, Total_Organic_Traffic,Total_Paid_Traffic) %>%
    group_by(month) %>%
    summarise_if(is.numeric, sum, na.rm=TRUE)
  
  trafficRatio_df <- gather(trafficRatio_df,"Source","value",2:3)
  
  ggplot(trafficRatio_df, aes(x=month, y=value,fill=Source)) +
    geom_bar(position="stack", stat="identity")
})
  
######################################################
#### Second tab content #####
output$modelinfo <- renderPrint({
  gb$bestTune
})

output$varImp <- renderPlot({
  
#   # Return a list containing the filename
#   list(src = "importance.png",
#        width= 400)
# }, deleteFile = FALSE)
  df3 %>%
    arrange(imp) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
    mutate(variable=factor(variable, levels=variable)) %>%   # This trick update the factor levels
    ggplot(aes(x = variable, y = 0, xend = variable, yend = imp),
           size = 5, alpha = 0.7) +
    geom_segment( aes(xend=variable, yend=imp)) +
    geom_point( size=4, color="orange") +
    coord_flip() +
    theme_bw() +
    xlab("")
})


#Predictive model and Output Plot
output$predPlot <- renderPlot({
  data1 <- data1 %>%
    filter(data1$`Cannibalization Rate` > -0.2)
  
  pred<-as.data.frame(predict(gb, newdata=data1))
  pred<-cbind(data1$Date,pred, actual=data1$`Cannibalization Rate`)
  colnames(pred)<-c("Date","predicted","actual")
  pred[1,4] <- 0
  
  colors <- c("Actual"= "tan1" ,"Predicted" ="darkorange4")
  
  p <- ggplot(pred, aes(x=Date)) +
    geom_line(aes(y=actual, color="Actual"), alpha=0.5, size=1.5) +
    geom_line(aes(y=predicted, color="Predicted"), size=1.5) +
    labs(y="Cannibalization Rate", x = "Date",color = "Legend")+
    scale_color_manual(values=colors)
  
  My_Theme = theme(
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "grey"),
    legend.position = c(0.1, 0.1))
  
  p +My_Theme
})
  
######################################################
#### Third tab content #####
  
#Slider inputs 
# Slider 2
output$slider2 <- renderUI({
  values <- min((100 - input$slider1),slider2limit)
  sliderInput("slider2", "Search Ads Percentage:", min=0,max=100-input$slider1, value = values)
})

# Slider 3
output$slider3 <- renderUI({
  values <- min((100 - input$slider1 - input$slider2), slider3limit)
  sliderInput("slider3", "Automated Search Ads Percentage: ", min=0,max=100-input$slider1-input$slider2, value = values)
})

output$restable <- renderTable({
  myvals<- c(input$slider1, input$slider2, input$slider3, 100-input$slider1-input$slider2-input$slider3)
  data.frame(Names=c("Banner Ads", "Search Ads", "Automated Search Ads", "Feed Ads"),
             Percentage=myvals)
})

#New data for prediction
newdata <- eventReactive(input$predict,{
  cost <- data.frame("jd_Cost" = input$slider1/100*input$totalinvestment,
              input$slider2/100*input$totalinvestment,
              input$slider3/100*input$totalinvestment,
              (100-input$slider1-input$slider2-input$slider3)/100*input$totalinvestment)
  names(cost) <- c("jd_Cost","ep_Total Cost","shop_Total Cost","con_Total Cost")
  cbind(d1, cost)
})

# Temporary
results <- eventReactive(input$predict,{
  
  pred<- predict(gb, newdata=newdata())
  result <- round(as.numeric(pred), 4)
})


#Output box              
output$approvalBox <- renderInfoBox({
    
  result <- results()
    if (result >= 0) {
      result <- paste(result * 100, '%.')
      infoBox(
        paste(
          # 'With an investment of ', input$totalinvestment, "that consists of ",
          # input$slider1, "% of Banner Ads (Zhanwei) ",
          # input$slider2, "% of Search Ads (Kuaiche) ",
          # input$slider3, "% of Automated Search Ads (Haitou) and ",
          # 100-input$slider1-input$slider2-input$slider3, "% of Feed Ads (Chudian). ",
          "The cannibalization rate will be ", result
        )
        ,
        subtitle = ".",
        icon = icon("thumbs-down", lib = "glyphicon"),
        color = "red",
        fill = TRUE
      )
    } else {
      result <- paste(result * 100, '%')
      valueBox(
        paste(
          # 'With an investment of ', input$totalinvestment, "that consists of ",
          # input$slider1, "% of Banner Ads (Zhanwei) ",
          # input$slider2, "% of Search Ads (Kuaiche) ",
          # input$slider3, "% of Automated Search Ads (Haitou) and ",
          # 100-input$slider1-input$slider2-input$slider3, "% of Feed Ads (Chudian). ",
          "The cannibalization rate will be ", result
        )
        ,
        subtitle = ".",
        icon = icon("thumbs-up", lib = 'glyphicon'),
        color = "green"
      )
    }
})

}

shinyApp(ui, server)

