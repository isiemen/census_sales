#US Census: Manufacturing and Trade Inventory and Sales
#This project is using the US Census: Manufacturing and Trade Inventory and Sales dataset and 
#creating predictions for the monthly sales for total business.

#Install required packages
if (!require(dplyr)) install.packages('dplyr')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(stringr)) install.packages('stringr')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(caret)) install.packages('caret')
if (!require(tidyr)) install.packages('tidyr')
if (!require(FNN)) install.packages('FNN')
if (!require(purrr)) install.packages('purrr')

library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(caret)
library(tidyr)
library(FNN)
library(purrr)

options(digits = 6)

#US Census dataset url: 
#https://www.census.gov/econ/currentdata/datasets/MTIS-mf.zip

#Download the dataset
dl <- tempfile()
download.file("https://www.census.gov/econ/currentdata/datasets/MTIS-mf.zip", dl)

#access the data files:
original <- read.csv(unzip(dl, "MTIS-mf.csv"),
                     sep = ",", 
                     col.names = c('idx','code','desc','col4','col5','col6','col7'), 
                     fill = TRUE, 
                     blank.lines.skip = TRUE, 
                     nrows =  19400,
                     header = FALSE,
                     stringsAsFactors = FALSE
                  )


#The csv file has 8 tables. The first 7 tables are codes and descriptions, the last table contains
#the actual data. We need to transform it to tidy data.

#NOTE: All of the dollar values are in the millions.

#start by storing all of the codes and desc in their one tables. 
#The Geo Levels table only has one record displaying the code US so it is ignored. 
#The notes and data updated tables are also ignored
categories<-original[3:6,1:3]
colnames(categories)<-c("cat_idx","cat_code","cat_desc")
categories<-categories%>%mutate(cat_idx = as.numeric(cat_idx))

datatypes<-original[9:13,1:4]
colnames(datatypes)<-c("dt_idx","dt_code","dt_desc","dt_unit")
datatypes<-datatypes%>%mutate(dt_idx = as.numeric(dt_idx))

errortypes<-original[16:20,1:4]
colnames(errortypes)<-c("et_idx","et_code","et_desc","et_unit")
errortypes<-errortypes%>%mutate(et_idx = as.numeric(et_idx))

timeperiods<-original[26:364,1:2]
colnames(timeperiods)<-c("per_idx","period")
timeperiods<-timeperiods%>%mutate(period = as.character(period),
                                  per_idx = as.numeric(per_idx))

timeperiods<-timeperiods%>%
  mutate(month = str_extract(period,c("Jan",'Feb','Mar','Apr','May',"Jun",'Jul','Aug','Sep','Oct','Nov','Dec')),
         year = as.integer(str_extract(period,"\\d{4}")))

#Extract the actual data table.
data<-original[371:19400,]
colnames(data)<-c("per_idx","cat_idx","dt_idx","et_idx","geo_idx","is_adj","value")
head(data)
dim(data)

#Assign the correct data types to each column
data<-data%>%mutate(per_idx = as.numeric(per_idx),
                    cat_idx = as.numeric(cat_idx),
                    dt_idx = as.numeric(dt_idx),
                    et_idx = as.numeric(et_idx),
                    geo_idx = as.numeric(geo_idx),
                    is_adj = as.numeric(is_adj),
                    value = as.numeric(value))

#Remove NaN values
data<-data[complete.cases(data), ]

#Use a left join to combine the codes tables with the actual data table.
data<-left_join(data,timeperiods, by = "per_idx")
data<-left_join(data,categories, by = "cat_idx")
data<-left_join(data, datatypes, by = "dt_idx")
data<-left_join(data, errortypes, by = "et_idx")

#Convert to tidy data so that each time period displays the all of the data types and category
#values in one row
data<-data%>%
  unite(dt_desc,cat_desc,col = "dt_cat", sep = ": ")%>%
  unite(dt_code,cat_code, col = "dt_cat_code" , sep = "_")%>%
  select(per_idx, period, month, year, dt_cat_code,value, is_adj, et_idx)%>%
  spread(dt_cat_code,value)%>%
  filter(is_adj == 0, et_idx == 0)%>%
  select(-c('NA_RETAIL','NA_TOTBUS','NA_WHLSLR'))

#Add columns for the previous period's Total Business monthly sales and percent change
data<- data%>%
  mutate(PREV_SM_TOTBUS = lag(SM_TOTBUS),
         PREV_SM_MNFCTR = lag(SM_MNFCTR),
         PREV_SM_RETAIL = lag(SM_RETAIL),
         PREV_SM_WHLSLR = lag(SM_WHLSLR),
         PREV_MPCSM_TOTBUS = lag(MPCSM_TOTBUS),
         PREV_MPCSM_MNFCTR = lag(MPCSM_MNFCTR),
         PREV_MPCSM_RETAIL = lag(MPCSM_RETAIL),
         PREV_MPCSM_WHLSLR = lag(MPCSM_WHLSLR))
  
head(data)

#Partition out the train and validation data set. It is a relatively small dataset so 90% 
#will be used for training
set.seed(1)
temp_index <- createDataPartition(y = data$per_idx, times = 1, p = 0.1, list = FALSE)
train <- data[-temp_index,]
validation <- data[temp_index,]
dim(validation)
dim(train)

#Partition out the test data set from the training data set
temp_index <- createDataPartition(y = train$per_idx, times = 1, p = 0.1, list = FALSE)
train <- train[-temp_index,]
test <- train[temp_index,]
dim(test)
dim(train)

#Remove NaNs
train<-train[complete.cases(train), ]
test<-test[complete.cases(test), ]
validation<-validation[complete.cases(validation), ]

#EDA
head(train)
str(train)
summary(train)

min_sales<-train%>%summarize(min = min(SM_TOTBUS))%>%pull(min)
max_sales<-train%>%summarize(max = max(SM_TOTBUS))%>%pull(max)
avg_sales<-train%>%summarize(avg = mean(SM_TOTBUS))%>%pull(avg)

#Adjusted monthly sales timeseries plot grouped by category
train%>%
  select(period, month, year, SM_MNFCTR, SM_RETAIL, SM_WHLSLR,SM_TOTBUS)%>%
  gather(category, value, c('SM_MNFCTR', 'SM_RETAIL', 'SM_WHLSLR','SM_TOTBUS'))%>%
  filter(!is.na(value))%>%
  mutate(cat_desc = case_when(category == 'SM_MNFCTR' ~ 'Manufacturer', 
                              category == 'SM_RETAIL' ~ 'Retail', 
                              category == 'SM_WHLSLR' ~ 'Merchant Wholesalers',
                              category == 'SM_TOTBUS' ~ 'Total Business'))%>%
  group_by(year, cat_desc)%>%
  summarize(total_value = sum(value))%>%
  ggplot(aes(year,total_value, color = cat_desc))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Total Annual Value in Millions")+
  xlab("Year")+
  ggtitle("Monthly Sales per Year for Each Category")+
  labs(color = "Category")

#Adjusted monthly inventory timeseries plot grouped by category
train%>%
  select(period, month, year, IM_MNFCTR, IM_RETAIL, IM_WHLSLR,IM_TOTBUS)%>%
  gather(category, value, c('IM_MNFCTR', 'IM_RETAIL', 'IM_WHLSLR','IM_TOTBUS'))%>%
  filter(!is.na(value))%>%
  mutate(cat_desc = case_when(category == 'IM_MNFCTR' ~ 'Manufacturer', 
                              category == 'IM_RETAIL' ~ 'Retail', 
                              category == 'IM_WHLSLR' ~ 'Merchant Wholesalers',
                              category == 'IM_TOTBUS' ~ 'Total Business'))%>%
  group_by(year, cat_desc)%>%
  summarize(total_value = sum(value))%>%
  ggplot(aes(year,total_value, color = cat_desc))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Total Annual Value in Millions")+
  xlab("Year")+
  ggtitle("Inventory per Year for Each Category")+
  labs(color = "Category")

#Adjusted total monthly inventory and sales timeseries plot for Total Business
train%>%
  select(period, month, year, SM_TOTBUS,IM_TOTBUS)%>%
  gather(category, value, c('SM_TOTBUS','IM_TOTBUS'))%>%
  filter(!is.na(value))%>%
  mutate(cat_desc = case_when(category == 'SM_TOTBUS' ~ 'Sales',
                              category == 'IM_TOTBUS' ~ 'Inventory'))%>%
  group_by(year, cat_desc)%>%
  summarize(total_value = sum(value))%>%
  ggplot(aes(year,total_value, color = cat_desc))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Total Annual Value in Millions")+
  xlab("Year")+
  ggtitle("Total Value per Year for Sales and Inventory")+
  labs(color = "Type")

#Adjusted inventory/sales ratio timeseries plot grouped by category
train%>%
  select(period, month, year, IR_MNFCTR, IR_RETAIL, IR_WHLSLR,IR_TOTBUS)%>%
  gather(category, value, c('IR_MNFCTR', 'IR_RETAIL', 'IR_WHLSLR','IR_TOTBUS'))%>%
  filter(!is.na(value))%>%
  mutate(cat_desc = case_when(category == 'IR_MNFCTR' ~ 'Manufacturer', 
                              category == 'IR_RETAIL' ~ 'Retail', 
                              category == 'IR_WHLSLR' ~ 'Merchant Wholesalers',
                              category == 'IR_TOTBUS' ~ 'Total Business'))%>%
  group_by(year, cat_desc)%>%
  summarize(total_value = sum(value))%>%
  ggplot(aes(year,total_value, color = cat_desc))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "none")+
  ylab("Ratio")+
  xlab("Year")+
  ggtitle("Adjusted Inventory to Sales Ratio per Year for Each Category")+
  facet_wrap(.~cat_desc,nrow=2,ncol=2)

#I want am going to explore years of previous declines since we are in a drastic decline 
#in both inventory and sales. 
annual_sales<-train%>%
  select('year','SM_TOTBUS')%>%
  filter(!is.na(SM_TOTBUS))%>%
  group_by(year)%>%
  summarize(total=sum(SM_TOTBUS))

as.data.frame(annual_sales)

#The sales data shows temporary declines in 2004 and 2015, and a prolonged decline between 2008-2010
#Plot the monthly sales for the year 2004 2015
train%>%
  select(period, month, year, SM_MNFCTR, SM_RETAIL, SM_WHLSLR)%>%
  gather(category, value, c('SM_MNFCTR', 'SM_RETAIL', 'SM_WHLSLR'))%>%
  filter(!is.na(value),
         year %in% c(2004,2005,2015,2016))%>%
  mutate(cat_desc = case_when(category == 'SM_MNFCTR' ~ 'Manufacturer', 
                              category == 'SM_RETAIL' ~ 'Retail', 
                              category == 'SM_WHLSLR' ~ 'Merchant Wholesalers'),
         year_group =case_when (year %in% 2004:2005 ~ '2004 - 2005',
                                year %in% 2015:2016 ~ '2015 - 2016'))%>%
  mutate(period_date = as.Date(paste('01', period), format = "%d %b%Y"))%>%
  select(cat_desc,year,value, period_date, year_group)%>%
  group_by(period_date,cat_desc, year_group)%>%
  summarize(total_value = sum(value))%>%
  ggplot(aes(period_date,total_value, color = cat_desc))+
  geom_line()+
  theme_bw()+
  scale_color_manual(values = c("#035789", "#890357", "#578903"))+
  ylab("Total Value in Millions")+
  xlab("Date")+
  ggtitle("Adjusted Monthly Sales in Years of Decline")+
  labs(color = "Category")+
  facet_grid(.~year_group, scales = "free")+  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
#Plot the monthly sales for 2008 - 2010
train%>%
  select(period, month, year, SM_MNFCTR, SM_RETAIL, SM_WHLSLR)%>%
  gather(category, value, c('SM_MNFCTR', 'SM_RETAIL', 'SM_WHLSLR'))%>%
  filter(!is.na(value),
         year %in% 2008:2010)%>%
  mutate(cat_desc = case_when(category == 'SM_MNFCTR' ~ 'Manufacturer', 
                              category == 'SM_RETAIL' ~ 'Retail', 
                              category == 'SM_WHLSLR' ~ 'Merchant Wholesalers'))%>%
  mutate(period_date = as.Date(paste('01', period), format = "%d %b%Y"))%>%
  select(cat_desc,year,value, period_date)%>%
  group_by(period_date,cat_desc)%>%
  summarize(total_value = sum(value))%>%
  ggplot(aes(period_date,total_value, color = cat_desc))+
  geom_line()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Total Value in Millions")+
  xlab("Date")+
  ggtitle("Adjusted Sales 2008-2010 for Each Category")+
  labs(color = "Category")

#Linegraph total sales per month to see if there is seasonality
train%>%filter(!is.na(SM_TOTBUS))%>%
    mutate(month_number = case_when(month=='Jan' ~ 1,
                                    month=='Feb' ~ 2,
                                    month=='Mar' ~ 3,
                                    month=='Apr' ~ 4,
                                    month=='May' ~ 5,
                                    month=='Jun' ~ 6,
                                    month=='Jul' ~ 7,
                                    month=='Aug' ~ 8,
                                    month=='Sep' ~ 9,
                                    month=='Oct' ~ 10,
                                    month=='Nov' ~ 11,
                                    month=='Dec' ~ 12),
           year == as.character(year))%>%
    select(month_number,year, SM_TOTBUS)%>%
    group_by(month_number,year)%>%
    summarize(total_value = sum(SM_TOTBUS))%>%
    ggplot(aes(month_number,total_value, group = year, color = year))+
    geom_line()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ylab("Total Value in Millions")+
    scale_x_continuous(name="Month", breaks=c(1:12))+
    ggtitle("Sales per Month")

#There tends to be an increase in March, decrease in July, and increase in August

#plot correlation between categories
#Total business vs other categories
train%>%filter(!is.na(SM_TOTBUS), 
               !is.na(SM_MNFCTR),
               !is.na(SM_RETAIL),
               !is.na(SM_WHLSLR))%>%
  select(period, SM_TOTBUS, SM_MNFCTR, SM_RETAIL, SM_WHLSLR)%>%
  gather(cat_code, value, c('SM_MNFCTR', 'SM_RETAIL', 'SM_WHLSLR'))%>%
  mutate(category = case_when(cat_code == 'SM_MNFCTR' ~ 'Manufacturer', 
                              cat_code == 'SM_RETAIL' ~ 'Retail', 
                              cat_code == 'SM_WHLSLR' ~ 'Wholesalers'))%>%
  ggplot(aes(x=value,y=SM_TOTBUS, color = category))+
  geom_point()+
  geom_smooth(method = 'loess', formula = y ~x, color = "black")+
  ylab("Total Business Sales")+
  xlab("Sales Values")+
  theme_bw()+
  ggtitle("Total Business Sales vs Other Categories")+
  facet_grid(.~category, scales = "free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

#Merchant Wholesalers have the strongest correlation with the Total Business sales

#Histogram of Sales - Monthly Percent change for Total Business
avg_mpcsm<-train%>%
  filter(!is.na(MPCSM_TOTBUS))%>%
  summarize(avg = mean(MPCSM_TOTBUS))%>%
  pull(avg)

train%>%filter(!is.na(MPCSM_TOTBUS))%>%
  mutate(value = round(MPCSM_TOTBUS))%>%
  ggplot(aes(value, fill = value >= 0))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 75, hjust = 1))+
  theme_bw()+
  xlab("Percent Change")+
  ylab("Number of Months")+
  ggtitle("Distribution of Percent Changes for Monthly Sales")

#Histogram of duration of declines in Sales - Monthly Percent Change
declines<-train%>%
  mutate(decline := MPCSM_TOTBUS < 0)

decline_rle <-rle(declines$decline)
decline_length <-decline_rle$lengths

decline_length<- as.data.frame(decline_length)%>%
  mutate(decline_values = decline_rle$values)%>%
  mutate(type = case_when(decline_values == 'FALSE' ~ "Increase",
                          TRUE ~ "Decrease"))

as.data.frame(decline_length)%>%
  ggplot(aes(decline_length, fill = type))+
  geom_bar(position = "dodge")+
  xlab("Number of Months")+
  ylab("Count")+
  theme_bw()+
  ggtitle("Duration of Increasing and Decreasing Monthly Sales")

#Apply machine learning models: linear model and KNN
#Update the dataframes only include the total business monthly sales (to be predicted), 
#the period, and the columns that store the previous periods' values  
train<-train%>%
  select(SM_TOTBUS,per_idx,PREV_SM_TOTBUS, PREV_SM_MNFCTR, PREV_SM_RETAIL, PREV_SM_WHLSLR, 
         PREV_MPCSM_TOTBUS, PREV_MPCSM_MNFCTR, PREV_MPCSM_RETAIL, PREV_MPCSM_WHLSLR)

test<-test%>%
  select(SM_TOTBUS,per_idx,PREV_SM_TOTBUS, PREV_SM_MNFCTR, PREV_SM_RETAIL, PREV_SM_WHLSLR, 
         PREV_MPCSM_TOTBUS, PREV_MPCSM_MNFCTR, PREV_MPCSM_RETAIL, PREV_MPCSM_WHLSLR)

validation<-validation%>%
  select(SM_TOTBUS,per_idx,PREV_SM_TOTBUS, PREV_SM_MNFCTR, PREV_SM_RETAIL, PREV_SM_WHLSLR, 
         PREV_MPCSM_TOTBUS, PREV_MPCSM_MNFCTR, PREV_MPCSM_RETAIL, PREV_MPCSM_WHLSLR)

#fit a linear model to determine the affect of time periods on the total monthly sales
fit_lm <- lm(SM_TOTBUS ~ per_idx, data = train)
fit_lm$coefficients

#predict the total monthly sales based on the time period
y_hat_lm <- predict(fit_lm, test)

#Use RMSE to evaluate the accuracy###need to confirm the evaluation
#RMSE calculation to evaluate predictions
RMSE <- function(true_ratings, predicted_ratings,n){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

time_RMSE<-RMSE(test$SM_TOTBUS,y_hat_lm)
time_RMSE

#plot Predictions versus actual monthly sales
as.data.frame(y_hat_lm)%>%
  mutate(Predictions = y_hat_lm,
         ActualSales = test$SM_TOTBUS)%>%
  ggplot(aes(Predictions,ActualSales))+
  geom_point()+
  xlab("Predicted Sales")+
  ylab("Actual Sales")+
  theme_bw()+
  ggtitle("Linear Model Results")

#fit a knn model based on the period and previous period's monthly sales and percent change columns.
#Evaluated on the test data set. Set k = 5 neighbors
fit_knn<-knn.reg(train = train, test = test, y = train$SM_TOTBUS, k = 5)
fit_knn$pred

#Evaluate the RMSE
knn_RMSE<-RMSE(test$SM_TOTBUS,fit_knn$pred)
knn_RMSE

#plot Predictions versus actual monthly sales
as.data.frame(fit_knn$pred)%>%
  mutate(Predictions = fit_knn$pred,
         ActualSales = test$SM_TOTBUS)%>%
  ggplot(aes(Predictions,ActualSales))+
  geom_point()+
  xlab("Predicted Sales")+
  ylab("Actual Sales")+
  theme_bw()+
  ggtitle("KNN Model Results")

#Evaluate a range of number of neighbors (K)
ks <- seq(3, 100, 2) #k sequence to test: odd numbers between 3-100
RMSE_results <- map_df(ks, function(k){ #map each value in the sequence ks to the fuction
  fit <- knn.reg(train = train, test = test, y = train$SM_TOTBUS, k = k) #fit the knn value
  test_RMSE<-RMSE(test$SM_TOTBUS,fit$pred)
  tibble(k = k, RMSE = test_RMSE) #create a tibble with all of the train and test accuracy values
})

RMSE_results%>%filter(RMSE == min(RMSE))

#k = 3 returns the lowest RMSE, but I will stick with 5 to prevent over-fitting
#Apply knn model to the validation set
fit_knn_val<-knn.reg(train = train, test = validation, y = train$SM_TOTBUS, k = 5)

#Evaluate the RMSE
knn_RMSE_val<-RMSE(validation$SM_TOTBUS,fit_knn_val$pred)
knn_RMSE_val

#Plot of final predictions for the validation data set
as.data.frame(fit_knn_val$pred)%>%
  mutate(Predictions = fit_knn_val$pred,
         ActualSales = validation$SM_TOTBUS)%>%
  ggplot(aes(Predictions,ActualSales))+
  geom_point()+
  xlab("Predicted Sales")+
  ylab("Actual Sales")+
  theme_bw()+
  ggtitle("Validation Data: KNN Model Results")