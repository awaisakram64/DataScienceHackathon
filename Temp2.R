library(dplyr)
library(lubridate)
library(plotly)
library(fpp2)
library(xray)
library(highcharter)
#  Read csv file 
dataset <- read.csv('TT2/data2.csv')

# 
summary(dataset)

#  extracting the main columns from dataset
# for time series analysis
time_data <- dataset %>%
  select(Branch_Code,Donation_type,Donation_Date)

#  
summary(time_data)

# converting date column from dataset
time_data$Donation_Date <- as.Date(time_data$Donation_Date, format = "%d-%b-%y")


#  remove the missing values from data 
#  ther is almost 3~4 values are missing in selected data
time_data <- na.omit(time_data)

# check min and max date from dataset
min(time_data$Donation_Date)
max(time_data$Donation_Date)


#  Starting date from the finincial_year which is July 1st
time_data <- time_data %>%
  filter(Donation_Date >= "2013-07-01")


# select data of specific branch 
# because there are different branch in dataset 
# and different count of people goto different branch 
# we select 1st branch
branch_data_fun <-  function(branch){
  brac_data <- time_data %>%
    filter(Branch_Code %in% branch)
}


# call above function that subset data on Branch column
branch_data <- branch_data_fun("BRC-01")

head(branch_data)


# creating a data.frame that contain just the date and no donor that 
# came to specific branch for donation
agg_brac_date <-aggregate(x = branch_data[,3],
  by = list(Date = branch_data$Donation_Date), 
  FUN = length)
  

#  checking if there is any missing date from the 
# sequence which is not included in above data frame
alldates = seq(min(agg_brac_date$Date), max(agg_brac_date$Date), 1)


#  checking if there is some date that is not included in above data frame
dates0 = alldates[!(alldates %in% agg_brac_date$Date)]

# creating a data.frame of missing dates  
data0 <- data.frame()
if(length(dates0)!=0){
  data0 <- data.frame(Date = dates0,x = 0)   
}


# bind the missing date data to orignal data in data.frame
full_data_branch = rbind(agg_brac_date, data0)


# Order the data by date column
full_data_branch = full_data_branch[order(full_data_branch$Date),]

head(full_data_branch)

count(full_data_branch)

# calculating the mean and median of data 
# then find the average of both of them 
# and replace it with the outliers
a <- mean(full_data_branch$x)+median(full_data_branch$x)
a <- a/2

outliers <- boxplot.stats(full_data_branch$x)$out

#  ploting the boxplot to check the outliers
hcboxplot(x = full_data_branch$x) %>%
  hc_chart(type = "column")

# data without outliers
xx <- full_data_branch %>%
  filter(!full_data_branch$x %in% c(outliers))

count(full_data_branch)

#  data with outliers
yy <- full_data_branch %>%
  filter(full_data_branch$x %in% outliers)

# replace outlier with the average of mean and median
yy$x <- a

# bind the clean and smooth data
full_data_branch <- NULL
full_data_branch <- rbind(xx,yy)



#  Creating a ts object on branch code that start from sept 1 2013
my_ts_br <- ts(full_data_branch$x , start = c(2013,7), frequency = 365)

hchart(my_ts_br)


# Dividing the dataset in Train and Test Data
# --------
train <- window(
  my_ts_br,
  end=c(2017,5))

test <- window(
  my_ts_br,
  start=c(2017,6))


# Decomposing time series object to Analyze dataset
decomp <- stl(log(my_ts_br), "per")
ggplotly(autoplot(decomp))


# Plot the time series data 
plot_ly(x = ~full_data_branch$Date, y = ~full_data_branch$x, mode = 'lines') %>%
  layout(
    title = " Sales",
    xaxis = list(
      title = "Date",
      rangeselector = list(
        buttons = list(
          list(
            count = 3,
            label = "3 mo",
            step = "month",
            stepmode = "backward"),
          list(
            count = 6,
            label = "6 mo",
            step = "month",
            stepmode = "backward"),
          list(
            count = 1,
            label = "1 yr",
            step = "year",
            stepmode = "backward"),
          list(
            count = 1,
            label = "YTD",
            step = "year",
            stepmode = "todate"),
          list(step = "all"))),
      
      rangeslider = list(type = "date")),
    yaxis = list(title = "Count"))

# It will use to plot the data over different years
ggplotly(ggseasonplot(my_ts_br,polar = F))

# These are some functions to calculata accuracy
-----------------------------
# Mean Square Error

  
  
  
MSE <- function(y,yhat)
{
  mean((y-yhat)**2)
}

# Mean absolute Error
MAE <- function(y,yhat)
{
  mean(abs(y-yhat))
}

# Mean absolute precentage error
MAPE <- function(y,yhat,percent=TRUE)
{
  if(percent){
    100*mean(abs( (y-yhat)/y ))
  } else {
    mean(abs( (y-yhat)/y ))
  }
}
-------------------------------


models_accuracy <- data.frame(modelname = as.character(), mse=as.character(),mae=as.character(),mape=as.character())


# This is our first model
# Which is time series linear model--------------
linerModel.fit <- forecast(tslm(train ~ trend ), h = length(test))

# ploting the forecasting model
ggplotly(autoplot(linerModel.fit))

# Mean of prediction model
yHat <- linerModel.fit$mean


mse <- MSE(test,yHat)

mae <- MAE(test,yHat)

mape <- MAPE(test,yHat)

models_accuracy <- data.frame(model.name="Linear Model",mse = mse, mae = mae, mape = mape)




# This is our 2nd model
# Which is Stlf model
stlf.fit <- stlf(train,h=length(test),robust = T,lambda = "auto")
ggplotly(autoplot(stlf.fit))

# Mean of prediction model
yHat <- ets.fit$mean

mse <- MSE(test,yHat)

mae <- MAE(test,yHat)

mape <- MAPE(test,yHat)

models_accuracy <- rbind(models_accuracy,data.frame(model.name = "STLF",mse=mse , mae = mae,mape = mape))


# Simple Exponentioal Smoothing --------------------
ses.fit <- ses(train,h = length(test))
ggplotly(autoplot(ses.fit))
yHat <- fcnv$mean

mse <- MSE(test,yHat)

mae <- MAE(test,yHat)

mape <- MAPE(test,yHat)

models_accuracy <- rbind(models_accuracy,data.frame(model.name = "SES",mse=mse , mae = mae,mape = mape))

# simple Forcasting 
forecas.fit <- forecast(train,h = length(test))

ggplotly(autoplot(forecas.fit))

yHat <- forecas.fit$mean

mse <- MSE(test,yHat)

mae <- MAE(test,yHat)

mape <- MAPE(test,yHat)

models_accuracy <- rbind(models_accuracy,data.frame(model.name = "simple forecas",mse=mse , mae = mae,mape = mape))



#  This will predict using neural network
nn.fit <- forecast(nnetar(train, lambda=0.9,),h=length(test))

ggplotly(autoplot(nn.fit))

yHat <- nn.fit$mean

mse <- MSE(test,yHat)

mae <- MAE(test,yHat)

mape <- MAPE(test,yHat)

models_accuracy <- rbind(models_accuracy,data.frame(model.name = "Neural network",mse=mse , mae = mae,mape = mape))
