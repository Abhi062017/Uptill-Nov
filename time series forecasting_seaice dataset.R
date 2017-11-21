#Time Series Forecasting
getwd()
#install and load required packages
install.packages(c('xts','lubridate','forecast'))
library(xts)
library(lubridate)
library(forecast)
library(ggplot2)

#Reading the dataset:
seaice <- read.csv('seaice.csv', stringsAsFactors = FALSE)
str(seaice)
library(dplyr)
glimpse(seaice)
head(seaice)
View(seaice)

#I).Data Handling:
    #Filter data to only the northern hemisphere
    table(seaice$hemisphere)
    seaice <- seaice[seaice$hemisphere == 'north',]
    #Adding a variable
    seaice$Date <- as.Date(paste(seaice$Year,
                                 seaice$Month,
                                 seaice$Day,
                                 sep = '-'
                                 )
                           )
    
    glimpse(seaice)
    tail(seaice$Date)

    #Step1).Data Conversion (DataFrame to 'xts')
        #Leverage the xts package to create the initial time series object
        #as the observations in the original dataset are measured every 2 days
        help(package = 'xts')
        names(seaice)
        seaice.xts <- xts(x = seaice$Extent, order.by = seaice$Date)
        seaice.xts
        class(seaice.xts) #Remember: ts<zoo<xts
        glimpse(seaice.xts)
        
        #Using xts package, aggregate the data to the month level,
        #averaging the 'Extent' values for each month
        glimpse(seaice)
        seaice.monthly <- apply.monthly(seaice.xts, mean)
        seaice.monthly
        class(seaice.monthly)
        glimpse(seaice.monthly)
        length(seaice.monthly)


    #Step2).Splitting data to prepare our model:
        #In time series analysis, it is common practice to split the data in 80/20 splits
        names(seaice)
        seaice.end <- floor(0.8*length(seaice.monthly))
        seaice.train <- seaice.monthly[1:seaice.end] #80%
        class(seaice.train)
        
        seaice.test <- seaice.monthly[(seaice.end+1):length(seaice.monthly)] #20%
        length(seaice.test)
        length(seaice.train)


    #Step3).Data conversion('xts' to 'ts'):
        #Many of the visualizations/functions work best/only with R 'tx' objects,
        #so, convert 'xts' train/test datasets to 'ts' objects.
        
        #Step3.1) train data
        seaice.start <- c(year(start(seaice.train)),month(start(seaice.train)))
        seaice.end <- c(year(end(seaice.train)),month(end(seaice.train)))
        seaice.start
        seaice.end
        
        seaice.train <- ts(data = as.numeric(seaice.train), #as.numeric() displays just the numeric values
                           start = seaice.start,            #vector of 2 values: year and month
                           end = seaice.end,                #vector of 2 values: year and month
                           frequency = 12                   #a year changes every 12 months
                           )
        seaice.train
        length(seaice.train) #372 months
        
        #Step3.2) test data
        seaice.start <- c(year(start(seaice.test)),month(start(seaice.test)))
        seaice.end <- c(year(end(seaice.test)), month(end(seaice.test)))
        
        seaice.test <- ts(data = as.numeric(seaice.test),
                          start = seaice.start,
                          end = seaice.end,
                          frequency = 12
                          )
        seaice.test
        length(seaice.test) #93 months

#II).Developing Models:
    #Step1).Decomposing the time-series:
        #We see that we have a very cyclical component to the times series,
        #that repeats on a yearly basis. This is what is anticipated when dealing with
        #yearly patterns. Let us see if we can decompose the time series
        #into its components.
        
        #Use a variable to track the forecast horizon
        #This will be how far out we will need our training model to predict,
        #in order to compare with our observed values for evaluating the model accuracy.
        forecast.horizon <- length(seaice.test)
        
        library(TTR) #helps in decomposing the time-series data in components
        seaice.train.components <- decompose(seaice.train) #it breaks the dataset in 4 components
        seaice.train.components[[6]]
        class(seaice.train.components)
        
        plot(seaice.train.components) #notice the downward 'trend', and very prevalent 'seasonal' component
        
        diff_seasonal <- seaice.train - seaice.train.components$seasonal
        diff_trend <- seaice.train - seaice.train.components$trend
        
        diff_seasonal[372]
        seaice.train[10]
        diff_trend[10]

    #Step2).Exponential Smoothing:
        #The HoltWinters function utilizes an exponential smoothing model,
        #to forecast future observations.
        
        #The forecast function must be used as well,
        #to predict out to the desired forecast horizon.
        seaice.train.esforecast <- HoltWinters(seaice.train,
                                               beta = TRUE,
                                               gamma = TRUE
                                               ) %>% forecast(h=forecast.horizon)
        
        #Take a look at what values are computed:
        #Note that the range for the forecast only consists of actual values,
        #over the time period of the training set
        head(seaice.train.esforecast$fitted)
        tail(seaice.train.esforecast$fitted)
        
        #Predicting future observations using forecast()
        seaice.train%>%
            HoltWinters(beta = TRUE,
                        gamma = TRUE
                        ) %>%
            forecast(h=forecast.horizon) %>%
        plot()
        lines(seaice.test, col = 'red')

    #Step3).Moving average forecasting:
        #The ma function computes a moving average smoother of a given time series.
        #It averages the nearest 'k' periods of each observation where 'k' is specified,
        #using the order. The function uses the knowledge that neighbouring observations,
        #of a time series are likely to be roughly equivalent in value,
        #therefore the moving average dampens some of the randomness in the data.
        seaice.train%>%
            ma(order = 3, centre = TRUE)%>%  #this calculates moving average
            forecast(h=forecast.horizon)%>%
            plot()
        lines(seaice.test, col = 'red')
        

    #Step4).Autoforecast:
        #Sending a 'ts' object into the forecast function,
        #without specifying the model to use,
        #forecast will default to the 'ets' or 'stl'
        #depending on the frequency of the time series.
        seaice.train%>%
            forecast(h=forecast.horizon)%>%
            plot()
        lines(seaice.test, col = 'red')
        
#III).Evaluating Forecast Accuracy:
        #The 'accuracy' function from the forecast package,
        #allows for easy calculation of several common metrics
        accuracy(seaice.train.esforecast, seaice.test)
