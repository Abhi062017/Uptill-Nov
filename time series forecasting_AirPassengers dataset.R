#Time Series forecasting on AirPassengers
data("AirPassengers")
glimpse(AirPassengers)
class(AirPassengers)
View(AirPassengers)


#Separate the AirPassenger data into a training set and test set, of 80/20 split,
#using subset.
len80 <- floor(0.8*length(AirPassengers))
train <- subset(AirPassengers, end = len80)
test <- subset(AirPassengers, start = (len80+1), end = length(AirPassengers))

#Predict the next five observations of the train subset,of AirPassengers data set,
#using a simple exponential smoothing model.
train%>%
    HoltWinters(beta = NULL,
                gamma = NULL
                )%>%
    forecast(h=5)

#compute the 80% and 90% confidence intervals around the prediction
train%>%
    HoltWinters(beta = NULL,
                gamma = NULL
                )%>%
    forecast(h=5, level = c(80,90))

#Plot your prediction including the 80% and 90% confidence interval
train%>%
    HoltWinters(beta = NULL,
                gamma = NULL
    )%>%
    forecast(h=5, level = c(80,90))%>%
    autoplot()

#Provide the mean absolute percentage error
#for the prediction five periods out by comparing it to the test subset
#of the  AirPassengers dataset.
prediction <- train%>%
    HoltWinters(beta = NULL,
                gamma = NULL
                )%>%
    forecast(h=5, level = c(80,90))
accuracy(prediction$mean, test)
