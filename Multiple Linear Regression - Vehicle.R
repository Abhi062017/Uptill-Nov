#Multiple Linear Regression
getwd()
vehicle <- read.csv('vehicle.csv')
str(vehicle)

library(psych)
pairs.panels(vehicle)

vehicle_plot <- vehicle[,c('Mileage', 'lh','lc')]
pairs.panels(vehicle_plot)
vehicle_plot2 <- vehicle[,c('State', 'lh','lc')]
pairs.panels(vehicle_plot2)

names(vehicle)
results <- lm(lc ~ State+lh, vehicle)
results
summary(results)

full.vehicle <- lm(lc ~. , vehicle)
full.vehicle
summary(full.vehicle)

anova(results, full.vehicle)

prediction <- predict(results, data.frame(State = 'HI', lh = 10), interval = 'confidence')
prediction
