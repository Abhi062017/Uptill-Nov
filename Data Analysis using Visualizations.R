#Data Analysis using Visualizations
#Base Plots used: hist, plot
#lattice package plots used: histogram,densityplot,bwplot,boxplot,xyplot
#ggplot2 plots used: ggplot, qplot
getwd()

data("mtcars")
str(mtcars)
class(mtcars)
names(mtcars)
head(mtcars)
summary(mtcars$mpg)
IQR(mtcars$mpg) 
#3rd Quarter - 1st Quarter (= values inside the box of boxplot, hence it's used to find outliers)
#Upper Outlier =  3/2*Upper Quartile(75%)
#Lower Outlier = 3/2*Lower Quartile(25%)

#****************************************************
#Base plots:
#****************************************************
colors() #lists out the available colors
par(bg = 'lightyellow3') #sets the background color for plots
hist(mtcars$mpg,
     breaks = 10,
     col = 'cyan4',
     xlab = 'Miles/Gallon',
     ylab = 'Freq'
     )
#****************************
plot(iris$Sepal.Length,
     col = 'red',
     type = 'h'
     )

plot(iris$Sepal.Width,
     col = 'cyan4',
     type = 'h'
     )

#****************************************************
#Lattice plots:
#****************************************************
library(lattice)
histogram(mtcars$mpg,
          breaks = 10,
          col = 'cyan4',
          xlab = "Miles/Gallon", 
          ylab = "Freq"
          )
#*********************************
densityplot(mtcars$mpg,
            col = 'blue',
            xlab = "Miles/Gallon", 
            ylab = "Freq"
            )
#*********************************
boxplot(mtcars$mpg ~ mtcars$cyl,
        col = 'cyan4',
        xlab = "Miles/Gallon", 
        ylab = "Freq"
        )

boxplot(iris$Sepal.Length ~ iris$Species, 
        main = 'Sepal Length of Iris',
        ylab = 'Sepal Length',
        xlab = 'Species',
        col = 'orange3'
        )

boxplot(iris$Petal.Length ~ iris$Species,
        main = 'Petal Length of Iris',
        ylab = 'Petal Length',
        xlab = 'Species',
        col = 'yellow2'
        )

boxplot(iris$Sepal.Width ~ iris$Species,
        main = 'Sepal Width of Iris',
        ylab = 'Sepal Width',
        xlab = 'Species',
        col = 'turquoise'
        )

boxplot(iris$Petal.Width ~ iris$Species,
        main = 'Petal Width of Iris',
        ylab = 'Petal Width',
        xlab = 'Species',
        col = 'lightgreen'
        )

#******************************************
bwplot(iris$Sepal.Length ~ iris$Species,
       main = 'Sepal Length across Species',
       ylab = 'Sepal Length',
       xlab = 'Species',
       col = 'orange'
       )
bwplot(iris$Petal.Length ~ iris$Species,
       main = 'Petal Length across Species',
       ylab = 'Petal Length',
       xlab = 'Species',
       col = 'turquoise2'
       )
bwplot(iris$Sepal.Width ~ iris$Species,
       main = 'Sepal Width across Species',
       ylab = 'Sepal Width',
       xlab = 'Species',
       col = 'navy'
       )
bwplot(iris$Petal.Width ~ iris$Species,
       main = 'Petal Width across Species',
       ylab = 'Petal Width',
       xlab = 'Species',
       col = 'red3'
       )

#*********************************************
data("environmental")
str(environmental)
library(dplyr)
glimpse(environmental)

xyplot(environmental$ozone ~ environmental$wind, col = 'magenta')

xyplot(iris$Sepal.Width ~ iris$Species,
       main = 'Sepal Length by Species',
       xlab = 'Species',
       ylab = 'Sepal Length',
       col = 'magenta',
       pch=20
       )

#*****************************************************
#plotting the features using ggplot() and qplot()
#*****************************************************
library(ggplot2)
table(iris$Sepal.Length, iris$Species)

ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
    geom_bar() +
    facet_wrap(~ Species) +
    ggtitle("Sepal Length of Iris' Species") +
    ylab('Frequency') +
    labs(fill = 'Species')

ggplot(iris, aes(x = Petal.Length, fill = Species)) +
    geom_bar() +
    facet_wrap(~ Species) +
    ggtitle("Petal Length of Iris' Species") +
    ylab('Frequency') +
    labs(fill = 'Species')

ggplot(iris, aes(x = Sepal.Width, fill = Species)) +
    geom_bar() +
    facet_wrap(~ Species) +
    ggtitle("Sepal Width of Iris' Species") +
    ylab('Frequency') +
    labs(fill = 'Species')

ggplot(iris, aes(x = Petal.Width, fill = Species)) +
    geom_bar() +
    facet_wrap(~ Species) +
    ggtitle("Petal Width of Iris' Species") +
    ylab('Frequency') +
    labs(fill = 'Species')

#*******************************************************
qplot(x = iris$Species,
      y = iris$Sepal.Length,
      data = iris,
      geom = 'boxplot',
      main = 'Sepal Length across Species',
      xlab = 'Species',
      ylab = 'Sepal Length'
      )

qplot(x = iris$Species,
      y = iris$Petal.Length,
      data = iris,
      geom = 'boxplot',
      main = 'Petal Length across Species',
      xlab = 'Species',
      ylab = 'Petal Length'
      )

qplot(x = iris$Species,
      y = iris$Sepal.Width,
      data = iris,
      geom = 'boxplot',
      main = 'Sepal Width across Species',
      xlab = 'Species',
      ylab = 'Sepal Width'
      )

qplot(x = iris$Species,
      y = iris$Petal.Width,
      data = iris,
      geom = 'boxplot',
      main = 'Petal Width across Species',
      xlab = 'Species',
      ylab = 'Petal Width'
      )
#****************************************************