#Visualizations overloaded
data(iris)
str(iris)

install.packages('psych')
library(psych)

pairs(iris)
pairs.panels(iris)

table(iris$Species)
pie(table(iris$Species), col = rainbow(3))
nrow(table(iris$Sepal.Length))
barplot(table(iris$Sepal.Length), col = rainbow(35), cex.names = 0.5)

#**************************************************************************
install.packages("scatterplot3d")
install.packages("googleVis")
install.packages("rpivotTable")

#1
library(scatterplot3d)
filled.contour(volcano,
               color = terrain.colors,
               asp =1,
               plot.axes = contour(volcano, add = T)
               )

persp(volcano, theta = 25, phi = 30, expand = 0.5, col = 'purple')

#2
library(googleVis)
data("Fruits")
myplot <- gvisMotionChart(Fruits, idvar = 'Fruit', 'Year')
plot(myplot)

data("Population")
names(Population)
myplot2 <- gvisGeoMap(Population,
                      locationvar = 'Country',
                      numvar = 'Population')
plot(myplot2)

#3
library(rpivotTable)
data("mtcars")
names(mtcars)

rpivotTable(mtcars)
