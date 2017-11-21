#svm on iris dataset
library(psych)
pairs.panels(iris)

plot(iris$Petal.Width, iris$Petal.Length, col = iris$Species)
plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species)

#deduction: best relationship is b/w Petal Length and Petal Width

#split
s <- sample(150,100)
col <- c('Petal.Length', 'Petal.Width', 'Species')
train <- iris[s, col]
test <- iris[-s, col]

#Attempt1:
install.packages("e1071")
library(e1071)
svm_model <- svm(Species ~., data = train, kernel = 'linear', scale = FALSE)
print(svm_model)
plot(svm_model, train)

p1 <- predict(svm_model, test,  type = 'class')
table(test[,3], p1)

#Attempt2:

#C is the cost of classification.
#A large C gives you low bias and high variance.
#Low bias because you penalize the cost of missclasification a lot.
#A small C gives you higher bias and lower variance.
#Gamma is the parameter of a Gaussian Kernel (to handle non-linear classification)

tuned <- tune(svm, Species ~., data = train, kernel = 'linear',
              ranges = list(cost = c(0.001,0.01,0.1,1,10,100)))
summary(tuned)
svm_model <- svm(Species ~., data = train, kernel = 'linear', scale = F, cost = 0.1)
print(svm_model)
plot(svm_model, train)

p1 <- predict(svm_model, test, type = 'class')
table(test[,3], p1)

#Attempt3: Using 'non-linear' kernel
tuned <- tune(svm, Species ~., data = train, kernel = 'polynomial', scale = F,
              ranges = list(gamma = c(0.333, 0.555, 0.777, 0.999, 1)))
summary(tuned)

svm_model <- svm(Species ~., data = train, scale = F, kernel = 'polynomial',
                 gamma = 0.333, cost = 0.1)

print(svm_model)
plot(svm_model, train)

p1 <- predict(svm_model, test, type = 'class')
table(p1, test[,3])
