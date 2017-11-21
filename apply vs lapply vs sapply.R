#apply vs lapply vs sapply:

#apply():
#takes list/vector/df as input and returns list/vector/df as output.

#lapply():
#takes list/vector/df as input and returns only list as output.

#sapply():
#takes list/vector/df as input and returns only vector as output.


x <- matrix(11:22, 3,2)
y <- matrix(1:4, 4,2)
z <- matrix(100:111, 6,2)
matrix.list <- list(x,y,z)
matrix.list

apply(z,1,sum) #sums the rows
apply(z,2,sum) #sums the columns
apply(z,1, function(x) x/2) #displayed column wise
apply(z,2, function(x) x/2) #displayed row wise
apply(z,1:2, function(x) x/2) #displayed row wise
apply(z,2:1, function(x) x/2) #displayed column wise

lapply(matrix.list, function(matrix.list) matrix.list/2)
lapply(matrix.list, mean)
lapply(matrix.list, IQR)

sapply(matrix.list, function(matrix.list) matrix.list/2) #note this returns a list
sapply(matrix.list, mean)
sapply(matrix.list, IQR)
x <- c('This','is','a','random', 'variable')
x
class(x)
sapply(x, nchar)
