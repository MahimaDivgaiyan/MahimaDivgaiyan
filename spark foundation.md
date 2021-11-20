#THE SPARK FOUNDATION

#NAME - MAHIMA DIVGAIYAN ( DATA SCIENCE AND BUSINESS ANALYTICS INTERN)

#TASK NO. 6 :- PREDICTION USING DECISION TREE ALGORITHM

#GRIP NOV 21

#(LEVEL : INTERMEDIATE )

#Create the Decision Tree and visualize it graphically .

library(datasets)
str(iris)


head(iris,5)    #View upper 5 data
dim(Iris)       #total row and column 

names(Iris)    #Name the observation of column

class(Iris)


## Get first 5 rows of each subset


subset(iris,Species == "setosa")[1:8,]
subset(iris, Species=="versicolor")[1:5,]
subset(iris, Species=="virginica")[1:5,]


##summary
summary(iris)

#statistics with box plots

par(mar=c(3,2,1,1))
boxplot(iris,las=2)

Ver<- subset(iris, Species== "versicolor")
set<- subset(iris , Species=="stosa")
Vir<- subset(iris, Species== "virginica")
par(mfrow=c(1,3),mar=c(6,3,2,1))
boxplot(Ver[,1:3], main = "Versicolor" , ylim =c(0,6), las=1)
boxplot(Vir[,1:4], main="Virginica",ylim = c(0,6),las=1)

#vISULIAZATION USING HISTOGRAM

hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)

#VISULIZATION USING PIE CHART
table(iris$Species)
pie(table(iris$Species) , col = c("yellow" ,"red" ,"green" ), radius = 1)


#BEANPLOT PRESENTATION
library(beanplot)
x <- iris

x$Species<- NULL
beanplot(x, main = "Iris flowers",col=c('brown' ,'sky blue','green','blue'), border = "#000000")


print("Covariance between variable.")
cov(Iris[ , 1:5])

print("Correlation between variable")
cor(Iris [ , 1:4])



#Multiple Regression

Iris$Species = factor(Iris$Species,
                      levels = c('setosa', 'versicolor', 'virginica'),
                      labels = c(1, 2, 3))

#Splitting the Iris data set into the training set and Test set
library(caTools)
set.seed(123)
split<- sample.split(Iris$Species , SplitRatio = 0.8 )
training_set <- subset(Iris, split== TRUE)
test_set<- subset(Iris, split== FALSE)

#fitting Multiple Linear  Regression



regressor = lm(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, iris)
summary(regressor)




#Correlations between variable

correlation <- cor(iris[,1:4])
round(correlation,3)

pairs(iris[,1:4])

pairs(iris[,1:4],col=iris[,5],oma=c(3,4,2,6))
par(xpd=TRUE)
legend(0.85,0.6, as.vector(unique(iris$Species)),fill=c(1,2,3))



library(MASS)
parcoord(iris[,1:4], col=iris[,5],var.label=TRUE,oma=c(1,2,3))
par(xpd=TRUE)
legend(0.85,0.6, as.vector(unique(iris$Species)),fill=c(1,2,3))


#Classification with Decision Trees


library(C50)
input <- iris[,1:4]
output <- iris[,5]
model1 <- C5.0(input, output, control = C5.0Control(noGlobalPruning = TRUE,minCases=1))
plot(model1, main="Decision Tree(Unpruned)")
model2 <- C5.0(input, output, control = C5.0Control(noGlobalPruning = FALSE))
plot(model2, main="Decision Tree(Pruned)")

summary(model2)
