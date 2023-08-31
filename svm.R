#support vector machines
#1 - maximal margin classifier - seperable data
#2 - support vector clssifier - non seperable data
#3 - support vector machines - non linear class boundaries

#concept of hyperplane - it divides a p dimentional plane into two parts
#support vector classifier is a soft margin classifier
#we allow some missclassification
#support vector machines use kernels to make non linear boundaries

movie <- read.csv("C:/Users/saji/Desktop/Alan Saji ML/Complete Machine Learning with R Studio - ML for 2022/Data Files/3 Decision Tree Dataset/Movie_classification.csv")

#data preprocessing
summary(movie)
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken, na.rm = TRUE)

#test train split

install.packages("caTools")
library(caTools)

split = sample.split(movie, SplitRatio = 0.8)
trainc = subset(movie, split == TRUE)
testc = subset(movie, split == FALSE)



#CLASSIFICATION
trainc$Start_Tech_Oscar <- as.factor(trainc$Start_Tech_Oscar)
testc$Start_Tech_Oscar <- as.factor(testc$Start_Tech_Oscar)


#install the library e1071

install.packages("e1071")
library(e1071)



svmfit <- svm(Start_Tech_Oscar~., data = trainc, kernel = "linear", cost = 1, scale = TRUE)
summary(svmfit)

#predicting on test set
ypred <- predict(svmfit, testc)
table(predict = ypred, truth= testc$Start_Tech_Oscar)


#to check support vectors
svmfit$index

#finding best value of c/ tuning the hyper parameter

tune.out <- tune(svm, Start_Tech_Oscar~., data = trainc, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10,100)))
bestmod = tune.out$best.model
summary(bestmod)
#we can use this intead of svmfit


#polynomial kernels

svmfitP = svm(Start_Tech_Oscar~., data = trainc, kernel = "polynomial", cost = 1, degree = 2)

#hyperparameter tuning
tune.outP = tune(svm, Start_Tech_Oscar~., data = trainc, cross = 4, kernel = "polynomial", ranges = list(cost=c(0.001,0.1,1,5,10), degree=c(0.5, 1,2,3,5)))
bestmodP = tune.out$best.model
summary(bestmodP)
ypredP <- predict(bestmodP, testc)
table(predict = ypredP, truth = testc$Start_Tech_Oscar)


#radial kernels

svmfitR <- svm(Start_Tech_Oscar~., data = trainc, kernel = "radial", gamma = 1, cost = 1 )
tune.outR <- tune(svm,Start_Tech_Oscar~., data = trainc, kernel ="radial", ranges = list(cost = c(0.001, 0.01,0.1,1,10,100,1000), gamma = c(0.01, 0.1, 0.5, 1,2,3,4,10,50)), cross = 4)
bestmodR <- tune.outR$best.model

ypredR <- predict(bestmodR, testc)
table(ypredR, testc$Start_Tech_Oscar)
#using the right kernel is very important


#regression SVM 

df <- read.csv("F:/Alan Saji ML/Complete Machine Learning with R Studio - ML for 2022/Data Files/4 Support Vector Machines Dataset/Movie_regression.csv")

#data preprocessing

df$Time_taken[is.na(df$Time_taken)] <- mean(df$Time_taken, na.rm = TRUE)


#test-train split

install.packages('caTools')
library(caTools)
split = sample.split(df, SplitRatio = 0.8)
train = subset(df, split == TRUE)
test = subset(df, split == FALSE)


#import lib e1071
install.packages("e1071")
library(e1071)


svmfit_reg <- svm(Collection~., data = train, kernel = "linear", cost = 0.01, scale = TRUE)
summary(svmfit_reg)

#predicting on test set 
ypred_reg <- predict(svmfit_reg, test)


mse <- mean((ypred_reg - test$Collection)^2)



