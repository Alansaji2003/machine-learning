#classification tree
#import dataset

df <- read.csv("C:/Users/koick/OneDrive/Desktop/Alan Saji/Files_Dt_r/Files/Movie_classification.csv")
View(df)


#Data Preprocessing
summary(df)
df$Time_taken[is.na(df$Time_taken)] <- mean(df$Time_taken,na.rm = TRUE)

# Test-Train Split
install.packages('caTools')
library(caTools)
set.seed(0)
split =sample.split(movie,SplitRatio = 0.8)
trainc = subset(df,split == TRUE)
testc = subset(df,split == FALSE)

#install required packages
install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)

#classsification model , notice method = class
classtree <- rpart(formula = Start_Tech_Oscar~., data = trainc, method = 'class', control = rpart.control(maxdepth = 3))


#Plot the decision Tree
rpart.plot(classtree, box.palette="RdBu", digits = -3)

#Predict value at any point
testc$pred <- predict(classtree, testc, type = "class")

table(testc$Start_Tech_Oscar,testc$pred)
#in first row out of 51 movies which got oscar we predicted 41 correctly
#IN SECOND row out of 62 movies which didnt get oscar we predicted 24 correctly


63/112 #This many correct predictions


#ensemble tecniques 
#we make a number of training sets by choosing random values from the original training set
#bagging


install.packages('randomForest')
library (randomForest)
set.seed (0)

bagging =randomForest(formula = Collection~., data = train ,mtry=17)#here mtry = 17 means
#the number of predictor variables in each training set is 17(if its less its random forest)
test$bagging <- predict(bagging, test)
MSE2bagging <- mean((test$bagging - test$Collection)^2)
#as you can see the mse2bagging is 52... which has significant accuracy compared to other models

#Random forest
install.packages('randomForest')
library(randomForest)

randomfor <- randomForest(Collection~., data = train,ntree=500)
#Predict Output 
test$random <- predict(randomfor, test)
MSE2random <- mean((test$random - test$Collection)^2)
#random forest is better accuracy till now

