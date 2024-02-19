
#we did regression trees,  now about classification trees
#for classification trees we use mode instead of mean
#we use  it to find most commonly occuring class in that region
#we use recursive binary splitting here also

#IN Classification we can use 1)classification error rate
#2) gini index
#3) cross entropy
#gini index , suppose we have two classes pass and fail
#let p be the probability of passed at a particular node 
#if p is very smal, the node is pure or if p is large(1) the again high purity
#this both makes gini index near to zero
#gini index and cross entropy are more sensitive to node purity

df <- read.csv("C:/Users/saji/Desktop/Alan Saji ML/Complete Machine Learning with R Studio - ML for 2022/Data Files/3 Decision Tree Dataset/Movie_classification.csv")
View(df)

#data preprocessing
summary(df)
df$Time_taken[is.na(df$Time_taken)] <- mean(df$Time_taken, na.rm = TRUE)

#test train split
install.packages('caTools')
library(caTools)
set.seed(0)
split = sample.split(df, SplitRatio = 0.8)
trainc = subset(df, split == TRUE)
testc = subset(df, split == FALSE)

#INSTALL packages reuired
install.packages('rpart')
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

#run classification tree model on train set
classtree <- rpart(formula = Start_Tech_Oscar~., data = trainc, method = 'class', control = rpart.control(maxdepth = 3) )

#to plot the decision tree we use rpart.plot
rpart.plot(classtree, box.palette = 'RdBu', digits = -3)


#predict value at any point
testc$pred <- predict(classtree, testc, type= "class") #for regression we use type = vector
View(testc)

#to compare performance
table(testc$Start_Tech_Oscar, testc$pred)

#when movie is not winning oscar we get heigher accuracy
#when movie is winning the oscar we have a bad prediction accuracy

50+38+19
45+19
64/107
#since a decision tree has high varience we use ensemble techniques to increase the accuracy
#methods are bagging , random forest, and boosting

#bagging uses bootstraping to create random samples from same data and make models on all
#them takes the average of all the predictions by different trees
#that is the final model
#we let the tree to grow full length in these
#random forest is selective bagging

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


