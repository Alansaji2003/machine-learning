
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


movie <- read.csv("C:/Users/saji/Desktop/Alan Saji ML/Complete Machine Learning with R Studio - ML for 2022/Data Files/3 Decision Tree Dataset/Movie_regression.csv")
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken, na.rm = TRUE)
split2 = sample.split(movie, SplitRatio = 0.8)
train = subset(movie, split == TRUE)
test = subset(movie, split == FALSE)



train$X3D_available <- as.factor(train$X3D_available)
test$X3D_available <- as.factor(test$X3D_available)
train$Genre <- as.factor(train$Genre)
test$Genre <- as.factor(test$Genre)

install.packages('randomForest')
library (randomForest)
set.seed (0)

bagging =randomForest(formula = Collection~., data = train ,mtry=17, na.action=na.roughfix)#here mtry = 17 means
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


#boosting
#here the trees are grown seqquencially that is each tree is grown from information of previous tree
#gradient boost, ada boost , xg boost

#gradient boost
#slow learning procedure, the tree is adjusted with residuals after each branch
#residual = prediction - actual


#adaboost - adaptive boosting, we create a tree , we nd predictions, wherever its missclassified
#we increase the importance of the misclassified oness
#the second tree will try to predict the misclassified observations correctly
#the second tree will be lil bit diff
#again same steps again

#XGboost
#similar to gradient, we use a regulalarsed model to prevent over fitting
#methods such as lasso and ridge regression use regularization

#implememtation - gradient boost
install.packages('gbm')
library(gbm)
set.seed(0)





boosting = gbm(Collection~., data = train, distribution = 'gaussian', n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)
#distribution is gaussian for regression and its bernoulli for classification, n trees is no. of trees or iterations and interaction depth is number of levels in intermediate trees,
#shrinkage parameter determines the learning rate of our model(low means slow), verbose means at each iteration we wont get output, we get only final output
test$boost <- predict(boosting, test, n.trees = 5000)
MSE2boost <- mean((test$boost - test$Collection)^2)
#mean square is as good as bagging in this scenario, lesser the mse , better the model

#Ada boosting,  we can only use it for classification data
install.packages("adabag")
library(adabag)

trainc$Start_Tech_Oscar <- as.factor(trainc$Start_Tech_Oscar)

adaboost <- boosting(Start_Tech_Oscar~., data = trainc, boos = TRUE, mfinal = 1000)

predada <- predict(adaboost, testc)
table(predada$class, testc$Start_Tech_Oscar)
31+42
73+19+15
73/107

#to plot trees
t1 <- adaboost$trees[[1]]
plot(t1)
text(t1, pretty=100)


#XG boost
install.packages("xgboost")
library(xgboost)

#we need all boolean values in xgboost
trainY <- trainc$Start_Tech_Oscar == "1"

#easier way of making dummy variables
trainX <- model.matrix(Start_Tech_Oscar~.-1, data = trainc )
#delete additional variables
trainX <- trainX[,-12]


testY <- testc$Start_Tech_Oscar == "1"


testX <- model.matrix(Start_Tech_Oscar~.-1, data = testc)
#delete additional variables
testX <- testX[,-12]

#xgboost take input as dmatrix
testX <- testX[,-21] #there was an extra variable pred1 on it idk how so deleted it
xmatrix <- xgb.DMatrix(data = trainX, label = trainY)
xmatrix_t <- xgb.DMatrix(data= testX, label = testY)

xgboosting <- xgboost(data = xmatrix, #the data
                             nround = 75, #max no of boosting iterations
                            objective = "multi:softmax", eta = 0.2, num_class = 2, max_depth = 100)
xgpred <- predict(xgboosting, xmatrix_t)
table(testY , xgpred)
72/107
