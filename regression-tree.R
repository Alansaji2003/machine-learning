#decision trees
#top down greedy approach
#for regression tree


#import dataset
movie <- read.csv("C:/Users/koick/OneDrive/Desktop/Alan Saji/Files_Dt_r/Files/Movie_regression.csv")
View(movie)

#missing value imputation
#data preprocessing
summary(movie)
#changing na of time taken variable
#filling the na values with mean of all other values
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken,na.rm = TRUE)


# Test-Train Split
install.packages('caTools')
library(caTools)
set.seed(0)
split =sample.split(movie,SplitRatio = 0.8)
train = subset(movie,split == TRUE)
test = subset(movie,split == FALSE)


#install required packages
install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)

#Run regression tree model on train set
regtree <- rpart(formula = Collection~., data = train, control = rpart.control(maxdepth = 3))
#collection is the dependent variable and . represents all the predictor variables
#press F1 on rpart for help on this function

#Plot the decision Tree
rpart.plot(regtree, box.palette="RdBu", digits = -3)
#to plot the tree from the tree information model. box.palette is used to assign colours, digits is the number of significant digits

#Predict value at any point
test$pred <- predict(regtree, test, type = "vector")
#here type is vector because it is regression tree , if it was classification then type would be class

MSE2 <- mean((test$pred - test$Collection)^2)#mean square error
#the lower the mse more accurate the value 

#tree prunning to increase accuracy
#cost complexity pruning or weakest link pruning to reduce the time complexity

#Tree Pruning
fulltree <- rpart(formula = Collection~., data = train, control = rpart.control( cp = 0))
#control parameter or cp is zero therefore it will be a tree of maximum size without any prunning
rpart.plot(fulltree, box.palette="RdBu", digits = -3)

#to find error(xerror) for diff values of cp
printcp(fulltree)
#or
plotcp(regtree)

mincp <- regtree$cptable[which.min(regtree$cptable[,"xerror"]),"CP"]

prunedtree <- prune(fulltree, cp = mincp)
rpart.plot(prunedtree, box.palette="RdBu", digits = -3)

test$fulltree <- predict(fulltree, test, type = "vector")
MSE2full <- mean((test$fulltree - test$Collection)^2)

test$pruned <- predict(prunedtree, test, type = "vector")
MSE2pruned <- mean((test$pruned - test$Collection)^2)




