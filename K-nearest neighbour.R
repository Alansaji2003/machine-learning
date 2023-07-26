#KNN  - k NEAREST neighbours
#install package
install.packages("class")
df <- read.csv("C:/Users/moham/OneDrive/Desktop/Alan saji/Pro-house-for-classif.csv", header = TRUE)

#activate class
#activate caTools


split <- sample.split(df, SplitRatio = 0.8)
#train will be true
train_set = subset(df, split == TRUE)

test_set = subset(df, split == FALSE)

#first two parameters of KNN
trainX = train_set[, -16]
testX = test_set[, -16]


#THird parameter is a vecter containing class labels for training observation (y variable for training observation) (dependend variable)
trainY = train_set$Sold

testY = test_set$Sold


#the fourth values is K value
k = 3 #any value but 3 is used usually
#since knn use distance its imp that we standardise these variable so every thing has same impact
#to standadise

trainX_s = scale(trainX)
testX_s = scale(testX)

set.seed(0)#to get same random values if same classes came in K 

knn.pred = knn(trainX_s, testX_s, trainY, k =k)

table(knn.pred, testY)


