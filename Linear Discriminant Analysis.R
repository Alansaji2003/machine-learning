#linear discriminant Analysis
#preferred when response variable has more than 2 classes
#based on bayes theorum
#if the predictor variable is normally distribted the LDA will be accurate
#first activate MASS modeule
df <- read.csv("C:/Users/moham/OneDrive/Desktop/Alan saji/Pro-house-for-classif.csv", header = TRUE)

lda_fit <- lda(Sold~., data = df)
lda_fit

#now we store the predicted probabilities using bayes theorm in a vaiable of the diffrent classes of sold
#(now its only 2 , if it was more it would sho tht too)

lda.pred = predict(lda_fit, df)

#we can view it by double clicking on the environment section
#it has 3 lists and class posterior and x , here the posterior contains the probabilties we need

#lets see it
lda.pred$posterior
#by default the boundary is 0.5 above which it is yes or 1
#in the class it has the predicted classes of the observations and in sold it has original values and then we can calculate the confusiion matrix


table(lda.pred$class, df$Sold)
#WE can check and compare with this logistic regression

#to get values above costom boundary limit
sum(lda.pred$posterior[, 1] >  0.8) #the posterior list has two parts 0 class probability and 1 class probabaility and this finds
#probablities of 1 class > 0.8
#qda can also be done in same way



#doing train test split for Logistic regression
#check mark the package caTools


set.seed(0)
split <- sample.split(df, SplitRatio = 0.8)
#train will be true
train_set = subset(df, split == TRUE)

test_set = subset(df, split == FALSE)

#train the model using train set (of Logistic regr)
#will create confusion matrix with test set 

train.fit = glm(Sold~., data = train_set, family = binomial)
#now we will get the predictes probablities of the trained model using the test data
test.probs <- predict(train.fit, test_set, type = 'response')

#now to create confusion matrix on predicted data and original data
#first an array of all no
test.preds <- rep("NO", 120)

#now we will chage the values which has probabilty greater than 0.5 to yes

test.preds[test.probs > 0.5] <- "YES"

table(test.preds, test_set$Sold)

#now doing the same for LDA

lda_split <- sample.split(df, SplitRatio = 0.8)
lda_train_set <- subset(df, lda_split == TRUE)
lda_test_set <- subset(df, lda_split == FALSE)

#training with training set
lda_fit <- lda(Sold~., data =lda_train_set)

lda_probs <- predict(lda_fit, lda_test_set )

table(lda_probs$class, lda_test_set$Sold)







