#classification models
#logistic regression
#Linear discriminant analysis
#K - nearest neighbours
df <- read.csv("C:/Users/moham/OneDrive/Desktop/Alan saji/Pro-house-for-classif.csv", header = TRUE)
#this is a preproccesd dataof the same boston house price
#here we are goung to predict the categorical variable sold which tells whether the house got sold in 3 months or not
#sold has values 0 and 1
str(df)

#logistic regression in R with just one predictor variable price
glm_fit = glm(Sold~price, data = df, family = binomial )
summary(glm_fit)

#multiple predictor variables

glm.fit = glm(Sold~., data = df, family = binomial )

rm(multi_glm_fit)

#we use this model to calculate the probabilties using the predict function

glm.probs = predict(glm.fit, type = "response")
glm.probs[1:10] #checking the probalities of first  10
#0.85 - means very close to one, meaning first house will be sold in 3 months
#and so on

#now we need to set a boundary above which we will say the house will be sold(YES)
#first we make an array with all NO values


glm.predict = rep("NO", 506)

#now we put the values above 0.5 on glm.probs to yes


glm.predict[glm.probs > 0.5] = "YES"

#creating confusion matrix to see how well our model is predicting the response variable
table(glm.predict, df$Sold)
#here the 79 shows type 1 error ie model predicted yes but its actually no and 81 is type 2 error vice-versa


