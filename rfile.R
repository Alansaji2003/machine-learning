2 + 5
#comment
print("first print")
s <- 2
s <- 4
s
#array
y <- c(1,2,4,5)
#or
y <- 1:10
x <- y <- 1:10
#array operations
x+y
z = x*y
#list all values in workspace
ls()
#deleting variables
rm(x)

#removing all
rm(list = ls())
#browse
browseURL()
#install package
install.packages("LiblineaR")
#to see all installed packages
library() 
search() #loaded packages
require("LiblineaR")
search()
#to remove from loaded
detach("package:LiblineaR", unload=TRUE)
remove.packages("LiblineaR")
? ggplot2
?? ggplot2
install.packages("ggplot2")
#Inputiing data
#R build in data sets

#to view
data()
#or 
library(help="datasets")
? iris
#to see structure of dataset
str(iris)
iris
data(iris)
#entering data manually
x <- seq(5,50, by = 5)
#to input through console
x4 <- scan()
#importing from csv or text file
#csv fullform : comma seperated values
product <- read.table("C:/Users/moham/OneDrive/Desktop/Alan saji/Complete+Machine+Learning+with+R+Studio+-+ML+for+2022/Complete Machine Learning with R Studio - ML for 2022/Data Files/Product.txt", header = TRUE, sep="\t")
str(product)


#csv
customer <- read.csv("C:/Users/moham/OneDrive/Desktop/Alan saji/Complete+Machine+Learning+with+R+Studio+-+ML+for+2022/Complete Machine Learning with R Studio - ML for 2022/Data Files/Customer.csv")
#TO LOOK AT the data
customer
#to see all the observations
View(customer)
#creating frequenct distribution of a particular variable
#barplots

y <- table(customer$Region)
y
barplot(y)
#parameters of the barplot
#acending and decending order
barplot(y[order(y)])
barplot(y[order(-y)]) #decending
#changing orientation
barplot(y[order(y)], horiz=TRUE)
#changing color
barplot(y[order(y)], horiz = TRUE, col ="red")
#concatination for different colours
barplot(y[order(y)], horiz = TRUE, col = c("red", 589, "green", "yellow"))
#to see all colours
colors()

#to remove black boundary
barplot(y[order(y)], border=NA, col = c(121,343,134,667))
#to add a title
barplot(y[order(y)], main = "region plot")
#x label and y label
barplot(y[order(y)], xlab = "region", ylab = "number of costomers")
#to export this graph
png(savedfromr="C:/Users/moham/OneDrive/Desktop/Alan saji", width = 800, height = 600)
barplot(y[order(y)], horiz = TRUE, col = c("red", 589, "green", "yellow"))
dev.off()


#Creating Histogram
hist(customer$Age)
#to change the number of buckets
hist(customer$Age, breaks = 5)
#to change the width
hist(customer$Age, breaks = c(0,20,40,60,80))

#house pricing analysis
house <- read.csv("C:/Users/moham/OneDrive/Desktop/Alan saji/Complete+Machine+Learning+with+R+Studio+-+ML+for+2022/Complete Machine Learning with R Studio - ML for 2022/Data Files/1 Preprocessing and Linear Regression Dataset/House_Price.csv")
house
View(house)
str(house)
#for univariate analysis
summary(house)
#analysing crime_rates
hist(house$crime_rate)
#scatter plots
#simultaniously getting scatter plots for variables 
pairs(~price+crime_rate+n_hot_rooms+rainfall, data = house) 

barplot(table(house$waterbody))
barplot(table(house$airport))        
barplot(table(house$bus_ter))

#observations
#nhotrooms and rainfall has outliers
#nhosbeds has missing values
#bus terminal is a uselesss variable
#crime rate has some other relationship between price
#using capping and flooring on n hot rooms
quantile(house$n_hot_rooms, 0.99)
uv <- 3*quantile(house$n_hot_rooms, 0.99) #upper value
#making all the values grater than the upper value(uv) to uv
house$n_hot_rooms[house$n_hot_rooms > uv] <- uv
summary(house$n_hot_rooms)
#finding lower value of rainfall
lv <- 0.3*quantile(house$rainfall, 0.01)
house$rainfall[house$rainfall<lv] <- lv
summary(house$rainfall)
#now the mean and median values are close enough for rainfall and n hot rooms


#missing value treatment for n hos beds
#with mean values
#if i write 
mean(house$n_hos_beds)
#we will get NA
#we can remove na
mean(house$n_hos_beds, na.rm = TRUE)
#to find which all positions have NA
which(is.na(house$n_hos_beds))
#shows all positions which have NA values

#now we put all the na  values with mean we calculated above
house$n_hos_beds[is.na(house$n_hos_beds)] <- mean(house$n_hos_beds, na.rm = TRUE)
mean(house$n_hos_beds)
which(is.na(house$n_hos_beds))
summary(house$n_hos_beds)

#seasonality in data

#now we are trying to change the relationship between crime rate and price to be a more linear one
#first we see scatter plot
pairs(~price+crime_rate, data = house)
#we can also do like this
plot(house$price, house$crime_rate)
#this graph looks like its a logarithmic relationship which is rotated
#so we take log
#lot of crime rates is zero and log of zero is negative infinity so we add one to the crime rate then take the log
house$crime_rate <- log(1+house$crime_rate)
house
plot(house$price, house$crime_rate)
#since we took a log of a variable so after creating the model we have to handle it
#now we will transform then 4 distance variable(distance to nearest employment hub) to one variable
#since this variable not present in table it will be created when we write this
house$avg_dist = (house$dist1+house$dist2+house$dist3+house$dist4)/4
#deleting 4 variables
house <- house[,-7:-10] #the first variable is for the number of rows since we want to remove all rows no need to specify anything just comma(,) then its coloumns , to delete we use negetive sign
#if we are not sure  about what will be deleted put that in some other variable first
#now we delete the categorical variable bus_ter because it only has one value yes
house <- house[,-14] #14 is the position of bus_ter coloumn


#now we change the non numerical values in the categorical variables to numerical values with dummy variables
#airport 2 categories yes and no
#waterbodies 4 (ribver lake etc)
#we need to install a pckage
install.packages("fastDummies")

house <- dummy_cols(house)
View(house)
# we can see dummy variables but we dont need all of them if we have n categories we only need n - 1 variable
#we can remove other
house <- house[,-16]
#we delete waterbody none too
house <- house[,-19]


#now we find the correlation matrix
#we can detrmine which variables affect our price the most
cor(house)
round(cor(house),2)
#now when you look at the data we can see that poor prop and room no has high correlation with price
#we only check the first coloumn for finding the correlation with independant variabe  that is price
#now we check if any values are > than 0.8 or < 0.8 ie highly correlated values.
#and we see that parks and air quality are highly correlated, since this will cause multicollinearity
#we remove parks because air quality is more correlated with price
house <- house[,-14]
View(house)
house <- house[,-9]
house <- house[,-11]

simple_model <- lm(price~room_num, data = house)
summary(simple_model)


multiple_model <- lm(price~., data = house)
summary(multiple_model)
#train test split


install.packages("caTools")
set.seed(0)
split <- sample.split(house, SplitRatio = 0.8)
training_set = subset(house, split==TRUE)

test_set = subset(house, split == FALSE)


#RUNNING LENEAR MODEL
lm_a <- lm(price~., data = training_set)
summary(lm_a)
#we need to predict to calculate MSE
train_a <- predict(lm_a, training_set)


test_a <- predict(lm_a, test_set)


#to get mean square
mean((training_set$price- train_a)^2)
#20.66 is the MSE on the training data
#for test data 
mean((test_set$price - test_a)^2)
#since test data is previously unseen there might be bigger error
#it is 33.04
#this error can be used when we are comparing diffrent models


#assignment car data
install.packages("readxl")
cars <- read_excel("C:/Users/moham/Downloads/Cardata.xlsx")

summary(cars)
View(cars)
str(cars)
pairs(~mpg+cyl+disp+HP+wt+accel, data = cars)
barplot(table(cars$cyl))
hist(cars$cyl)
summary(cars)
cars$disp <- -(cars$disp)
cars$wt <- -cars$wt
cars$HP <- -cars$HP
round(cor(cars),2)



#this is not working
model <- lm(mpg~.,data = cars)
summary(model)
predict_car<- predict(model, cars)

