#install.packages('corrplot')
#install.packages('caret')
#install.packages('gmlnet')
#install.packages('ISLR')
#install.packages('psych')

library(corrplot)
library(caret)
library(glmnet)
library(mlbench)
library(psych)
#Importing the data
kc <- read.csv("/Users/omkar/Omkar/DM&ML/Project/KC Housing/kc_house_data.csv", header = TRUE, sep=",")

#Analyze the data
head(kc)
str(kc)
summary(kc)

#We had decided to keep 'price variable' as our independent variables and we assume rest of other variables are independent variables, 
#We will examine the data first and then decide the dependent variables

#There are two NA's in sqft_above data, we will remove these two NA's from the data
kc <- kc[-which(is.na(kc$sqft_above)),]
summary(kc)

#We will clean the date column
#Date is in format of yyyy/mm/dd. We will need only month column to see if price is associated with month or not.
kc$date <- substr(kc$date,2,nchar(kc$date)-4)
kc$date <- substr(kc$date,2,nchar(kc$date)-2)
kc$date <- substr(kc$date,2,nchar(kc$date)-1)
kc$date <- substr(kc$date,2,nchar(kc$date)-2)
kc$date <- as.integer(kc$date)
names(kc)[names(kc) == "date"] <- "month"
summary(kc$month)


par(mfrow=c(3,5))
hist(kc$price, main = "price", xlab = "price", ylab = "Count", col = "blue", )
hist(kc$sqft_living, main = "sqft_living", xlab = "sqft_living", ylab = "Count", col = "blue")
hist(kc$sqft_lot, main = "sqft_lot", xlab = "sqft_lot", ylab = "Count", col = "blue")
hist(kc$sqft_above, main = "sqft_above", xlab = "sqft_above", ylab = "Count", col = "blue")
hist(kc$sqft_basement, main = "sqft_basement", xlab = "sqft_basement", ylab = "Count", col = "blue")
hist(kc$month, main = "month", xlab = "price", ylab = "Count", col = "blue", )
hist(kc$bedrooms, main = "bedrooms", xlab = "price", ylab = "Count", col = "blue", )
hist(kc$bathrooms, main = "bathrooms", xlab = "price", ylab = "Count", col = "blue", )
hist(kc$condition, main = "condition", xlab = "condition", ylab = "Count", col = "blue", )
hist(kc$yr_built, main = "yr_built", xlab = "yr_built", ylab = "Count", col = "blue", )
hist(kc$yr_renovated, main = "yr_renovated", xlab = "yr_renovated", ylab = "Count", col = "blue", )
hist(kc$waterfront, main = "waterfront", xlab = "waterfront", ylab = "Count", col = "blue", )
hist(kc$grade, main = "grade", xlab = "grade", ylab = "Count", col = "blue", )
hist(kc$floors, main = "floors", xlab = "floors", ylab = "Count", col = "blue", )
hist(kc$view, main = "view", xlab = "view", ylab = "Count", col = "blue", )

#We can observe that price, sqft_living, sqft_lot, and sqft_above are more skewed

kc$price <- log10(kc$price)
kc$sqft_living <- log10(kc$sqft_living)
kc$sqft_lot <- log10(kc$sqft_lot)
kc$sqft_above <- log10(kc$sqft_above)


par(mfrow=c(3,2))
hist(kc$price, main = "price", xlab = "price", ylab = "Count", col = "blue", )
hist(kc$sqft_living, main = "sqft_living", xlab = "sqft_living", ylab = "Count", col = "blue")
hist(kc$sqft_lot, main = "sqft_lot", xlab = "sqft_lot", ylab = "Count", col = "blue")
hist(kc$sqft_above, main = "sqft_above", xlab = "sqft_above", ylab = "Count", col = "blue")

#No we will check how other variables are associated with the price variable
#In normal scenario, price of house is directly proportional to sqft of living area, we will check if its true in our study
par(mfrow=c(3,5))
plot(x=kc$sqft_living, y=kc$price, xlab="sqft_living", ylab="price")
plot(x=kc$sqft_lot, y=kc$price, xlab="sqft_lot", ylab="price")
plot(x=kc$sqft_above, y=kc$price, xlab="sqft_above", ylab="price")
plot(x=kc$sqft_basement, y=kc$price, xlab="sqft_basement", ylab="price")
plot(x=kc$bedrooms, y=kc$price, xlab="bedrooms", ylab="price")
plot(x=kc$bathrooms, y=kc$price, xlab="bathrooms", ylab="price")
plot(x=kc$condition, y=kc$price, xlab="condition", ylab="price")
plot(x=kc$yr_built, y=kc$price, xlab="yr_built", ylab="price")
plot(x=kc$month, y=kc$price, xlab="month", ylab="price")
plot(x=kc$waterfront, y=kc$price, xlab="waterfront", ylab="price")
plot(x=kc$grade, y=kc$price, xlab="grade", ylab="price")
plot(x=kc$floors, y=kc$price, xlab="floors", ylab="price")
plot(x=kc$month, y=kc$price, xlab="month", ylab="price")
plot(x=kc$view, y=kc$price, xlab="view", ylab="price")
plot(x=kc$zipcode, y=kc$price, xlab="zipcode", ylab="price")

#"condition" data looks good, we can observe that prices increases when the condition increases
#We can conclude that houses with waterfront are quite less. Also, houses with waterfront are costlier than house with no waterfront view
#Grade is a construction quality parameter which gives an idea about quality of materials and workmanship used for that house
#By observing the plot we can clearly say that house having higher grades costs more than house with low grades
#By observing the plots we can say that prices increases when the number of floor increases

#Now we will see the correaltion among all variables
par(mfrow=c(1,1))
corrplot(cor(kc),method="circle")
#I am not considering 'sqft_above' as feature in my model is because it is highly correalated with sqft_living
#sqft_living15 and sqft_lot15 is correalted with sqft_living and sqft_lot respectively. so, I will not include sqft_living15 and sqft_lot15 in my model
#Also, id will be removed from the data, yr_renovated do not have great association with price so we will remove same from the dataset
#Since we are keeping zipcode in our dataset then we can remove latitude and longitude from the dataset

kc$sqft_above <- NULL
kc$sqft_living15 <- NULL
kc$sqft_lot15 <- NULL
kc$id <- NULL
kc$yr_renovated <- NULL
kc$lat <- NULL
kc$long <- NULL

par(mfrow=c(3,5))
boxplot(kc$price, main = "price", xlab = "price")
boxplot(kc$sqft_living, main = "sqft_living", xlab = "sqft_living")
boxplot(kc$sqft_lot, main = "sqft_lot", xlab = "sqft_lot")
boxplot(kc$sqft_basement, main = "sqft_basement", xlab = "sqft_basement")
boxplot(kc$month, main = "month", xlab = "price")
boxplot(kc$bedrooms, main = "bedrooms", xlab = "price")
boxplot(kc$bathrooms, main = "bathrooms", xlab = "price")
boxplot(kc$condition, main = "condition", xlab = "condition")
boxplot(kc$yr_built, main = "yr_built", xlab = "yr_built")
boxplot(kc$yr_renovated, main = "yr_renovated", xlab = "yr_renovated")
boxplot(kc$waterfront, main = "waterfront", xlab = "waterfront")
boxplot(kc$grade, main = "grade", xlab = "grade")
boxplot(kc$floors, main = "floors", xlab = "floors")
boxplot(kc$view, main = "view", xlab = "view")

kc_clean = subset(kc, price>5 & price<6.3 & sqft_living>2.7 & sqft_living<3.7 & sqft_lot>3.25 & sqft_lot<4.5 & sqft_basement<1500 & bedrooms>1 & bedrooms<6 & bathrooms>0.75 & bathrooms<4.1)
str(kc_clean)

par(mfrow=c(3,5))
boxplot(kc_clean$price, main = "price", xlab = "price")
boxplot(kc_clean$sqft_living, main = "sqft_living", xlab = "sqft_living")
boxplot(kc_clean$sqft_lot, main = "sqft_lot", xlab = "sqft_lot")
boxplot(kc_clean$sqft_basement, main = "sqft_basement", xlab = "sqft_basement")
boxplot(kc_clean$month, main = "month", xlab = "price")
boxplot(kc_clean$bedrooms, main = "bedrooms", xlab = "price")
boxplot(kc_clean$bathrooms, main = "bathrooms", xlab = "price")
boxplot(kc_clean$condition, main = "condition", xlab = "condition")
boxplot(kc_clean$yr_built, main = "yr_built", xlab = "yr_built")
boxplot(kc_clean$yr_renovated, main = "yr_renovated", xlab = "yr_renovated")
boxplot(kc_clean$waterfront, main = "waterfront", xlab = "waterfront")
boxplot(kc_clean$grade, main = "grade", xlab = "grade")
boxplot(kc_clean$floors, main = "floors", xlab = "floors")
boxplot(kc_clean$view, main = "view", xlab = "view")




#After observing all data, I am assuming my independent variables should be bedrooms, bathroom, sqft_living, sqft_lot, and floors


#Now we will proceed with the train and test data
set.seed(1234)
ind_sample <- sample(2, nrow(kc), replace=TRUE, prob=c(0.70, 0.30))
train <- kc[ind_sample==1,]
test <- kc[ind_sample==2,]
cbind(summary(train$price), summary(test$price))

x.train <- model.matrix(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+grade+yr_built+zipcode, training)
y.train <- training$price

custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = T)

ridge <- train(price~bedrooms+bathrooms+sqft_living+floors+waterfront+view+grade+yr_built+zipcode,
               train,
               method = "glmnet",
               tuneGrid = expand.grid(alpha=0,
                                      lambda=seq(0.0001, 0.1, length=5)),
               trControl=custom)
plot(ridge)
ridge
par(mfrow=c(1,1))
plot(ridge$finalModel, xvar = "lambda", label = T)
par(mfrow=c(1,1))
plot(ridge$finalModel, xvar = "dev", label = T)
plot(varImp(ridge, scale=T))




lasso <- train(price~bedrooms+bathrooms+sqft_living+floors+waterfront+view+grade+yr_built+zipcode,
               train,
               method = "glmnet",
               tuneGrid = expand.grid(alpha=1,
                                      lambda=seq(0.0001, 0.01, length=5)),
               trControl=custom)
par(mfrow=c(1,1))
plot(lasso)
lasso
par(mfrow=c(1,1))
plot(lasso$finalModel, xvar = "lambda", label = T)
par(mfrow=c(1,1))
plot(lasso$finalModel, xvar = "dev", label = T)
plot(varImp(ridge, scale=T))

model_list <- list(Ridge = ridge, Lasso = lasso)
res <- resamples(model_list)
summary(res)
bwplot(res)
xyplot(res, metric = "RMSE")



p1 <- predict(ridge, train)
sqrt(mean((train$price-p1)^2))

p2 <- predict(ridge, test)
sqrt(mean((test$price-p2)^2))

p3 <- predict(lasso, train)
sqrt(mean((train$price-p3)^2))

p4 <- predict(lasso, test)
sqrt(mean((test$price-p4)^2))


