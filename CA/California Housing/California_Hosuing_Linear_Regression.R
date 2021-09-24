#install.packages('tidyverse')
#install.packages('corrplot')
#install.packages('olsrr')
#install.packages('VIF')
library(corrplot)
library(tidyverse)
library(olsrr)
library(car)
cal = read.csv("/Users/omkar/Omkar/DM&ML/Project/California Hosuing/housing.csv", header = TRUE, sep = ",")
head(cal)
str(cal)
summary(cal)

#After observing the data, we can say that 'ocean_proximity' is a string variable
cal$ocean_proximity <- as.integer(as.factor(cal$ocean_proximity))

head(cal)

summary(cal)

#We will observe the relationship of total_rooms and tota_bedrooms
plot(x=cal$total_bedrooms, y=cal$total_rooms, xlab="total_bedrooms", ylab="total_rooms")

#Since realtion between total_bedroom and total_room is linear, we can produce a linear model between totol_rooms and total_bedroom 
#So that we can predict the value of total_bedroom which are currently NA
cal_bedrooms_lm = lm(as.numeric(total_bedrooms) ~ as.numeric(total_rooms), 
                      data = subset(cal, !is.na(total_bedrooms)))
# Fill missing value of Bathroom by predicting from above model
cal$total_bedrooms[is.na(cal$total_bedrooms)] = ceiling(predict(object = cal_bedrooms_lm, newdata = data.frame(total_rooms = cal$total_rooms[is.na(cal$total_bedrooms)])))

summary(cal)

#While studying about the data, I realized that total_rooms and total_bedrooms would be irrelevant if we want to predict price of house,
#So we will add two column avg_bedrooms and avg_rooms in the data
#avg_bedroom = total_bedroom/households
#avg_rooms = total_rooms/houselholds
#Also we will consider population of each household now os we will need to average out population also
cal$avg_bedrooms <- cal$total_bedrooms/cal$households
cal$avg_rooms <- cal$total_rooms/cal$households
cal$population <- cal$population/cal$households

#Removed the total_rooms and total_bedrooms from the data
cal$total_rooms <- NULL
cal$total_bedrooms <- NULL


#Let us visualize the data now to see whether any independent variable has a linear relationship without dependent varaible
par(mfrow=c(3,3))
plot(x=cal$longitude, y=cal$median_house_value, xlab="Longitude", ylab="Median House Value")
plot(x=cal$latitude, y=cal$median_house_value, xlab="Latitude", ylab="Median House Value")
plot(x=cal$housing_median_age, y=cal$median_house_value, xlab="Cal Median Age", ylab="Median House Value")
plot(x=cal$population, y=cal$median_house_value, xlab="Population", ylab="Median House Value")
plot(x=cal$households, y=cal$median_house_value, xlab="Households", ylab="Median House Value")
plot(x=cal$median_income, y=cal$median_house_value, xlab="Median Income", ylab="Median House Value")
plot(x=cal$avg_bedrooms, y=cal$median_house_value, xlab="Avg Bedrooms", ylab="Median House Value")
plot(x=cal$avg_rooms, y=cal$median_house_value, xlab="Avg Rooms", ylab="Median House Value")
plot(x=cal$ocean_proximity, y=cal$median_house_value, xlab="Ocean Proximity", ylab="Median House Value")
#By observing the scatter plot we can conclude that only 'Median_income' is in linear relationship with 'Median_House_Value'

#Now we will observe the frequency plot of all variables
par(mfrow=c(3,3))
hist(cal$longitude, main = "Longitude", xlab = "Longitude", ylab = "Count", col = "blue", )
hist(cal$latitude, main = "latitude", xlab = "latitude", ylab = "Count", col = "blue")
hist(cal$housing_median_age, main = "housing_median_age", xlab = "housing_median_age", ylab = "Count", col = "blue")
hist(cal$avg_rooms, main = "Avg_rooms", xlab = "total_rooms", ylab = "Count", col = "blue")
hist(cal$avg_bedrooms, main = "Avg_bedrooms", xlab = "total_bedrooms", ylab = "Count", col = "blue")
hist(cal$population, main = "population", xlab = "population", ylab = "Count", col = "blue")
hist(cal$households, main = "households", xlab = "households", ylab = "Count", col = "blue")
hist(cal$median_income, main = "median_income", xlab = "median_income", ylab = "Count", col = "blue")
hist(cal$median_house_value, main = "median_house_value", xlab = "median_house_value", ylab = "Count", col = "blue")


#We can apply log transformation on 'total_rooms', 'total_bedrooms', 'population', 'households', 'median_income' and 'median_house value'
cal$avg_rooms <- log(cal$avg_rooms)
cal$avg_bedrooms <- log(cal$avg_bedrooms)
cal$population <- log(cal$population)
cal$households <- log(cal$households)
cal$median_income <- log(cal$median_income)
cal$median_house_value <- log(cal$median_house_value)

#We can observe that these variables are now normally distributed due to log transformation
par(mfrow=c(2,3))
hist(cal$avg_rooms, main = "avg_rooms", xlab = "avg_rooms", ylab = "Count", col = "blue")
hist(cal$avg_bedrooms, main = "avg_bedrooms", xlab = "avg_bedrooms", ylab = "Count", col = "blue")
hist(cal$population, main = "population", xlab = "population", ylab = "Count", col = "blue")
hist(cal$households, main = "households", xlab = "households", ylab = "Count", col = "blue")
hist(cal$median_income, main = "median_income", xlab = "median_income", ylab = "Count", col = "blue")
hist(cal$median_house_value, main = "median_house_value", xlab = "median_house_value", ylab = "Count", col = "blue")

#Now we can check the  outliers by observing the boxplots
par(mfrow=c(2,5))
boxplot(cal$longitude, xlab = "Boxplot (Longitude)")
boxplot(cal$latitude, xlab = "Boxplot (Latitude)")
boxplot(cal$housing_median_age, xlab = "Boxplot (Housing Median Age)")
boxplot(cal$population, xlab = "Boxplot (Population)")
boxplot(cal$households, xlab = "Boxplot (Households)")
boxplot(cal$median_income, xlab = "Boxplot (Median Income)")
boxplot(cal$median_house_value, xlab = "Boxplot (Median House Value)")
boxplot(cal$avg_bedrooms, xlab="Boxplot (Avg Bedrooms)")
boxplot(cal$avg_rooms, xlab="Boxplot (Avg Rooms)")
boxplot(cal$ocean_proximity, xlab="Boxplot (Ocean Proximity)")

summary(cal)

#Removing the outliers
cal_clean = subset(cal, population>0.5 & population<1.6 & households>4.7 & households<7.4 & median_income>0.1 & median_income<2.4 & median_house_value>10.5 & avg_bedrooms>-0.12 & avg_bedrooms<0.2 & avg_rooms>1.1 & avg_rooms<2.2)
str(cal_clean)

par(mfrow=c(2,5))
boxplot(cal_clean$longitude, xlab = "Boxplot (Longitude)")
boxplot(cal_clean$latitude, xlab = "Boxplot (Latitude)")
boxplot(cal_clean$housing_median_age, xlab = "Boxplot (Housing Median Age)")
boxplot(cal_clean$population, xlab = "Boxplot (Population)")
boxplot(cal_clean$households, xlab = "Boxplot (Households)")
boxplot(cal_clean$median_income, xlab = "Boxplot (Median Income)")
boxplot(cal_clean$median_house_value, xlab = "Boxplot (Median House Value)")
boxplot(cal_clean$avg_bedrooms, xlab="Boxplot (Avg Bedrooms)")
boxplot(cal_clean$avg_rooms, xlab="Boxplot (Avg Rooms)")
boxplot(cal_clean$ocean_proximity, xlab="Boxplot (Ocean Proximity)")

#Now we will check the correaltion between the independent variables,
par(mfrow=c(1,1))
corrplot(cor(cal_clean),method="circle")
cor(cal_clean)

#median_income and avg_rooms are highly correlated so I have decided to remove avg_rooms because we have avg_bedrooms in our dataset.
cal_clean$avg_rooms <- NULL


set.seed(1234)
ind_sample <- sample(2, nrow(cal_clean), replace=TRUE, prob=c(0.70, 0.30))
training <- cal_clean[ind_sample==1,]
testing <- cal_clean[ind_sample==2,]
cbind(summary(training$median_house_value), summary(testing$median_house_value)) 

model <- lm(median_house_value~., data=training)
model
summary(model)
par(mfrow=c(2,2))
plot(model)

#RSE - Residual Standard Error - Prediction Error Rate
sigma(model)/mean(testing$median_house_value)
