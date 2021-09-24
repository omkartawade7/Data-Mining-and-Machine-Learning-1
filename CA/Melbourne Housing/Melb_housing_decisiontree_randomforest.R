#install.packages("rpart.plot") 
#install.packages('rpart')
#install.packages('randomForest')

library(corrplot)
library(rpart.plot)
library(rpart)
library(randomForest)

melb <- read.csv("/Users/omkar/Omkar/DM&ML/Project/Melbourne Housing/Melbourne_housing_FULL.csv", header = TRUE, sep=",")
head(melb)
str(melb)
summary(melb)

#By observing the data we can say that there are too many NA's in data and datatype of few columns seems to be incorrect.

#We will correct the datatype of few columns first
sapply(melb, class)
melb$Distance<- as.numeric(melb$Distance)
melb$Postcode<- as.integer(melb$Postcode)
melb$Propertycount<- as.integer(melb$Propertycount)
melb$Type<-as.integer(as.factor(melb$Type))
melb$Regionname<-as.numeric(as.factor(melb$Regionname))
melb$CouncilArea<-as.numeric(as.factor(melb$CouncilArea))
melb$Method<-as.numeric(as.factor(melb$Method))
melb$SellerG<-as.factor(melb$SellerG)
sapply(melb, class)

#Now we will check the count of NA's in each columns
colSums(is.na(melb))

#We will remove NA's of Price columns since it is our dependent variable,
melb <- melb[-which(is.na(melb$Price)),]
colSums(is.na(melb))

#By oberving the data, we can classify data into three features such as LocationBased, HouseBased, SellerBased,
#We will start cleaning the data of Location based datafirst,

#LocationBased - Suburbs, Address, PostCode, CouncilArea, Latitude, Longitude Regionname, Propertycount and Type.
#Since address in unique to each row, so I think it will be not help to our analysis so I will drop address column
#Also, suburbs will be not help in our analysis since it has too many unique values so I will drop Suburbs also
melb <- melb[,c(1,3:21)]
colSums(is.na(melb))

melb <- melb[,c(2:20)]
colSums(is.na(melb))

#We will remove the latitude and longitude column since we have regionname in the data
melb <- melb[,c(1:15,18:19)]
colSums(is.na(melb))

#There are three NA's in the property count, we will remove it from the data
melb <- melb[-which(is.na(melb$Propertycount)),]
colSums(is.na(melb))

#Now we will observe the frequency plot of LacationBased feature variables,
par(mfrow=c(2,5))
hist(melb$Postcode, main = "Postcode", xlab = "Postcode", ylab = "Count", col = "blue", )
hist(melb$CouncilArea, main = "CouncilArea", xlab = "CouncilArea", ylab = "Count", col = "blue", )
hist(melb$Regionname, main = "RegionName", xlab = "RegionName", ylab = "Count", col = "blue", )
hist(melb$Type, main = "Type", xlab = "Type", ylab = "Count", col = "blue", )
hist(melb$Propertycount, main = "Propertycount", xlab = "Propertycount", ylab = "Count", col = "blue", )

melb$Propertycount <- log(melb$Propertycount)


plot(x=melb$Postcode, y=melb$Price, xlab="Postcode", ylab="Price")
plot(x=melb$CouncilArea, y=melb$Price, xlab="CouncilArea", ylab="Price")
plot(x=melb$Regionname, y=melb$Price, xlab="Regionname", ylab="Price")
plot(x=melb$Type, y=melb$Price, xlab="Type", ylab="Price")
plot(x=melb$Propertycount, y=melb$Price, xlab="Propertycount", ylab="Price")

#Now we will proceed with the data cleaning of house related features,
#HouseBased - Rooms, Bedroom2, Bathroom, Landsize, Distance, Car, BuildingArea and YearBuilt

#We can see there are too many NA's in the Bedroom2. We will check it's association with other house based feature first
plot(x=melb$Bedroom2, y=melb$Rooms, xlab="Bedroom2", ylab="Rooms")

#By observing the plot we can conclude that Rooms and Bedroom2 is highly correlated, so we will drop Bedroom2 since it has NA's and removing Bedroom2 will avoid multi-collinearity
melb <- melb[,c(1:8,10:17)]
colSums(is.na(melb))

#we can see that building area and year built has highest count of NA's which is more than 50 percent of the data,
#If we input median or mean values inplace of NA's we might introduce bias in the data
#So we will remove the BuildingArea and YearBuilt in our analysis
melb <- melb[,c(1:11,14:16)]
colSums(is.na(melb))

#Bathroom column have 6444 NA's, we will check how Rooms and bathroom is associated
plot(x=melb$Rooms, y=melb$Bathroom, xlab="Rooms", ylab="Bathroom")

#We can see a linear relationship between bathroom and rooms, so we will create a linear model by using bathroom and room and then predict the bathroom data
melb_bathroom_lm = lm(as.numeric(as.character(Bathroom)) ~ as.numeric(as.character(Rooms)), 
                 data = subset(melb, !is.na(Bathroom)))
# Fill missing value of Bathroom by predicting from above model
melb$Bathroom[is.na(melb$Bathroom)] = ceiling(predict(object = melb_bathroom_lm, newdata = data.frame(Rooms = melb$Rooms[is.na(melb$Bathroom)])))
colSums(is.na(melb))

#For NA's of Car, we will check again how it is associated with rooms
plot(x=melb$Car, y=melb$Rooms, xlab="Car", ylab="Rooms")

#We will use the same technique for Car also
melb_car_lm = lm(as.numeric(as.character(Car)) ~ as.numeric(as.character(Rooms)), 
                      data = subset(melb, !is.na(Car)))
# Fill missing value of Bathroom by predicting from above model
melb$Car[is.na(melb$Car)] = ceiling(predict(object = melb_car_lm, newdata = data.frame(Rooms = melb$Rooms[is.na(melb$Car)])))
colSums(is.na(melb))

#Now there are NA's only in Landsize
#We can use median values in place of NA's in Landsize data
median_landsize = median(as.numeric(as.character(melb$Landsize)), na.rm = TRUE)
melb$Landsize[is.na(melb$Landsize)] = median_landsize
colSums(is.na(melb))

#Now we will observe the frequency plot of House based feature varaibles
par(mfrow=c(2,5))
hist(melb$Rooms, main = "Rooms", xlab = "Rooms", ylab = "Count", col = "blue", )
hist(melb$Distance, main = "Distance", xlab = "Distance", ylab = "Count", col = "blue", )
hist(melb$Bathroom, main = "Bathroom", xlab = "Bathroom", ylab = "Count", col = "blue", )
hist(melb$Car, main = "Car", xlab = "Car", ylab = "Count", col = "blue", )
hist(melb$Landsize, main = "Landsize", xlab = "Landsize", ylab = "Count", col = "blue", )


plot(x=melb$Rooms, y=melb$Price, xlab="Rooms", ylab="Price")
plot(x=melb$Distance, y=melb$Price, xlab="Distance", ylab="Price")
plot(x=melb$Bathroom, y=melb$Price, xlab="Bathroom", ylab="Price")
plot(x=melb$Car, y=melb$Price, xlab="Car", ylab="Price")
plot(x=melb$Landsize, y=melb$Price, xlab="Landsize", ylab="Price")


#Now we will observe the Seller related features,
#Seller - Method, SellerG, and Date
#As we observed earlier that sellerG has 346 unique values, so I don't think it will be helpful in our analysis
#So I have decide to drop SellerG column
melb <- melb[,c(1:4,6:14)]
colSums(is.na(melb))

#We need to change the date column, we will check if price is associated with the month, for this we will need to separte date
str(melb)
date<-as.Date(melb$Date,format="%d/%m/%Y")
year=as.numeric (format(date,"%Y"))
month=as.numeric (format(date,"%m"))
day=as.numeric (format(date,"%d"))
melb$Date <- month
names(melb)[names(melb) == "Date"] <- "Month"

par(mfrow=c(2,2))
hist(melb$Month, main = "Month", xlab = "Month", ylab = "Count", col = "blue", )
hist(melb$Method, main = "Method", xlab = "Method", ylab = "Count", col = "blue", )
plot(x=melb$Month, y=melb$Price, xlab="Month", ylab="Price")
plot(x=melb$Method, y=melb$Price, xlab="Method", ylab="Price")

#Now we will check the outliers using boxplots
par(mfrow=c(3,5))
boxplot(melb$Price, main = "Price", xlab = "Price")
boxplot(melb$Postcode, main = "Postcode", xlab = "Postcode")
boxplot(melb$CouncilArea, main = "CouncilArea", xlab = "CouncilArea")
boxplot(melb$Regionname, main = "Regionname", xlab = "Regionname")
boxplot(melb$Type, main = "Type", xlab = "Type")
boxplot(melb$Propertycount, main = "Propertycount", xlab = "Propertycount")
boxplot(melb$Rooms, main = "Rooms", xlab = "Rooms")
boxplot(melb$Distance, main = "Distance", xlab = "Distance")
boxplot(melb$Bathroom, main = "Bathroom", xlab = "Bathroom")
boxplot(melb$Car, main = "Car", xlab = "Car")
boxplot(melb$Landsize, main = "Landsize", xlab = "Landsize")
boxplot(melb$Month, main = "Month", xlab = "Month")
boxplot(melb$Method, main = "Method", xlab = "Method")

#We can observe that there are outliers in the Price, PropertyCount, Rooms, Distance, Bathroom, Car and  Landsize data, we will remove these outliers from our data
outliers <- boxplot(melb$Price, plot=FALSE)$out
melb[which(melb$Price %in% outliers),]
melb <- melb[-which(melb$Price %in% outliers),]

outliers <- boxplot(melb$Propertycount, plot=FALSE)$out
melb[which(melb$Propertycount %in% outliers),]
melb <- melb[-which(melb$Propertycount %in% outliers),]

outliers <- boxplot(melb$Rooms, plot=FALSE)$out
melb[which(melb$Rooms %in% outliers),]
melb <- melb[-which(melb$Rooms %in% outliers),]

outliers <- boxplot(melb$Distance, plot=FALSE)$out
melb[which(melb$Distance %in% outliers),]
melb <- melb[-which(melb$Distance %in% outliers),]

outliers <- boxplot(melb$Bathroom, plot=FALSE)$out
melb[which(melb$Bathroom %in% outliers),]
melb <- melb[-which(melb$Bathroom %in% outliers),]

outliers <- boxplot(melb$Car, plot=FALSE)$out
melb[which(melb$Car %in% outliers),]
melb <- melb[-which(melb$Car %in% outliers),]

outliers <- boxplot(melb$Landsize, plot=FALSE)$out
melb[which(melb$Landsize %in% outliers),]
melb <- melb[-which(melb$Landsize %in% outliers),]

par(mfrow=c(3,5))
boxplot(melb$Price, main = "Price", xlab = "Price")
boxplot(melb$Postcode, main = "Postcode", xlab = "Postcode")
boxplot(melb$CouncilArea, main = "CouncilArea", xlab = "CouncilArea")
boxplot(melb$Regionname, main = "Regionname", xlab = "Regionname")
boxplot(melb$Type, main = "Type", xlab = "Type")
boxplot(melb$Propertycount, main = "Propertycount", xlab = "Propertycount")
boxplot(melb$Rooms, main = "Rooms", xlab = "Rooms")
boxplot(melb$Distance, main = "Distance", xlab = "Distance")
boxplot(melb$Bathroom, main = "Bathroom", xlab = "Bathroom")
boxplot(melb$Car, main = "Car", xlab = "Car")
boxplot(melb$Landsize, main = "Landsize", xlab = "Landsize")
boxplot(melb$Month, main = "Month", xlab = "Month")
boxplot(melb$Method, main = "Method", xlab = "Method")


#Now data is cleaned and it is ready for regression process
head(melb)
str(melb)
summary(melb)


#We will check the associate of dependent and independent variables and also will check if there is any correaltion between independent varialbes 
par(mfrow=c(1,1))
corrplot(cor(melb),method="circle")
cor(melb)
#We can see that price is influenced by Rooms, Bathroom, Car, Landsize, Postcode and Regionname
#There is no strong correlation between among any independent variables

#Now we will separate train and test data,
set.seed(123)
ind_sample <- sample(2, nrow(melb), replace=TRUE, prob=c(0.70, 0.30))
train <- melb[ind_sample==1,]
test <- melb[ind_sample==2,]
cbind(summary(train$Price), summary(test$Price))

#With rpart
melb.tree <-rpart(Price~., data=train ,cp=0.0001)
printcp(melb.tree)
plotcp(melb.tree)
melb.prune<- prune(melb.tree, cp=0.0004)
melb.prune
rpart.plot(melb.prune,type = 5,extra=101)

melb.tree.pred <- predict(melb.prune,newdata = test)
SSE <- sum((test[,"Price"]-melb.tree.pred)^2)
SST <- sum((test[,"Price"] - mean(test[,"Price"]))^2)
r2 <- 1 - SSE/SST
r2


#RandomForest
melb.rf <- randomForest(Price~.,data = train)
importance(melb.rf)
varImpPlot(melb.rf)
melb.rf
plot(melb.rf)
melb.rf.pred <- predict(melb.rf,newdata = test)
SSE.rf <- sum((test[,"Price"]-melb.rf.pred)^2)
SST.rf <- sum((test[,"Price"] - mean(test[,"Price"]))^2)
r2.rf <- 1 - SSE.rf/SST.rf
r2.rf

