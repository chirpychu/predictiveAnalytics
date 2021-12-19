data <- read.csv("C:/Users/Wolflx/OneDrive/Desktop/proj analytics proj/hdb_final.csv")
View(data)
library(car)
install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

hist(data$resale_price_thousands, main = "Histogram for resale price for hdb")

# split data
trainingset <- data[1:1045,] # 50%
testingset <- data[1046:2090,] #50% 

data$mature_estate <- as.factor(data$mature_estate)

model2 <- lm(resale_price~mature_estate+storey_avg+floor_area_sqm+remaining_lease_years+distanceToMrt++distanceToPri+distanceToMall+numMall+numChildcare+distanceToHosp+distanceToRafflesMRT, data = trainingset)
summary(model2)

model3 <- lm(resale_price~mature_estate+storey_avg+floor_area_sqm+remaining_lease_years+distanceToMrt+distanceToPri+distanceToHosp+distanceToRafflesMRT, data = trainingset)
summary(model3)

#Predicting the price of the houses
predict(model3,head(trainingset))

#Print the actual price of the house
print(head(trainingset$resale_price))

mse <- sum(((testingset$resale_price - predict(model3, data = testingset))^2))/1036
mse

rmse<- sqrt(mean(model3$residuals^2)/1036)
rmse

library(olsrr)
ols_step_best_subset(model2)


a = aov(resale_price~town+flat_type+remaining_lease_years+floor_area_sqm+lease_commence_date+distanceToMrt+distanceToPri+distanceToMall+distanceToMrt, data = data)
summary(a)

# Plot #1: Basic scatterplot matrix of the 3 measurements
pairs(~resale_price+storey_avg+remaining_lease_years+floor_area_sqm, col = 'dark blue', data=data)

pairs(~resale_price+distanceToMrt+distanceToHosp+distanceToRafflesMRT+numMall+ numChildcare, col = 'dark blue', data=data)

boxplot(resale_price_thousands~flat_model, data=data,main="Boxplots for each flat model", xlab="flat model",ylab="resale price(K)",col="lightpink",border="brown")

boxplot(resale_price_thousands~remaining_lease_years, data=data,main="Boxplots for each floor_area_sqm", xlab="remaining_lease_years",ylab="resale price(K)",col="orange",border="brown")

modified<-data.frame(data$floor_area_sqm,data$remaining_lease_years, data$resale_price_k, data$distanceToMrt, data$distanceToPri, data$distanceToMall, data$distanceToRafflesMRT)
cor(modified)

plot(data$floor_area_sqm, data$resale_price_thousands, xlab = "square metre", ylab = "Resales price (thousands)")

set.seed(1)
data1 <- sample(nrow(data), nrow(data)*.70)
trainingset<- data[data1,]
testingset<-  data[-data1,]

model <- lm(resale_price_thousands~remaining_lease_years+floor_area_sqm+lease_commence_date, data = trainingset)
summary(model)

model3 <- lm(resale_price_thousands~remaining_lease_years, data = trainingset)
summary(model3)
plot(trainingset$resale_price_thousands~trainingset$remaining_lease_years, xlab = "remaining_lease_years", ylab = "Resales price (thousands)")
abline(model3, col = "blue", lwd = 3)

model4 <- lm(resale_price_thousands~floor_area_sqm, data = trainingset)
summary(model4)
plot(data$resale_price_thousands~data$floor_area_sqm, xlab = "square metre", ylab = "Resales price (thousands)")
abline(model4, col = "red", lwd = 3)
unique(data$town)

# regression tree
library(ISLR)
library(rpart)
treemodel <- rpart(town~mature_estate+flat_type+flat_model, data = data)
summary(treemodel)
plot(treemodel, margin=0.1)
text(treemodel, cex = 0.7, pretty = 0)

#Predicting the price of the houses
predict(treemodel,head(data))

#Print the actual price of the house
print(head(data$resale_price))


library(modelr)
mae(model=treemodel,data=data)

# classification tree
library(tree)
Cmodel<- tree(resale_price~mature_estate+storey_avg+floor_area_sqm+remaining_lease_years+distanceToMrt+distanceToHosp+distanceToRafflesMRT+distanceToPri, data=training)
plot(Cmodel)
text(Cmodel)

predict(Cmodel,head(data))
print(head(data$resale_price))
library(modelr)
mae(model=Cmodel,data=data)
