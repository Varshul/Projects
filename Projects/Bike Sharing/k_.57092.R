library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(lubridate)


fit1 <- rpart(registered ~ season + holiday + workingday + weather + temp+ atemp + humidity + windspeed, data=train, method="poisson")
fit2 <- rpart(casual ~ season + holiday + workingday + weather + temp+ atemp + humidity + windspeed, data=train, method="poisson")
s <- data.frame(datetime = test$datetime, count = (p+p1))

a <- as.character(train$datetime)
b <- as.character(test$datetime)
train$t <- hour(train$datetime)
test$t <- hour(test$datetime)
train$d <- wday(train$datetime)
test$d <- wday(test$datetime)
fit1 <- randomForest(count ~ d  + temp + t + humidity + holiday + workingday + season + weather + windspeed, data=train, importance=T,ntree=1000)
varImpPlot(fit1)
p <- predict(fit1, test,interval=("confidence"))
s <- data.frame(datetime = test$datetime, count = p)

write.csv(s, file = "submit12.csv", row.names=F)


##
summary(lm(casual ~ temp, data = train))$coefficients
cor(train$casual,train$humidity)
require(datasets); data(InsectSprays); require(stats)
g= ggplot(data=train, aes(y=casual, x= as.factor(season), fill=as.factor(season)))
g= g + geom_violin(colour="black", size=2)
g= g + xlab("seasons")+ylab("casua; count")
g

summary(lm(casual ~ as.factor(season), data = train))$coef
summary(lm(casual ~ as.factor(season) - 1, data = train))$coef

fit <- rpart(casual ~ season, data=train)
fancyRpartPlot(fit)

train$hour <-hour(train$datetime)
test$hour <- hour(test$datetime)

#Compute day of the week
train$dow <- wday(train$datetime)
test$dow <- wday(test$datetime)

test$count<-0
#Create a random forest
fit <- randomForest(as.factor(count) ~ season + holiday + weather + dow+ hour + temp + atemp+humidity+windspeed , data=train, ntree = 700, importance=TRUE)
#Uncomment the following line if you want to see how your model plot looks like
#varImpPlot(fit)

#Predict values and save output
Prediction <- predict(fit, test)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "random-forest.csv", row.names = FALSE)
