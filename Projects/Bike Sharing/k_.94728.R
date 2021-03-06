library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)


fit1 <- rpart(registered ~ season + holiday + workingday + weather + temp+ atemp + humidity + windspeed, data=train, method="poisson")
fit2 <- rpart(casual ~ season + holiday + workingday + weather + temp+ atemp + humidity + windspeed, data=train, method="poisson")
s <- data.frame(datetime = test$datetime, count = (p+p1))

a <- as.character(train$datetime)
b <- as.character(test$datetime)
train$t <- hour(train$datetime)
test$t <- hour(test$datetime)
train$d <- wday(train$datetime)
test$d <- wday(test$datetime)
fit1 <- randomForest(registered ~ t  + d + season + holiday + workingday + weather + temp+ atemp + humidity + windspeed, data=train, importance=T,ntree=1000)
fit2 <- randomForest(casual ~ t + d + season + holiday + workingday + weather + temp + atemp + humidity + windspeed, data=train, importance=T,ntree=1000)
varImpPlot(fit1)
varImpPlot(fit2)
p <- predict(fit1, test,interval=("confidence"))
p <- ceiling(p)
p1 <- predict(fit1, test,interval=("confidence"))
p1 <- ceiling(p1)
s <- data.frame(datetime = test$datetime, count = (p+p1))

write.csv(s, file = "submit6.csv", row.names=F)


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
