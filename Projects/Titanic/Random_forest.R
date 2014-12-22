library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
## Combining both the test and train sets
test$Survived <- NA
c <- rbind(train,test)
c$FamilySize <- c$SibSp + c$Parch + 1
c$Name <- as.character(c$Name)

##Spilitting the title name in the form eg: Sarvapalli Dr. Radhakrishnan: Dr is to be saved in other collumn
c$Title <- sapply(c$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
c$Title <- sub(' ', '', c$Title)

##For better prediction grouping the titles
c$Title[c$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
c$Title[c$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
c$Title[c$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
c$Title <- factor(c$Title)

Agefit <- rpart(Age ~ Pclass + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
             data= c[!is.na(c$Age),], method="anova")
c$Age[is.na(c$Age)] <- predict(Agefit, c[is.na(c$Age),])
summary(c$Age)
summary(c$Embarked)

which(c$Embarked == '')
c$Embarked[c(62,830)] = "S"
c$Embarked <- factor(c$Embarked)
c$Fare[1044] <- median(c$Fare, na.rm=TRUE)

c$Surname <- sapply(c$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
c$FamilyID <- paste(as.character(c$FamilySize), c$Surname, sep="")
c$FamilyID[c$FamilySize <= 2] <- 'Small'
famIDs <- data.frame(table(c$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
c$FamilyID[c$FamilyID %in% famIDs$Var1] <- 'Small'
c$FamilyID <- factor(c$FamilyID)

c$FamilyID2 <- c$FamilyID
c$FamilyID2 <- as.character(c$FamilyID2)
c$FamilyID2[c$FamilySize <= 3] <- 'Small'
c$FamilyID2 <- factor(c$FamilyID2)

library(randomForest)
train <- c[1:891,]
test <- c[892:1309,]

set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass  + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                      FamilyID2, data=train, importance=TRUE, ntree=2000)
varImpPlot(fit)

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "Submission3.csv", row.names = FALSE)

library(party)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "Submission4.csv", row.names = FALSE)

set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + Ticket + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "Submission5.csv", row.names = FALSE)
