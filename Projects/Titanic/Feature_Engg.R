test$Survived <- NA
combi <- rbind(train, test)
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
combi$Title <- factor(combi$Title)
qplot(Title, data=combi, facets=.~Survived)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
qplot(FamilySize, data=combi, facets=.~Survived)
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)
train <- combi[1:891,]
test <- combi[892:1309,]
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "Submission2.csv", row.names = FALSE)