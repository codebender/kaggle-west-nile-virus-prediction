library(Metrics)
library(gam)
library(data.table)   ## load data in quickly with fread
train <- fread("Data/train.csv")
test <- fread("Data/test.csv")

## prep the species column by moving the test-only UNSPECIFIED CULEX to CULEX ERRATICUS, and re-doing the levels
## logistic regression will complain otherwise
vSpecies<-c(as.character(train$Species),as.character(test$Species))
vSpecies[vSpecies=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"
vSpecies[-which(vSpecies == "CULEX PIPIENS" |
                  vSpecies == "CULEX PIPIENS/RESTUANS" |
                  vSpecies == "CULEX RESTUANS")] = "CULEX OTHER"
vSpecies<-factor(vSpecies,levels=unique(vSpecies))

## data.table syntax for adding a column; could overwrite the existing column as well
train[,Species2:=factor(vSpecies[1:nrow(train)],levels=unique(vSpecies))]
test[,Species2:=factor(vSpecies[(nrow(train)+1):length(vSpecies)],levels=unique(vSpecies))]

## date fields
train$week <- factor(format(as.POSIXct(train$Date, format="%Y-%m-%d"), format="%U"))
test$week <- factor(format(as.POSIXct(test$Date, format="%Y-%m-%d"), format="%U"))
train$year <- format(as.POSIXct(train$Date, format="%Y-%m-%d"), format="%Y")
test$year <- format(as.POSIXct(test$Date, format="%Y-%m-%d"), format="%Y")

## fix Blocks data
train$Block <- as.factor(train$Block)
test$Block <- as.factor(test$Block)
test$Block[test$Block == 26] <- 25

# we'll set aside 2011 data as test, and train on the remaining
my.train = data.frame(train[,list(WnvPresent, week, Block, Species2, Latitude, Longitude)])
train1<-my.train[train$year != 2011,]
train2<-my.train[train$year == 2011,]
train2$Block[train2$Block==29] <- 28
train2$Block[train2$Block==34] <- 33

## GAM modelling
fitCv = gam(WnvPresent ~ week + Block + Species2 + lo(Latitude, Longitude),
           data = train1, family="binomial")
p2<-predict(fitCv, newdata = train2, type = "response")
## check for a reasonable AUC of the model against unseen data (2011)
auc(train2$WnvPresent,p2)

## now fit a new model to all the data, so that our final submission includes information learned from 2011 as well
fitSubmit <- update(fitCv, data=my.train)
pSubmit<-predict(fitSubmit, newdata = test, type = "response")
## look at the predicted distribution (AUC doesn't care about probabilities; just ordering. It's still a good diagnostic)
summary(pSubmit)

submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"Output/GAM_with_block_2.csv",row.names=FALSE,quote=FALSE)
