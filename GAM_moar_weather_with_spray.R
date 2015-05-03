library(Metrics)
library(gam)
library(data.table)   ## load data in quickly with fread
train <- fread("Data/train.csv")
test <- fread("Data/test.csv")
weather <- fread("Data/weather.csv")
spray <- fread("Data/spray.csv")

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

## average temps by week(avg, high, low)
weather$week <- factor(format(as.POSIXct(weather$Date, format="%Y-%m-%d"), format="%U"))
weather$year <- factor(format(as.POSIXct(weather$Date, format="%Y-%m-%d"), format="%Y"))
weather$Tavg <- as.numeric(weather$Tavg)
weather$Tmax <- as.numeric(weather$Tmax)
weather$Tmin <- as.numeric(weather$Tmin)
weather$PrecipTotal <- as.numeric(weather$PrecipTotal)
weather$AvgSpeed <- as.numeric(weather$AvgSpeed)
weather$Tavg[is.na(weather$Tavg)] <- (weather$Tmax[is.na(weather$Tavg)] + weather$Tmin[is.na(weather$Tavg)])/2
weather$PrecipTotal[is.na(weather$PrecipTotal)] <- 0.0

week_average_temps <- aggregate(Tavg ~ week + year, data=weather, mean)
week_average_temps <- merge(week_average_temps, aggregate(Tmax ~ week + year, data=weather, mean), by=c("week","year"))
week_average_temps <- merge(week_average_temps, aggregate(Tmin ~ week + year, data=weather, mean), by=c("week","year"))
week_average_temps <- merge(week_average_temps, aggregate(PrecipTotal ~ week + year, data=weather, sum), by=c("week","year"))
week_average_temps <- merge(week_average_temps, aggregate(AvgSpeed ~ week + year, data=weather, mean), by=c("week","year"))
week_average_temps <- week_average_temps[with(week_average_temps, order(year, week)), ]

train_with_weather <- merge(train, week_average_temps, by=c("week","year"))
test_with_weather <- merge(test, week_average_temps, by=c("week","year"))

# we'll set aside 2011 data as test, and train on the remaining
my.train = data.frame(data.frame(train_with_weather)[,c("WnvPresent", "week", "Tavg", "Tmax", "Tmin", "PrecipTotal", "AvgSpeed", "Block", "Species2", "Latitude", "Longitude")])
train1<-my.train[train_with_weather$year != 2011,]
train2<-my.train[train_with_weather$year == 2011,]
train2$Block[train2$Block==29] <- 28
train2$Block[train2$Block==34] <- 33

## GAM modelling
fitCv = gam(WnvPresent ~ week + Block + Tavg + Tmax + Tmin + PrecipTotal + AvgSpeed + Species2 + lo(Latitude, Longitude),
           data = train1, family="binomial")
p2<-predict(fitCv, newdata = train2, type = "response")
## check for a reasonable AUC of the model against unseen data (2011)
auc(train2$WnvPresent,p2)

## now fit a new model to all the data, so that our final submission includes information learned from 2011 as well
test_with_weather <- test_with_weather[with(test_with_weather, order(Id)), ]
fitSubmit <- update(fitCv, data=my.train)
pSubmit<-predict(fitSubmit, newdata = test_with_weather, type = "response")
## look at the predicted distribution (AUC doesn't care about probabilities; just ordering. It's still a good diagnostic)
summary(pSubmit)

submissionFile <- cbind(test_with_weather$Id,pSubmit)
colnames(submissionFile) <- c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"Output/GAM_moar_weather_4.csv",row.names=FALSE,quote=FALSE)
