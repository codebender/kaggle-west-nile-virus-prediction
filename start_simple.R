#load dependencies

#load data
train = read.csv("Data/train.csv", header = TRUE, stringsAsFactors = FALSE)
test = read.csv("Data/test.csv", header = TRUE, stringsAsFactors = FALSE)
spray = read.csv("Data/spray.csv", header = TRUE, stringsAsFactors = FALSE)
weather = read.csv("Data/weather.csv", header = TRUE, stringsAsFactors = FALSE)

train$Block <- factor(as.character(train$Block))
test$Block <- factor(as.character(test$Block))
test$Block[test$Block == 26] <- 25
train$month <- factor(format(as.POSIXct(train$Date, format="%Y-%m-%d"), format="%B"))
test$month <- factor(format(as.POSIXct(test$Date, format="%Y-%m-%d"), format="%B"))

block_month_mean <- aggregate(WnvPresent ~ Block + month, data=train, mean)
merged <- merge(test, block_month_mean, by=c('Block', 'month'), all.x=TRUE)
merged <- merged[order(merged$Id),]
merged$WnvPresent[is.na(merged$WnvPresent)] <- 0.0

submit <- data.frame(Id = merged$Id, WnvPresent = merged$WnvPresent)
write.csv(submit, file = "Output/block_month_mean.csv", row.names = FALSE)
