setwd("C:\\Users\\PCUSER\\Desktop\\Evolution\\Tasks\\Task_02")
Data <- read.csv("http://jonsmitchell.com/data/beren.csv", stringsAsFactors=F)
Data
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
Feeds <- which(Data[,9] == "bottle")
Feeds
berenMilk <- Data[Feeds,]
head(berenMilk)
Feeds <- which(Data[, "event"] == "bottle")
Feeds
Feeds <- which(Data$event == "bottle")
Feeds
dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse="-"))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == "birth")]
Data2 <- Data
Data3 <- Data2[order(Data2$age),]
head(Data)
head(Data2)
head(Data3)
write.csv(Data3, "beren_new.csv", quote=F, row.names=FALSE)
Feeds <- which(Data3$event == "bottle")
avgMilk <- mean(Data3$value [Feeds])
avgFeed <- tapply(Data3$value [Feeds], Data3$age [Feeds], mean)
head(avgFeed)
avgFeed
varFeed <- tapply(Data3$value [Feeds], Data3$age [Feeds], var)
totalFeed <- tapply(Data3$value [Feeds], Data3$age [Feeds], sum)
numFeeds <- tapply(Data3$value [Feeds], Data3$age [Feeds], length)
varFeed
totalFeed
numFeeds
cor(Data3$value [Feeds], Data3$age [Feeds])
cor.test(Data3$value [Feeds], Data3$age [Feeds])
DataCor <- cor.test(Data3$value [Feeds], Data3$age [Feeds])
summary(DataCor)
DataAnova <- aov(Data3$value [Feeds] ~ Data3$caregiver [Feeds])
DataAnova
boxplot(Data3$value [Feeds] ~ Data3$caregiver [Feeds], xlab = "who gave the bottle", ylab = "amount of milk consumed (oz)")
?par
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height = 4, width = 4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab= "age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()

#extra credit assignment
which(Data3$event == "nap")
Data4 <- which(Data3$event == "nap")
Data4
DataNap <- Data3[Data4,]
StartTime <- DataNap$start_hour + (DataNap$start_minute / 60)
EndTime <- DataNap$end_hour + (DataNap$end_minute / 60)
DataNap$nap_length_in_hours <- EndTime - StartTime [which(DataNap$event == "nap")
DailyNapTime <- tapply(DataNap$nap_length_in_hours, DataNap$age, sum)
plot(as.numeric(names(DailyNapTime)), DailyNapTime, type="b", pch=16, xlab="age in days", ylab="hours of nap time")
pdf("r02bxc-DailyNapTime.pdf", height = 4, width = 4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(DailyNapTime)), DailyNapTime, type="b", pch=16, xlab="age in days", ylab="hours of nap time")
dev.off()
DailyNapCor <- cor.test(StartTime, DataNap$nap_length_in_hours)
#The relationship between the nap starting time and total duration is a slightly negative corralation. The earlier it is in the day, the longer the nap generally tends to be, and vice versa.