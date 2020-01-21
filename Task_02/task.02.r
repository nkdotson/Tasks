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