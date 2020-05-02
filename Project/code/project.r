setwd("C:\\Users\\PCUSER\\Desktop\\Evolution\\Tasks\\Project\\data")
MothData <- read.csv(file = "Appendix_Raw_Winglength.csv")
head(MothData)
nrow(MothData)
#added genus/species names to data based on other data in the collection.

plot(MothData$Elevation,log(MothData$FWL),type="p", pch=19, ylab="Log of Moth Wingspan Length (mm)", xlab="Elevation Moth was found at (m)", main="Moth Wingspan vs Elevation", col="blue")
Line <- lm(log(MothData$FWL) ~ MothData$Elevation)
abline(Line, col="red")
cor( MothData$Elevation, log(MothData$FWL))
summary(Line)
#Y intercept is 2.6
#First graph completed
#Winglength vs Elevation
#0-500 meter elevation
Mothrange1 <- which(MothData$Elevation <= 500 )
MothRange1 <- MothData[Mothrange1,]
MothRange1
#501-1000 elevation
Mothrange2 <- MothData$Elevation[(MothData$Elevation < 1001) & (MothData$Elevation > 500)]
MothRange2 <- MothData[Mothrange2,]
MothRange2
#1001-1500 elevation
Mothrange3 <- MothData$Elevation[(MothData$Elevation < 1501) & (MothData$Elevation > 1000)]

#1501-2000 elevation
Mothrange4 <- MothData$Elevation[(MothData$Elevation < 2001) & (MothData$Elevation > 1500)]

#2001-2500 elevation
Mothrange5 <- MothData$Elevation[(MothData$Elevation < 2501) & (MothData$Elevation > 2000)]

#2501-3000 elevation
Mothrange6 <- MothData$Elevation[(MothData$Elevation < 3000) & (MothData$Elevation > 2500)]

length(Mothrange1)
length(Mothrange2)
length(Mothrange3)
length(Mothrange4)
length(Mothrange5)
length(Mothrange6)
 
IndividualCount <- c(length(Mothrange1), 115, 233, 110, 196, 32)
plot(IndividualCount, type="h", axes=FALSE, col = "blue", xlab = "Elevation Range (m)", ylab = "Moth Frequency", main = "Moth Frequency vs. Elevation Ranges")
par(las=2, cex.axis = .8, col.axis = "red")
axis(1, at=1:6, labels=c("0-500m", "501-1000m", "1001-1500m", "1501-2000m", "2001-2500m", "2501-3000m"))
axis(2)
#finished 2nd figure
