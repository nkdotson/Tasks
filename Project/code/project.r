setwd("C:\\Users\\PCUSER\\Desktop\\Evolution\\Tasks\\Project\\data")
MothData <- read.csv(file = "Appendix_Raw_Winglength.csv")
head(MothData)
nrow(MothData)
#added genus/species names to data based on other data in the collection.

plot(MothData$FWL, MothData$Elevation, type="p", pch=19, xlab="Moth Wingspan Length (mm)", ylab="Elevation Moth was found at (meters)", main="Moth Wingspan vs Elevation", col="blue")
Line <- lm(MothData$Elevation ~ MothData$FWL)
abline(Line, col="red")
cor(MothData$FWL, MothData$Elevation)
#First graph completed
#Winglength vs Elevation
#0-500 meter elevation
Mothrange1 <- which(MothData$Elevation <= 500 )
MothRange1 <- MothData[Mothrange1,]
#501-1000 elevation

#1001-1500 elevation

#1501-2000 elevation

#2001-2500 elevation

#2501-3000 elevation
