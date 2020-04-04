setwd("C:\\Users\\PCUSER\\Desktop\\Evolution\\Tasks\\Project\\data")
MothData <- read.csv(file = "Appendix_Raw_Winglength.csv")
head(MothData)
nrow(MothData)
#added genus/species names to data based on other data in the collection.
plot?
plot(MothData$FWL, MothData$Elevation, type="p", col="blue")
Line <- lm(MothData$Elevation ~ MothData$FWL)
abline(Line, col="red")
#First graph completed
#Winglength vs Elevation


