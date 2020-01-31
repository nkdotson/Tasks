library(paleobioDB)
Taxon <- "Dinosauria"
MinMA <- 66
MaxMA <- 252
fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
Res <- 5
nspeciesOverTime <- pbdb_richness(fossils, rank = "genus", temporal_extent = c(MaxMA,MinMA), res=Res)
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime)), nspeciesOverTime[,2], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", main = Taxon)
newspeciesOverTime <- pbdb_orig_ext(fossils, res=5, rank="species", temporal_extent=c(MinMA, MaxMA))
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime)), newspeciesOverTime[,1], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", main = Taxon)
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime)), newspeciesOverTime[,2], col="red")
legend("topleft", legend=c("first appear", "go extinct"), col=c('black', 'red'), lty=1, bty="n")
#The number of species going extinct seems to rise as the number of species appearing goes down. 
#Im surprised how cleanly it seems to follow that pattern, but after thinking about it, it does make a decent amount of sense.
#Dinosaurs seem to have been evolving the fastest at around 60-50 million years ago
OceanCol <- "light blue"
LandCol <- "black"
Cols <- c('#fee5d9', '#fcae91', '#fb6a4a', '#de2d26', '#a50f15')
par(las=0)
pbdb_map_richness(fossils, col.ocean=OceanCol, col.int=LandCol, col.rich=Cols)
MinMA <- 201
MaxMA <- 252
triassic_fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
MinMA <- 145
MaxMA <- 201
jurassic_fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
MinMA <- 66
MaxMA <- 145
cretaceous_fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
dev.new(height = 7.8, width = 13)
pbdb_map_richness(triassic_fossils, col.ocean=OceanCol, col.int=LandCol, col.rich=Cols)
mtext(side=3, "Triassic (252 - 201Ma)", cex=3, line=-2)
dev.new(height = 7.8, width = 13)
pbdb_map_richness(jurassic_fossils, col.ocean=OceanCol, col.int=LandCol, col.rich=Cols)
mtext(side=3, "Jurassic (201 - 145Ma)", cex=3, line=-2)
dev.new(height = 7.8, width = 13)
pbdb_map_richness(cretaceous_fossils, col.ocean=OceanCol, col.int=LandCol, col.rich=Cols)
mtext(side=3, "Cretaceous (145 - 66Ma)", cex=3, line=-2)
Taxon2 <- "Mammalia"
MinMA <- 66
MaxMA <- 252
fossils2 <- pbdb_occurrences(base_name = Taxon2, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
nspeciesOverTime2 <- pbdb_richness(fossils2, rank = "genus", temporal_extent = c(MaxMA,MinMA), res=Res)
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
Col_dino <- Cols[length(Cols)]
Col_mammal <- Cols[1]
LineWidth <- 2
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime)), nspeciesOverTime[,2], xlim=c(MaxMA,MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", col = Col_dino, lwd=LineWidth)
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime2)), nspeciesOverTime2[,2], col = Col_mammal, lwd=LineWidth)
legend("topleft", legend=c(Taxon, Taxon2), col=c(Col_dino, Col_mammal), bty="n", lwd=LineWidth)
Taxon3 <- "Reptilia"
MinMA <- 0.0017
MaxMA <- 150
fossils3 <- pbdb_occurrences(base_name = Taxon3, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
nspeciesOverTime3 <- pbdb_richness(fossils3, rank = "genus", temporal_extent = c(MaxMA,MinMA), res=Res)
Taxon4 <- "Gastropoda"
MinMA <- 0.0017
MaxMA <- 150
fossils4 <- pbdb_occurrences(base_name = Taxon4, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
nspeciesOverTime4 <- pbdb_richness(fossils4, rank = "genus", temporal_extent = c(MaxMA,MinMA), res=Res)
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
Col_reptilia <- Cols[length(Cols)]
Col_gastropoda <- Cols[1]
LineWidth <- 2
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime3)), nspeciesOverTime3[,2], xlim=c(MaxMA,MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", col = Col_reptilia, lwd=LineWidth)
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime4)), nspeciesOverTime4[,2], col = Col_gastropoda, lwd=LineWidth)
legend("topleft", legend=c(Taxon3, Taxon4), col=c(Col_reptilia, Col_gastropoda), bty="n", lwd=LineWidth)
# the relationship between the number of gastropods and reptiles could be due to the fact that the gastropods that don't have shells don't fossilize well, as well as the fact that many reptiles live in dryer climates, while gastropods tend to live in more moist environments, where erosion is more prevelantIt seems possible that there is a negative correlation between reptiles and gastropods in terms of producing fossils.

#extra credit
install.packages("rfishbase")
library("rfishbase")
library("dplyr")
catfish <- common_to_sci("catfish")
species(catfish$species)
Dat <- species(catfish$Species, fields=c("Species", "Length", "Weight"))
Dat
CatfishData <- data.frame(Dat)
CatfishData
CatfishData2 <- na.omit(CatfishData)
CatfishData2
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
plot(CatfishData2$Weight, CatfishData2$Length, pch=16, xlab="Weight in Grams", ylab="Length in cm", main = "Catfish Length vs. Weight")
CatfishData3 <- which(CatfishData2$Weight < 120000 & CatfishData2$Length < 250)
CatfishData3
CatfishData4 <- CatfishData2[CatfishData3,]
CatfishData4
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
plot(CatfishData4$Weight, CatfishData4$Length, pch=16, xlab="Weight in Grams", ylab="Length in cm", main = "Catfish Length vs. Weight", xlim=c(0, 120000), ylim=c(0, 250))
abline(lm(CatfishData4$Length ~ CatfishData4$Weight), col = "red")
#end