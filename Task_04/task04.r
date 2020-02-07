trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
Sample1
Sample2
# Yes, the samples are different. Sample 1 has a larger range of values compared to the second Sample.
# The populations are the same size, but seemingly different individuals otherwize the values likely wouldn't be different.
boxplot(Sample1, Sample2)


source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandps_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")
head(MatGrandma)
nrow(MatGrandma)
head(MatGrandpa)
nrow(MatGrandpa)
head(PatGrandma)
nrow(PatGrandma)
head(PatGrandpa)
nrow(PatGrandpa)
Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)
Focus <- makeBaby(Brenda, Alan)
#50%
ToMom <- length(grep("mom", Focus))/length(Focus)
ToMom
# 25% with each
ToMomMom <- length( grep( "grandma_mom", Focus))/length(Focus)
ToMomDad <- length( grep( "grandpa_mom", Focus))/length(Focus)
ToMomMom
ToMomDad
# they didn't mtach what I expected
#Focus isn't equally related to each maternal grandparent
ToDadMom <- length( grep( "grandma_da", Focus))/length(Focus)
ToDadDad <- length( grep( "grandpa_da", Focus))/length(Focus)
ToDadMom
ToDadDad
#Paternal grandparents are also not equally related
TotalRelation <- ToDadDad + ToDadMom + ToMomDad + ToMomMom
TotalRelation
AvgRelation <- TotalRelation/4
AvgRelation
#The average relation is 23.89%, which is closer to what I originally expected.
Sibling_01 <- makeBaby(Brenda, Alan)
# 50%
ToSib <- length( intersect( Focus, Sibling_01))/length(Focus)
ToSib
# 49.86%, so very close, but not 50%
ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan)))/length(Focus))
ManySiblings
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="proportion of shared genes")
#The siblings can get a wide variety of DNA of each parent as they get half of their DNA from each one. There are many possible combinations.
#There are also things like recombination that can lead to a greater variety of relatedness. 
#It's unlikely that they would be incredibly far off from any given sibling, but it is possible, especially with 1000 individuals
HWE <- function(p) {
aa <- p^2
ab <- 2 * p * (1 - p)
bb <- (1 - p)^2
return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
plot(1, 1, type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="freq. allele a", ylab="geno. freq")
p <- seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p, HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
#aa increases as a increases and vice versa. Time and geographic space are not shown in this graph.
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"], lwd=2, col="blue")
legend("top", legend=c("aa","ab","bb"), col=c("red","purple","blue"), lty=1, lwd=2, bty="n")
Pop <- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
#Yes, all the points for the population are all close to Hardy_Weinburg
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=21, bg="red")
#The values are much more spread out because of the smaller sampling size
library(learnPopGen)
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)
x <- genetic.drift(Ne=100, nrep=5, pause=0.01)
x <- genetic.drift(Ne=20, nrep=5, pause=0.01)
#Smaller populations are much faster
PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
Line2 <- lm(tExt ~ Samples + 0)
abline(Line2, col="red")
#There seem to be many more points below the line, and they are generally farther away on the top. The points become more scattered as the populaiton size increases, implying that the alleles are getting fixed.





