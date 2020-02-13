source("http://jonsmitchell.com/code/fxn05.R")
Pop1 <- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h = 1, s = 0)
plot(1:nrow(Pop1), Pop1[,1], ylim=c(0, 1), type ="l", xlab="generation", ylab="allele freq.", lwd=2)
lines(1:nrow(Pop1), Pop1[,2], lwd=2, col="red")
legend("topleft", legend = c("a", "b"), col = c("black", "red"), lwd = 2, bty="n")
plotFit(nruns=10, n=50, ngens=100, init_p=0.5, h=1, s=0)
Expectation <- c(10, 10, 10, 10)
Observed <- c(15, 15, 5, 5)
Chisq <- sum(((Expectation - Observed) ^ 2) / Expectation)
Chisq
barplot(rbind(Expectation, Observed), beside = T, main=bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
Observed <- c(5, 0, 0, 35)
barplot(rbind(Expectation, Observed), beside = T, main=bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
Observed <- c(2, 3, 10, 30)
barplot(rbind(Expectation, Observed), beside = T, main=bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
Observed <- c(0, 0, 0, 40)
barplot(rbind(Expectation, Observed), beside = T, main=bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
Observed <- c(10, 10, 10, 10)
barplot(rbind(Expectation, Observed), beside = T, main=bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
# The X^2 value is always 10 in this scenario, meaning that the X^2 value isn't effected by how uneven the values are.

results <- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors=F)
counts <- results[,c("yellow","red","green","blue","black","tan")]
backgrounds <- c("White","Red","Yellow","Green","Blue","Black")
backgroundCol <- c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
calcChi(counts[1,])
Chisqs <- apply(counts, 1, calcChi)
plotChis(counts)
plotChis(counts)
plotChis(counts)
#The bars are more uneven as the Chi squared value gets higher, and vice versa
#This shows that a higher chi squared value means that the data values are less uniform and more lopsided.
#Smaller chi sqaured values reflect a more even distribution of data values that are closer to one another.
Avg <- mean(Chisqs)
Avg
#The avergage is 60.99081, which is much higher than 11.70
backgroundAvgs <- tapply(Chisqs, results[,3], mean)
backgroundAvgs
#Yes, it does vary by background
propSig <- length(which(Chisqs > 11.70)) / length(Chisqs)
percSig <- round(100 * propSig)
percSig
#Almost certainly not
par(las=1,mar=c(4,4,1,1),mgp=c(2,0.5,0),tck=-0.01,cex.axis=1)
hist(Chisqs, main="",xlab="chi-squared values",ylab="frequency")
par(las=1,mar=c(4,4,1,1),mgp=c(2,0.5,0),tck=-0.01,cex.axis=1)
plot(1,1,xlim=c(0,400),ylim=c(1,8.5),xlab="",ylab="",type="n",yaxt="n")
axis(2,at=1:length(backgrounds),labels=backgrounds)
mtext(side=1,expression(chi^2),cex=1.75,line=2.5)
counter <- 1
for (i in backgrounds) {
Data <- Chisqs[which(results[,3] == i)]
addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
counter <- counter + 1
}
abline(v=11.70, lty=2,lwd=2,col="black")
#The backgrounds don't seem crazily different from each other when looking at the plot. Some are longer/shorter by a little, but they largely have the same shape.
#Although black and white don't seem to have as many extreme cases towards the end of the x-axis.
Simulation <- simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")
abline(v=11.70, lty=2, lwd=2)
propSigSim <- length(which(Simulation > 11.70)) / length(Simulation)
percSigSim <- round(100 * propSigSim)
percSigSim
percSig
#The simulation has an "significant" result 89% percent of the time, which is 3% lower than the observed in the lab. This is likely because some of the students were thinking too much about the experiment, doing the opposite of what was expected out of spite, or possibly just weren't paying attention.
Fit <- c(1,1,1,1,1,1)
names(Fit) <- 1:6
Simulation2 <- simDraws(1e4, w = Fit)
addHist(Y=8,Dat=Simulation2,Color=rgb(0,0,0,0.25))
Fit <- c(0.1,1,1,1,1,1)
names(Fit) <- 1:6
Simulation3 <- simDraws(1e4, w = Fit)
addHist(Y=8,Dat=Simulation3,Color=rgb(0,0,0,0.25))
Fit <- c(0.5,0.6,0.7,1,1,1)
names(Fit) <- 1:6
Simulation4 <- simDraws(1e4, w = Fit)
addHist(Y=8,Dat=Simulation4,Color=rgb(0,0,0,0.25))
Fit <- c(0.1,0.2,0.3,0.4,0.5,1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4, w = Fit)
addHist(Y=8,Dat=Simulation5,Color=rgb(0,0,0,0.25))
Fit <- c(0.1,0.1,0.1,0.1,0.1,1)
names(Fit) <- 1:6
Simulation6 <- simDraws(1e4, w = Fit)
addHist(Y=8,Dat=Simulation6,Color=rgb(0,0,0,0.25))
Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0,0,1,0.25))
#The mixture looks similar in the bieginning but has far more extreme examples ar the end.
#Most student groups don't seem to show evidence of strong selection.
#It seems that many of the groups chose at complete random for any of the given backgrounds, but a few groups each year strongly selected. Selection overall is very weak, but likely strong in a very condensed amount of groups.
#Random Selection seems to be demonstated in the 112 lab
#Natural Selection is demonstrated in the simulations
#That random selection seems to be a fairly weak in terms of changing the overall population, becuase of the fact that It's random
#Comparing student numbers to the simulated numbers, because it gives you the inference that the students don't actually seem to be selecting the toothpicks according to the insturctions because their own concious and thought is messing with the experiment.
# The X^2 values would likely rise, as certain colors would change into others, changing the balance of the colors, resulting in higher values over time

