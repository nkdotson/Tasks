library("ape")
library("phytools")
text.string <- "(((((((cow, pig), whale),(bat,(lemur,human))),(robin,iguana)),coelacanth),(gold_fish,trout)),shark);"
vert.tree <- read.tree(text=text.string)
plot(vert.tree, edge.width=2)
nodelabels(frame="circle",bg="white",cex=1)
# 1. gold fish are closer to humans because they both come from node 14
vert.tree
# 2. It says there aren't, so no
str(vert.tree)
tree <- read.tree(text="(((A,B),(C,D)),E);")
plotTree(tree,offset=1)
tiplabels(frame="circle",bg="lightblue",cex=1)
nodelabels(frame="circle",bg="white",cex=1)
tree$tip.label
tree$edge
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col="black",border="white",main="",xlab="edge lengths for the Anolis tree", ylim=c(0, 50),xlim=c(0, 6))
tipEdges <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths==min(Lengths))]
plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length,round,digits=2)
edgelabels(text=Labs, cex=0.25)
?plot.phylo
#3
plot(AnolisTree, cex=0.25, show.tip.label = FALSE)
#4
plot(AnolisTree, cex=0.25, type = "fan")
#5
plot(AnolisTree, cex=0.25, tip.color = "red")
#6
names(Lengths)[which(Lengths==min(Lengths))]
#7

#Anolis_occultus

ltt(AnolisTree)
abline(0,1,lwd=2,col="red",lty=2)
#It never goes down but it is horizontal for short bursts. New species appear but old ones never leave the lineage because they still existed, though they may not be around today.
?fit.bd()
#10
fit.bd(AnolisTree, rho=0.2)
# b = 0.8031
# d = 0