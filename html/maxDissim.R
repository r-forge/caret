library(caret)
library(mlbench)
data(BostonHousing)

testing <- scale(BostonHousing[, c("age", "nox")])
set.seed(11)
## A random sample of 5 data points
startSet <- sample(1:dim(testing)[1], 5)
samplePool <- testing[-startSet,]
start <- testing[startSet,]
newSamp <- maxDissim(start, samplePool, n = 20)
head(newSamp)

########################################################

newSamp <- maxDissim(start, samplePool, n = subsetSize)

for(i in seq(along = newSamp))
  {
    png(paste("MaxDiss", i, ".png", sep = ""))
    plot(samplePool[,1], 
         samplePool[,2], 
         type = "n", 
         xlab = "age", 
         ylab = "nox")
    points(samplePool[-newSamp,1], 
           samplePool[-newSamp,2], 
           col = rgb(.2, .2, .2, .4),
           pch = 16,
           cex = 1)

    text(start[,1], 
         start[,2], 
         rep("S", dim(start)[1]), 
         col = "red", 
         cex = 1.5)
    text(samplePool[newSamp[1:i],1], 
         samplePool[newSamp[1:i],2], 
         paste(1:i), 
         col = "blue", 
         cex = 1.5)

    dev.off()
  }

## convert -delay 100 -size 495 -page +0+0  MaxDiss1.png MaxDiss2.png MaxDiss3.png MaxDiss4.png MaxDiss5.png MaxDiss6.png MaxDiss7.png MaxDiss8.png MaxDiss9.png MaxDiss10.png MaxDiss11.png MaxDiss12.png MaxDiss13.png MaxDiss14.png MaxDiss15.png MaxDiss16.png MaxDiss17.png MaxDiss18.png MaxDiss19.png MaxDiss20.png -loop 100 MaxDissim.gif
