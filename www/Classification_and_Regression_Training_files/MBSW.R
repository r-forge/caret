###################################################
###################################################
library(caret)
library(Rgraphviz)
library(pls)
library(rpart)
library(partykit)
library(gbm)
library(svmpath)
library(ROCR)
library(kernlab)
library(randomForest)
library(class)
library(e1071)
library(corrplot)
library(mlbench)
library(snow)
data(Sonar)
options(width=80)

plotTheme <- caretTheme()
plotTheme$superpose.symbol$col[2] <- rgb(50/255, 136/255, 189/255, alpha = .5)

###################################################
###################################################
## A matrix of integers
aMatrix <- matrix(1:6, ncol = 3)
aMatrix
## Peal off the 2nd column to make a vector
aMatrix[,2]
## Is this still a matrix?
is.matrix(aMatrix[,2])
is.vector(aMatrix[,2])
## Let's keep it a matrix
is.matrix(aMatrix[,2,drop = FALSE])
is.vector(aMatrix[,2,drop = FALSE])


###################################################
###################################################
aDataFrame <- data.frame(a = letters[1:4],
                         b = runif(4))
aDataFrame
## The str() function is the best way of looking at
## an object in R:
str(aDataFrame)
## Two ways of accessing a column of a data frame:
aDataFrame[,1]
aDataFrame$a


###################################################
###################################################
load("raw.RData")
data <- raw
data$result <- factor(as.character(data$DIAG),
                      levels = c("AD", "C"))
data$DIAG <- NULL


###################################################
###################################################
dim(data)
# values of the response variable
str(data$result)
# frequency distribution of the outcome
table(data$result)
# look at first 4 predictors
str(data[, 1:4])


###################################################
###################################################
# Indictor variables for two of the six possible genotypes
table(data$e2_e2)
table(data$e3_e3)


###################################################
###################################################
interlukins <- grep("^IL", names(data), value = TRUE)
interlukins
apoe <- grep("^Apo", names(data), value = TRUE)                  
apoe


###################################################
###################################################
splom(data[, apoe],
      groups = data$result,        # color symbols by AD status
      auto.key = TRUE,             # produce a symbol key
      # pscales will make get rid of the tick marks to make the 
      # plot a little cleaner
      pscales = 0)

###################################################
###################################################
featurePlot(x = data[, apoe], 
            y = data$result, 
            plot = "strip",
            # "jitter" the points a little to remove overlap
            jitter.data = TRUE,
            # this is a lattice option that unlinks the y-axis scale
            scales = list(relation = "free"))

###################################################
###################################################
corMat <- cor(data[, c(apoe, interlukins)])
corrplot(corMat,
         # reorder the columns/rows using clustering
         order = "hclust")

###################################################
###################################################
set.seed(1)
inTrain <- createDataPartition(data$result,
                               times = 1,
                               p = 2/3)

## returns a list of length 1: the index of samples in the training set
str(inTrain)

training <- data[ inTrain[[1]], ]
test     <- data[-inTrain[[1]], ]

table(training$result)
table(test$result)


###################################################
###################################################

print(
      xyplot(V11 ~ V12,
            data = Sonar,
             groups = Class,
             aspect = 1,
             auto.key = list(columns = 2),
            xlab = "Predictor A",
            ylab = "Predictor B"))

###################################################
###################################################
trainX <- training[, names(training) != "result"]
preProcValues <- preProcess(trainX, method = c("center", "scale"))


###################################################
###################################################
scaledTrain <- predict(preProcValues, trainX)
scaledTrain$result <- training$result


###################################################
###################################################
testX <- test[, names(test) != "result"]
scaledTest <- predict(preProcValues, testX)
scaledTest$result <- test$result


###################################################
###################################################
corMat <- cor(trainX)
dim(corMat)
# the distribution of pair-wise correlations:
summary(corMat[upper.tri(corMat)])
remove <- findCorrelation(corMat, cutoff = .75)
## The column numbers of the predictors to remove
remove
trainX2 <- trainX[, -remove]


###################################################
###################################################
rpart1 <- rpart(result ~ .,
                data = training,
                control = rpart.control(maxdepth = 2))
rpart1


###################################################
###################################################
rpart1a <- as.party(rpart1)
plot(rpart1a)


###################################################
###################################################

print(
      xyplot(Age ~ Cortisol,
             data = training,
             groups = result,
             panel = function(...)
             {
               panel.xyplot(...)
               panel.segments(0, 77.5, 3, 77.5)
               panel.segments(2.070034, 77.5, 2.070034, 150)         
               
               panel.text(2.3, 98, "Predicted as AD",
                          col = trellis.par.get()$superpose.symbol$col[1])
               
               panel.text(1.7, 98, "Predicted as C",
                          col = trellis.par.get()$superpose.symbol$col[2])
               
               panel.text(2, 51, "Depends on the E4 Genotype",
                          col = "black")
               
             }, auto.key = list(columns = 2)))


###################################################
###################################################
rpartFull <- rpart(result ~ ., data = training)


###################################################
###################################################
rpartFulla <- as.party(rpartFull)
plot(rpartFulla)

###################################################
###################################################
rpartFull


###################################################
###################################################
rpartPred <- predict(rpartFull, test, type = "class") # returns a factor vector
confusionMatrix(rpartPred, test$result)               # requires 2 factor vectors


###################################################
###################################################
# the gbm function does not accept factor response values so
# will make a copy and modify the result variable
forGBM <- training
forGBM$result <- ifelse(forGBM$result == "AD", 1, 0)

gbmFit <- gbm(formula = result ~ .,       # try all predictors
              distribution = "bernoulli", # for classification
              data = forGBM,
              n.trees = 500,              # 500 boosting iterations
              interaction.depth = 1,      # how many splits in each tree
              shrinkage = 0.1,            # learning rate
              bag.fraction = 0.8,         # % resampling within the algorithm
              verbose = FALSE)            # do not print the details


###################################################
###################################################
gbm.perf(gbmFit, method = "OOB", oobag.curve = TRUE)


###################################################
###################################################
# use the resampled likelihood estimates to determine the number of trees
gbm.perf(gbmFit, method = "OOB")


###################################################
###################################################
gbmGrid <- expand.grid(.interaction.depth = seq(1, 7, by = 2),
                       .n.trees = seq(50, 500, by = 50),
                       .shrinkage = 0.1)
# number of parameter sets:
nrow(gbmGrid)


###################################################
###################################################
set.seed(2)
gbmTune <- train(result ~ .,             # train allows a factor outcome variable
                 data = training,
                 method = "gbm",
                 metric = "Kappa",       # Determine the best gbm model using the
                                         #   Kappa statistic.
                 tuneGrid = gbmGrid,     # Use a custom grid of tuning parameters
                                         # Do 50 iterations bootstrap samples
                                         #   to estimate performance
                 trControl = trainControl(number = 50,
                                          returnResamp = "all",
                                          verboseIter = FALSE),
                 verbose = FALSE,        # These last two options are directly
                 bag.fraction = 0.8)     #   passed to the gbm function.


###################################################
###################################################
print(plot(gbmTune))
print(plot(gbmTune, metric = "Kappa"))



###################################################
###################################################
print(gbmTune, printCall = FALSE)


###################################################
###################################################
preds <- extractPrediction(list(boostModel = gbmTune),
                           testX = test[, names(test) != "result"],
                           testY = test$result)
head(preds)
table(preds$dataType)


###################################################
###################################################
trainPred <- predict(gbmTune, training)    # this uses predict.train
confusionMatrix(trainPred, training$result)


###################################################
###################################################
gbmPred <- predict(gbmTune, test)     # this uses predict.train
confusionMatrix(gbmPred, test$result)


###################################################
###################################################
xyplot(gbmTune, 
       subset = interaction.depth == 1, 
       metric = "Kappa",
       # plot a grid, the data points and connect the averages
       type = c("g", "p", "a"))


###################################################1
###################################################
print(
      xyplot(gbmTune, 
       subset = interaction.depth == 1, 
              metric = "Kappa",      
       # plot a grid, the data points and connect the averages
       type = c("g", "p", "a")))



###################################################
###################################################
# similar to extractPrediction
testProbs <- extractProb(list(boosted = gbmTune), 
                         testX = test[, names(test) != "result"], 
                         testY = test$result)
testProbs <- subset(testProbs, dataType == "Test")
head(testProbs)

# alternatively, using predict.gbm:
predict(gbmTune$finalModel, newdata = testX, n.trees = 500, type = "response")[1:5]


###################################################
###################################################
print(
      histogram(~AD|obs, 
          data = testProbs,
          layout = c(1, 2), 
          xlab = "AD CLass Probability"))



###################################################
###################################################
library(ROCR)
# inputs are the class probabilities and the true classes
rocrObject <- prediction(testProbs$C, testProbs$obs)


###################################################
###################################################
# compute the true- and false-positive rates
rocCurve <- performance(rocrObject,"tpr","fpr")


###################################################
###################################################
# Area under the ROC curve
# best possible = 1, no-information value = 0.5
performance(rocrObject,"auc")@y.values


###################################################
###################################################
plot(rocCurve)

###################################################
###################################################
set.seed(2)

n <- 50

data1 <- cbind(rnorm(n, mean = -3),
               rnorm(n, mean = -3))

data2 <- cbind(rnorm(n, mean = 3),
               rnorm(n, mean = 3))

data3 <- rbind(data1, data2)

plot(data3[,1], data3[,2],
     type = "n",
     xlab = "Predictor 1",
     ylab = "Predictor 2")
points(data1[,1], data1[,2], pch = 1, col = "darkblue")
points(data2[,1], data2[,2], pch = 2, col = "darkred")

p <- 20
for(i in 1:p) abline(runif(p, min = -1, max = 1),
                     runif(p, min = -3, max = -0),
                     col = sample(colors(), 1))

y <- rep(c(-1, 1), each = n)


###################################################
###################################################

fit <- svmpath(data3, y)

index <- 128
alpha0 <- fit$alpha0[index]
alpha <- fit$alpha[, index]
lambda <- fit$lambda[index]

plot(data3[,1], data3[,2],
     type = "n",
     xlab = "Predictor 1",
     ylab = "Predictor 2")
beta <- (alpha * y) %*% data3
abline(-alpha0/beta[2], -beta[1]/beta[2], col = "darkorange", lwd = 3)
abline(lambda/beta[2] - alpha0/beta[2], -beta[1]/beta[2], 
       col = "darkorange", lwd = 2, lty = 3)
abline(-lambda/beta[2] - alpha0/beta[2], -beta[1]/beta[2], 
       col = "darkorange", lwd = 2, lty = 3)

points(data1[,1], data1[,2], pch = 1, col = "darkblue")
points(data2[,1], data2[,2], pch = 2, col = "darkred")

## which are sv?
dist1 <- data1[,1]^2 + data1[,2]^2
plotThese <- which(rank(dist1) <= 2)
points(data1[plotThese,1], data1[plotThese,2], pch = 16, col = "darkblue")

dist2 <- data2[,1]^2 + data2[,2]^2
plotThese <- which(rank(dist2) <= 1)
points(data2[plotThese,1], data2[plotThese,2], pch = 17, col = "darkred")

###################################################
###################################################
set.seed(2)
svmTune <- train(result ~ .,
                 data = scaledTrain,
                 method = "svmRadial",
                 # The default grid of cost parameters go from 10^-1,
                 # 1, 10, ... 
                 # We'll fit 4 values in that sequence via the tuneLength
                 # argument.
                 tuneLength = 4,
                 metric = "Kappa",
                 trControl = trainControl(number = 50))


###################################################
###################################################
print(plot(svmTune, xTrans = function(x) log10(x)))
print(plot(svmTune, metric = "Kappa", xTrans = function(x) log10(x)))



###################################################
###################################################
svmTune


###################################################
###################################################
svmTune$finalModel


###################################################
###################################################
svmPred <- predict(svmTune, scaledTest)
confusionMatrix(svmPred, scaledTest$result)


###################################################
###################################################
qsarData <- read.delim("Karthikeyan.txt")
qsarData <- qsarData[complete.cases(qsarData),]


###################################################
###################################################
qsarData <- read.delim("Karthikeyan.txt")
qsarData <- qsarData[complete.cases(qsarData),]


###################################################
###################################################
set.seed(2)

inTrain <- createDataPartition(qsarData$MTP,
                               times = 1,
                               p = 3/4)

qsarTest  <- qsarData[-inTrain[[1]],]
# the predictors are in columns >= 4
qsarTestX <- qsarTest[, -(1:3)]
qsarTestY <- qsarTest$MTP
nrow(qsarTest)

# also create our training set
qsarTrain  <- qsarData[ inTrain[[1]],]
qsarTrainX <- qsarTrain[, -(1:3)]
qsarTrainY <- qsarTrain$MTP
nrow(qsarTrain)


###################################################
###################################################
preProc <- preProcess(qsarTrainX)
processedTrainX <- predict(preProc, qsarTrainX)


###################################################
###################################################
processedTestX <- predict(preProc, qsarTestX)


###################################################
###################################################
gbmControl <- trainControl(method = "cv",
                           verboseIter = FALSE)


###################################################
###################################################
gbmGrid <- expand.grid(.interaction.depth = seq(1, 7, by = 2),
                       .n.trees = seq(50, 500, by = 50),
                       .shrinkage = 0.1)


set.seed(3)
gbmQSAR <- train(qsarTrainX, qsarTrainY,  # We use the matrix interface now         
                 method = "gbm",
                 tuneGrid = gbmGrid,   
                 trControl = gbmControl,
                 verbose = FALSE,       
                 bag.fraction = 0.8,    
                 train.fraction = 1.0)

###################################################
###################################################
print(plot(gbmQSAR))
print(plot(gbmQSAR, metric = "Rsquared"))



###################################################
###################################################
print(gbmQSAR, printCall = FALSE)


###################################################
###################################################
gbmPred <- extractPrediction(list(gbm = gbmQSAR),
                             testX = qsarTestX,
                             testY = qsarTestY)
gbmPred <- subset(gbmPred, 
                  dataType == "Test")
defaultSummary(gbmPred)

# from resampling
subset(gbmQSAR$results,
       interaction.depth == gbmQSAR$bestTune$.interaction.depth &
       n.trees == gbmQSAR$bestTune$.n.trees)



###################################################
###################################################
print(xyplot(obs ~ pred, data = gbmPred, type = c("g", "p")), xlim = c(25, 360), ylim = c(25, 360))  
 


###################################################
###################################################
matrixData <- matrix("", nrow = 15, ncol = 2)
matrixData[1:3, 1] <- "Predictor3"
matrixData[1:3, 2] <-  paste("Comp", 1:3, sep = "")
matrixData[4:6, 1] <- "Predictor4"
matrixData[4:6, 2] <-  paste("Comp", 1:3, sep = "")
matrixData[7:9, 1] <- "Predictor1"
matrixData[7:9, 2] <-  paste("Comp", 1:3, sep = "")
matrixData[10:12, 1] <- "Predictor2"
matrixData[10:12, 2] <-  paste("Comp", 1:3, sep = "")
matrixData[13, 1] <- "Comp1"
matrixData[14, 1] <- "Comp2"
matrixData[15, 1] <- "Comp3"
matrixData[13:15, 2] <-  "Outcome"

allEdges <- paste(matrixData[,1], "~", matrixData[,2], sep = "")

graphData <- ftM2graphNEL(matrixData)

nodeRenderInfo(graphData) <- list(shape = "ellipse") 
nodeRenderInfo(graphData) <- list(shape =
                                  c(Outcome = "box",
                                    Comp1 = "circle",
                                    Comp2 = "circle",
                                    Comp3 = "circle"))

nodeRenderInfo(graphData) <- list(fill =
                                  c(Outcome = "#A6CEE3",
                                    Predictor1 = "#B2DF8A",
                                    Predictor2 = "#B2DF8A",
                                    Predictor3 = "#B2DF8A",
                                    Predictor4 = "#B2DF8A",
                                    Comp1 = "#FDBF6F",
                                    Comp2 = "#FDBF6F",
                                    Comp3 = "#FDBF6F")) 
graph.par(
          list(nodes =
               list(
                    col = "black",
                    lty = "solid", 
                    lwd = 2,
                    fontsize = 14))) 

graphData <- layoutGraph(graphData) 
renderGraph(graphData)


###################################################
###################################################
set.seed(3)
plsQSAR <- train(processedTrainX, qsarTrainY,
                method = "pls",
                # Try 1...12 PLS components
                tuneLength = 12,
                trControl = trainControl(method = "cv"))
                


###################################################
###################################################
print(plsQSAR, printCall = FALSE)             


###################################################
###################################################
print(plot(plsQSAR))
print(plot(plsQSAR, metric = "Rsquared"))



###################################################
###################################################
plsPred <- extractPrediction(list(pls = plsQSAR),
                             testX = processedTestX,
                             testY = qsarTestY)
plsPred <- subset(plsPred, 
                  dataType == "Test")
defaultSummary(plsPred)

# from resampling
subset(plsQSAR$results,
       ncomp == plsQSAR$bestTune$.ncomp)



###################################################
###################################################
print(xyplot(obs ~ pred, data = plsPred, type = c("g", "p")))  


###################################################
###################################################
toLatex(sessionInfo(), locale=FALSE)


###################################################
###################################################
rpartImp <- varImp(rpart1, scaled = FALSE) 
head(rpartImp)


###################################################
###################################################
## Only predictors with non-zero importance
head(subset(rpartImp, Overall != 0))  


###################################################
###################################################
## gbmTune is an object with class "train".
## This calls varImp.train which then calls varImp.gbm
gbmImp <- varImp(gbmTune, scale = FALSE) 
print(gbmImp, top = 5)


###################################################
###################################################
print(plot(gbmImp, top = 20))

###################################################
###################################################
## Using the QSAR PLS regression model
plsImp <- varImp(plsQSAR, scale = FALSE)  
print(plsImp, top = 5)


###################################################
###################################################
print(plot(plsImp, top = 20))


###################################################
###################################################
smootherImp <- filterVarImp(qsarTrainX, 
                            qsarTrainY, 
                            ## use a loess smoother instead of linear model
                            nonpara = TRUE)
head(smootherImp)

## order the rows from best R-squared to worst
smootherImp <- smootherImp[order(smootherImp$Overall, decreasing = TRUE),, drop = FALSE]


###################################################
###################################################
## The 8 best predictors
top8 <- rownames(smootherImp)[1:8]

## Plot the relationship with a smoothed line
featurePlot(qsarTrainX[, top8],
            qsarTrainY,
            ## below: p = points, g = gird
            type = c("p", "g", "smooth"),
            degree = 2)


###################################################
###################################################
print(
      featurePlot(qsarTrainX[, top8],
            qsarTrainY,
            type = c("p", "g", "smooth"),
            degree = 2))

###################################################
###################################################
rocImp <- filterVarImp(training[, colnames(training) != "result"],
                       training$result)

## An auc is computed for each class
head(rocImp)


###################################################
###################################################
rocImp <- rocImp[order(rocImp$AD, decreasing = TRUE),]

## The 8 best predictors
top8 <- rownames(rocImp)[1:8]

featurePlot(training[, top8],
            training$result,
            scales = list(y = list(relation = "free")),
            plot = "box")


###################################################
###################################################
print(
      featurePlot(training[, top8],
                  training$result,
                  scales = list(y = list(relation = "free")),
                  plot = "box"))


