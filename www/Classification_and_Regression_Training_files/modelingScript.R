########################################################
########################################################
## R Predictive Modeling Script version 1.15
##
## Author: Max Kuhn (max.kuhn@pfizer.com)
## 
## Original Date: 3/12/06
##
## Many choices are made for the user in this script. 
## These choices are not always good, but rough guidelines.
## 
## Please send bugs and suggestions to Max.

########################################################
## Assumptions:
##
## The software requirements are:
## 
##  - R version 2.5.1 or greater. You can get this from:
##
##    http://cran.r-project.org/
##  
##  - the caret package, version 3.59 or greater and many
##    dependencies. To get the full set, use:
##
##  install.pacakges("caret",
##                   dependencies = c("Depends", "Imports",
##                                    "Suggests"))
##
##  Parallel processing is implimented usng MPI (when
##  requested)

library(caret)

##
## This script assumes that you have:
##
##  - a data frame of predictor variables named xData. If
##    there are factors in the data frame, these should 
##    already be split into dummy variables. See the 
##    function model.matrix for this purpose.
##
##  - a vector of outcomes named yData. For regression, this
##    should be a numeric vector and for classification it 
##    should be a factor
##
## For both the predictors and outcome, we assume that 
## there are no missing values or that they have been 
## imputed.
##
## Example 1
##   library(mlbench)
##   data(PimaIndiansDiabetes)
##   xData <- PimaIndiansDiabetes[, 1:8]
##   yData <- PimaIndiansDiabetes$diabetes 
##   
##   
## Example 2
##   library(caret)
##   data(BloodBrain)
##   xData <- bbbDescr
##   yData <- logBBB

## Example 3
library(caret)
data(mdrr)
xData <- mdrrDescr
yData <- mdrrClass

    
########################################################
## Specifications
##    
## The following variables must be set:
## 
##  - model: with possible values: gbm, rf, svmpoly, 
##    svmradial, nnet, lvq, rpart, pls, pam, knn, nb, earth,
##    mars, fda, bagEarth, bagFDA, treebag, lm, multinom,
##    glmboost, gamboost, blackboost, ctree, cforest, 
##    enet, lasso, lda, rda
##    
##    Details can found found by opening R, loading caret 
##    and typing:
##    
##      vignette("caretTrain")
##
##    at the R prompt to open a pdf version. Also there is
##    a publication on caret at
##
##     http://www.jstatsoft.org/v28/i05
##

model <- "gbm"
  
##  - the type of resampling used. Possible techniques are 
##    bootstrapping (use "boot"), leave-group-out 
##    cross-validation ("LGOCV"), k-fold cross-validation 
##    ("cv") or leave-one-out cross-validation ("LOOCV")

resampleType <- "cv"

##  - the number of resamples. For bootstrapping and LGOCV, 
##    this is the number of iterations. For k-fold CV this 
##    is k
  
reampleN <- 10    
    
##  - the percentage of data used in the training set 
  
trainPct <- .80

##  - for models where we will filter the data to remove 
##    high correlations between predictors, the pair-wise 
##    correlation threshold

corrThreshold <- .5

##  - how many processor should be used? If > 1, Rmpi and
##    snow will need to be installed and working

numWorkers <- 1

##  - a random seed in case you want to reproduce the same 
##    train/test split or model

gridSize <- 5

set.seed(1)
    
########################################################
## Data Splitting

inTrain <- createDataPartition(yData,
                               trainPct, 
                               times = 1,
                               list = FALSE)

trainX <- xData[ inTrain,]
testX  <- xData[-inTrain,]

trainY <- yData[ inTrain]
testY  <- yData[-inTrain]

########################################################
## Pre-Processing

## first, remove near-zero variance descriptors (if needed)
if(suggestions(model)["nzv"])
{
  lowVar <- nearZeroVar(trainX)
  if(length(lowVar) > 1)
    {
      trainX <- trainX[, -lowVar, drop = FALSE]
      testX  <-  testX[, -lowVar, drop = FALSE]
    }
}   

## now, if needed, remove highly correlated predictors
if(suggestions(model)["corr"])
{
  ## check for zero variance just in case
  zeroVar <- unlist(lapply(trainX, function(u) length(unique(u)) > 1))
  if(length(zeroVar) > 0)
    {
      trainX <- trainX[, zeroVar, drop = FALSE]
      testX  <- testX[, zeroVar, drop = FALSE]
    }
  
  xCorr <- cor(trainX)
  highCorr <- findCorrelation(xCorr, corrThreshold)
  if(length(highCorr) > 1)
    {
      trainX <- trainX[, -highCorr, drop = FALSE]
      testX  <-  testX[, -highCorr, drop = FALSE]
    }
}  

## also, if needed, centering and scaling
if(suggestions(model)["center"] | suggestions(model)["scale"])
{
  ppMethod <- NULL
  if(suggestions(model)["center"]) ppMethod <- c(ppMethod, "center")
  if(suggestions(model)["scale"]) ppMethod <- c(ppMethod, "scale")
  
  ## check for zero variance just in case
  if(!suggestions(model)["corr"])
    {
      zeroVar <- unlist(lapply(trainX, function(u) length(unique(u)) > 1))
      if(length(zeroVar) > 0)
        {
          trainX <- trainX[, zeroVar, drop = FALSE]
          testX  <- testX[, zeroVar, drop = FALSE]
        }
    }
  preProcValues <- preProcess(trainX, ppMethod)
  trainX <- predict(preProcValues, trainX) 
  testX  <- predict(preProcValues, testX)   
}  

########################################################
## Create the Model

## setup some variables
isClass <- is.factor(trainY)
classProbs <- any(modelLookup(model)$probModel)

## setup reampling
## TODO mpi
ctlObj <- trainControl(method = resampleType,
                       number = reampleN) 

## setup the tuning grid

if(model == "gbm")
  {
    tGrid <- expand.grid(
                         .interaction.depth = -1 + (1:5)*2, 
                         .n.trees = (1:20)*10, 
                         .shrinkage = .1)
  } else {
    tmp <- trainX
    tmp$.outcome <- trainY
    tGrid <- createGrid(model, gridSize, tmp)
  }

modelFit <- switch(model,
                   gbm = 
                   {
                     train(
                           trainX, trainY, model,
                           verbose = FALSE,
                           bag.fraction = .9, 
                           trControl = ctlObj,
                           tuneGrid = tGrid)
                   },
                   multinom =
                   {
                     train(
                           trainX, trainY, model, 
                           trace = FALSE,
                           maxiter = 1000,
                           MaxNWts = 5000,
                           trControl = ctlObj,
                           tuneGrid = tGrid)   
                   },
                   nnet =
                   {
                     train(
                           trainX, trainY,
                           model, 
                           linout = !isClass,
                           trace = FALSE,
                           maxiter = 1000,
                           MaxNWts = 5000,
                           trControl = ctlObj,
                           tuneGrid = tGrid)  
                     
                   }, 
                   train(trainX, trainY, model, trControl = ctlObj, tuneGrid = tGrid))
  
########################################################
## Test Set Prediction

testPred <- extractPrediction(list(modelFit),
                              testX = testX,
                              testY = testY)

testPred <- testPred[testPred$dataType == "Test",]

if(classProbs & isClass)
{
  testProbs <- extractProb(list(modelFit),
                           testX = testX,
                           testY = testY)
  testProbs <- testProbs[testProbs$dataType == "Test",]
}

########################################################
## Model Evaluation

print(modelFit)
cat("Test Set Results:\n")
print(postResample(testPred$pred, testPred$obs))
if(isClass) print(confusionMatrix(testPred$pred, testPred$obs))

########################################################
## Model Visualizations

trellis.par.set(caretTheme(), warn = FALSE)

if(any(modelLookup(model)$parameter != "parameter"))
{
  plot(modelFit)
  if(isClass)
    {
      ##TODO change this:
      plot(modelFit, metric = "Kappa")
    } else plot(modelFit, metric = "Rsquared")
}

##TODO change this
if(resampleType != "LOOCV") resampleHist(modelFit, adjust = 1.25)

if(!isClass)
{
  testPred$resid <- testPred$obs - testPred$pred
  diagPlot1 <- xyplot(resid ~ pred, data = testPred,
                      panel = function(x, y, ...)
                      {
                        panel.abline(h = 0)
                        panel.xyplot(x, y, ...)
                      },
                      xlab = "Predicted", ylab = "Residual")
  diagPlot2 <- qqmath(~ resid,  data = testPred,
                      prepanel = prepanel.qqmathline,
                      panel = function(x, ...) 
                      {
                        panel.qqmathline(x, ...)
                        panel.qqmath(x, ...)
                      },
                      xlab = "Normal Quantile",
                      ylab = "Residual")
  diagPlot3 <- histogram(~ resid,  data = testPred,
                         type = "density",
                         xlab = "Residual",
                         panel = function(x, ...) 
                         {
                           panel.histogram(x, ...)
                           panel.xyplot(x,
                                        rep(0, length(x)),
                                        pch = "|",
                                        cex = .8)
                         })
  diagPlot4 <- xyplot(pred ~ obs, data = testPred,
                      panel = function(x, y, ...)
                      {
                        panel.abline(0,1, lty = 2)         
                        panel.xyplot(x, y, ...)
                        panel.loess(x, y,
                                    span = 0.75,
                                    col = "darkred", ...)            
                      },
                      ylab = "Predicted",
                      xlab = "Observed") 
  
  print(diagPlot1, split=c(1,1,2,2), more=TRUE)
  print(diagPlot2, split=c(2,1,2,2), more=TRUE)
  print(diagPlot3, split=c(1,2,2,2), more=TRUE)
  print(diagPlot4, split=c(2,2,2,2))     
} else {
  if(classProbs) plotClassProbs(testProbs)
}
