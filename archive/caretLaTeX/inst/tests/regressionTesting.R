tmp <- modelLookup()
tmp <- subset(tmp, forReg)
unique(tmp$model)

set.seed(1)
regData <- data.frame(matrix(rnorm(200), ncol = 4))
names(regData)[4] <- "y"

logicData <- data.frame(matrix(sample(c(0, 1), 200, replace = TRUE), ncol = 4))
logicData$y <- rnorm(nrow(logicData))

#####################################################################
## Regression Tests:

bootControl <- trainControl(number = 10)
boot632Control <- trainControl(method = "boot632", number = 10)
kcvControl <- trainControl(method = "cv", number = 3)
mkcvControl <- trainControl(method = "repeatedcv", number = 3, repeats = 2)
lgoControl <- trainControl(method = "LGOCV", number = 3, p = .9)
looControl <- trainControl(method = "LOOCV")


###

set.seed(1)
bagEarthBT <- train(y~., data = regData, "bagEarth",
                    tuneLength = 2, B = 10,
                    trControl = boot632Control)
bagEarthBT632 <- train(y~., data = regData, "bagEarth",
                       tuneLength = 2, B = 10,
                       trControl = boot632Control)
bagEarthKCV <- train(y~., data = regData, "bagEarth",
                     tuneLength = 2, B = 10,
                     trControl = kcvControl)
bagEarthMKCV <- train(y~., data = regData, "bagEarth",
                      tuneLength = 2, B = 10,
                      trControl = mkcvControl)
bagEarthLGO <- train(y~., data = regData, "bagEarth",
                     tuneLength = 2, B = 10,
                     trControl = lgoControl)
bagEarthLOO <- train(y~., data = regData, "bagEarth",
                     tuneLength = 2, B = 10,
                     trControl = looControl)

###

set.seed(1)
blackboostBT <- train(y~., data = regData, "blackboost",
                      tuneLength = 2,
                      trControl = boot632Control)
blackboostBT632 <- train(y~., data = regData, "blackboost",
                         tuneLength = 2,
                         trControl = boot632Control)
blackboostKCV <- train(y~., data = regData, "blackboost",
                       tuneLength = 2, 
                       trControl = kcvControl)
blackboostMKCV <- train(y~., data = regData, "blackboost",
                        tuneLength = 2, 
                        trControl = mkcvControl)
blackboostLGO <- train(y~., data = regData, "blackboost",
                       tuneLength = 2,
                       trControl = lgoControl)
blackboostLOO <- train(y~., data = regData, "blackboost",
                       tuneLength = 2,
                       trControl = looControl)

###

set.seed(1)
cforestBT <- train(y~., data = regData, "cforest",
                   tuneLength = 2,
                   trControl = boot632Control)
cforestBT632 <- train(y~., data = regData, "cforest",
                      tuneLength = 2,
                      trControl = boot632Control)
cforestKCV <- train(y~., data = regData, "cforest",
                    tuneLength = 2, 
                    trControl = kcvControl)
cforestMKCV <- train(y~., data = regData, "cforest",
                     tuneLength = 2, 
                     trControl = mkcvControl)
cforestLGO <- train(y~., data = regData, "cforest",
                    tuneLength = 2,
                    trControl = lgoControl)
cforestLOO <- train(y~., data = regData, "cforest",
                    tuneLength = 2,
                    trControl = looControl)


###

set.seed(1)
ctreeBT <- train(y~., data = regData, "ctree",
                 tuneLength = 2,
                 trControl = boot632Control)
ctreeBT632 <- train(y~., data = regData, "ctree",
                    tuneLength = 2,
                    trControl = boot632Control)
ctreeKCV <- train(y~., data = regData, "ctree",
                  tuneLength = 2, 
                  trControl = kcvControl)
ctreeMKCV <- train(y~., data = regData, "ctree",
                   tuneLength = 2, 
                   trControl = mkcvControl)
ctreeLGO <- train(y~., data = regData, "ctree",
                  tuneLength = 2,
                  trControl = lgoControl)
ctreeLOO <- train(y~., data = regData, "ctree",
                  tuneLength = 2,
                  trControl = looControl)

###

set.seed(1)
ctree2BT <- train(y~., data = regData, "ctree2",
                  tuneLength = 2,
                  trControl = boot632Control)
ctree2BT632 <- train(y~., data = regData, "ctree2",
                     tuneLength = 2,
                     trControl = boot632Control)
ctree2KCV <- train(y~., data = regData, "ctree2",
                   tuneLength = 2, 
                   trControl = kcvControl)
ctree2MKCV <- train(y~., data = regData, "ctree2",
                    tuneLength = 2, 
                    trControl = mkcvControl)
ctree2LGO <- train(y~., data = regData, "ctree2",
                   tuneLength = 2,
                   trControl = lgoControl)
ctree2LOO <- train(y~., data = regData, "ctree2",
                   tuneLength = 2,
                   trControl = looControl)


###

set.seed(1)
earthBT <- train(y~., data = regData, "earth",
                 tuneLength = 2,
                 trControl = boot632Control)
earthBT632 <- train(y~., data = regData, "earth",
                    tuneLength = 2,
                    trControl = boot632Control)
earthKCV <- train(y~., data = regData, "earth",
                  tuneLength = 2, 
                  trControl = kcvControl)
earthMKCV <- train(y~., data = regData, "earth",
                   tuneLength = 2, 
                   trControl = mkcvControl)
earthLGO <- train(y~., data = regData, "earth",
                  tuneLength = 2,
                  trControl = lgoControl)
earthLOO <- train(y~., data = regData, "earth",
                  tuneLength = 2,
                  trControl = looControl)


###

set.seed(1)
enetBT <- train(y~., data = regData, "enet",
                tuneLength = 2,
                trControl = boot632Control)
enetBT632 <- train(y~., data = regData, "enet",
                   tuneLength = 2,
                   trControl = boot632Control)
enetKCV <- train(y~., data = regData, "enet",
                 tuneLength = 2, 
                 trControl = kcvControl)
enetMKCV <- train(y~., data = regData, "enet",
                  tuneLength = 2, 
                  trControl = mkcvControl)
enetLGO <- train(y~., data = regData, "enet",
                 tuneLength = 2,
                 trControl = lgoControl)
enetLOO <- train(y~., data = regData, "enet",
                 tuneLength = 2,
                 trControl = looControl)

## TODO enetLOO failed


###

set.seed(1)
fobaBT <- train(y~., data = regData, "foba",
                tuneLength = 2,
                trControl = boot632Control)
fobaBT632 <- train(y~., data = regData, "foba",
                   tuneLength = 2,
                   trControl = boot632Control)
fobaKCV <- train(y~., data = regData, "foba",
                 tuneLength = 2, 
                 trControl = kcvControl)
fobaMKCV <- train(y~., data = regData, "foba",
                  tuneLength = 2, 
                  trControl = mkcvControl)
fobaLGO <- train(y~., data = regData, "foba",
                 tuneLength = 2,
                 trControl = lgoControl)
fobaLOO <- train(y~., data = regData, "foba",
                 tuneLength = 2,
                 trControl = looControl)


###

set.seed(1)
gamBT <- train(y~., data = regData, "gam",
               tuneLength = 2,
               trControl = boot632Control)
gamBT632 <- train(y~., data = regData, "gam",
                  tuneLength = 2,
                  trControl = boot632Control)
gamKCV <- train(y~., data = regData, "gam",
                tuneLength = 2, 
                trControl = kcvControl)
gamMKCV <- train(y~., data = regData, "gam",
                 tuneLength = 2, 
                 trControl = mkcvControl)
gamLGO <- train(y~., data = regData, "gam",
                tuneLength = 2,
                trControl = lgoControl)
gamLOO <- train(y~., data = regData, "gam",
                tuneLength = 2,
                trControl = looControl)


###

set.seed(1)
gamLoessBT <- train(y~., data = regData, "gamLoess",
                    tuneLength = 2,
                    trControl = boot632Control)
gamLoessBT632 <- train(y~., data = regData, "gamLoess",
                       tuneLength = 2,
                       trControl = boot632Control)
gamLoessKCV <- train(y~., data = regData, "gamLoess",
                     tuneLength = 2, 
                     trControl = kcvControl)
gamLoessMKCV <- train(y~., data = regData, "gamLoess",
                      tuneLength = 2, 
                      trControl = mkcvControl)
gamLoessLGO <- train(y~., data = regData, "gamLoess",
                     tuneLength = 2,
                     trControl = lgoControl)
gamLoessLOO <- train(y~., data = regData, "gamLoess",
                     tuneLength = 2,
                     trControl = looControl)


###

set.seed(1)
gamSplineBT <- train(y~., data = regData, "gamSpline",
                     tuneLength = 2,
                     trControl = boot632Control)
gamSplineBT632 <- train(y~., data = regData, "gamSpline",
                        tuneLength = 2,
                        trControl = boot632Control)
gamSplineKCV <- train(y~., data = regData, "gamSpline",
                      tuneLength = 2, 
                      trControl = kcvControl)
gamSplineMKCV <- train(y~., data = regData, "gamSpline",
                       tuneLength = 2, 
                       trControl = mkcvControl)
gamSplineLGO <- train(y~., data = regData, "gamSpline",
                      tuneLength = 2,
                      trControl = lgoControl)
gamSplineLOO <- train(y~., data = regData, "gamSpline",
                      tuneLength = 2,
                      trControl = looControl)


###

set.seed(1)
gamboostBT <- train(y~., data = regData, "gamboost",
                    tuneLength = 2,
                    trControl = boot632Control)
gamboostBT632 <- train(y~., data = regData, "gamboost",
                       tuneLength = 2,
                       trControl = boot632Control)
gamboostKCV <- train(y~., data = regData, "gamboost",
                     tuneLength = 2, 
                     trControl = kcvControl)
gamboostMKCV <- train(y~., data = regData, "gamboost",
                      tuneLength = 2, 
                      trControl = mkcvControl)
gamboostLGO <- train(y~., data = regData, "gamboost",
                     tuneLength = 2,
                     trControl = lgoControl)
gamboostLOO <- train(y~., data = regData, "gamboost",
                     tuneLength = 2,
                     trControl = looControl)

###

set.seed(1)
gaussprLinearBT <- train(y~., data = regData, "gaussprLinear",
                         tuneLength = 2,
                         trControl = boot632Control)
gaussprLinearBT632 <- train(y~., data = regData, "gaussprLinear",
                            tuneLength = 2,
                            trControl = boot632Control)
gaussprLinearKCV <- train(y~., data = regData, "gaussprLinear",
                          tuneLength = 2, 
                          trControl = kcvControl)
gaussprLinearMKCV <- train(y~., data = regData, "gaussprLinear",
                           tuneLength = 2, 
                           trControl = mkcvControl)
gaussprLinearLGO <- train(y~., data = regData, "gaussprLinear",
                          tuneLength = 2,
                          trControl = lgoControl)
gaussprLinearLOO <- train(y~., data = regData, "gaussprLinear",
                          tuneLength = 2,
                          trControl = looControl)

###

set.seed(1)
gaussprPolyBT <- train(y~., data = regData, "gaussprPoly",
                       tuneLength = 2,
                       trControl = boot632Control)
gaussprPolyBT632 <- train(y~., data = regData, "gaussprPoly",
                          tuneLength = 2,
                          trControl = boot632Control)
gaussprPolyKCV <- train(y~., data = regData, "gaussprPoly",
                        tuneLength = 2, 
                        trControl = kcvControl)
gaussprPolyMKCV <- train(y~., data = regData, "gaussprPoly",
                         tuneLength = 2, 
                         trControl = mkcvControl)
gaussprPolyLGO <- train(y~., data = regData, "gaussprPoly",
                        tuneLength = 2,
                        trControl = lgoControl)
gaussprPolyLOO <- train(y~., data = regData, "gaussprPoly",
                        tuneLength = 2,
                        trControl = looControl)


###

set.seed(1)
gaussprRadialBT <- train(y~., data = regData, "gaussprRadial",
                         tuneLength = 2,
                         trControl = boot632Control)
gaussprRadialBT632 <- train(y~., data = regData, "gaussprRadial",
                            tuneLength = 2,
                            trControl = boot632Control)
gaussprRadialKCV <- train(y~., data = regData, "gaussprRadial",
                          tuneLength = 2, 
                          trControl = kcvControl)
gaussprRadialMKCV <- train(y~., data = regData, "gaussprRadial",
                           tuneLength = 2, 
                           trControl = mkcvControl)
gaussprRadialLGO <- train(y~., data = regData, "gaussprRadial",
                          tuneLength = 2,
                          trControl = lgoControl)
gaussprRadialLOO <- train(y~., data = regData, "gaussprRadial",
                          tuneLength = 2,
                          trControl = looControl)


###

set.seed(1)
gbmBT <- train(y~., data = regData, "gbm",
               tuneLength = 2,
               verbose= FALSE,
               bag.fraction = .8,

               trControl = boot632Control)
gbmBT632 <- train(y~., data = regData, "gbm",
                  tuneLength = 2,
                  verbose= FALSE,
               bag.fraction = .8,
                  trControl = boot632Control)
gbmKCV <- train(y~., data = regData, "gbm",
                tuneLength = 2,
                verbose= FALSE,
               bag.fraction = .8,
                trControl = kcvControl)
gbmMKCV <- train(y~., data = regData, "gbm",
                 tuneLength = 2,
                 verbose= FALSE,
               bag.fraction = .8,
                 trControl = mkcvControl)
gbmLGO <- train(y~., data = regData, "gbm",
                tuneLength = 2,
                verbose= FALSE,
               bag.fraction = .8,
                trControl = lgoControl)
gbmLOO <- train(y~., data = regData, "gbm",
                tuneLength = 2,
                verbose= FALSE,
               bag.fraction = .8,
                trControl = looControl)

###

set.seed(1)
glmBT <- train(y~., data = regData, "glm",
                    tuneLength = 2,
                    trControl = boot632Control)
glmBT632 <- train(y~., data = regData, "glm",
                    tuneLength = 2,
                    trControl = boot632Control)
glmKCV <- train(y~., data = regData, "glm",
                    tuneLength = 2, 
                    trControl = kcvControl)
glmMKCV <- train(y~., data = regData, "glm",
                    tuneLength = 2, 
                    trControl = mkcvControl)
glmLGO <- train(y~., data = regData, "glm",
                    tuneLength = 2,
                    trControl = lgoControl)
glmLOO <- train(y~., data = regData, "glm",
                    tuneLength = 2,
                    trControl = looControl)


###

set.seed(1)
glmboostBT <- train(y~., data = regData, "glmboost",
                    tuneLength = 2,
                    trControl = boot632Control)
glmboostBT632 <- train(y~., data = regData, "glmboost",
                    tuneLength = 2,
                    trControl = boot632Control)
glmboostKCV <- train(y~., data = regData, "glmboost",
                    tuneLength = 2, 
                    trControl = kcvControl)
glmboostMKCV <- train(y~., data = regData, "glmboost",
                    tuneLength = 2, 
                    trControl = mkcvControl)
glmboostLGO <- train(y~., data = regData, "glmboost",
                    tuneLength = 2,
                    trControl = lgoControl)
glmboostLOO <- train(y~., data = regData, "glmboost",
                    tuneLength = 2,
                    trControl = looControl)



###

set.seed(1)
glmnetBT <- train(y~., data = regData, "glmnet",
                    tuneLength = 2,
                    trControl = boot632Control)
glmnetBT632 <- train(y~., data = regData, "glmnet",
                    tuneLength = 2,
                    trControl = boot632Control)
glmnetKCV <- train(y~., data = regData, "glmnet",
                    tuneLength = 2, 
                    trControl = kcvControl)
glmnetMKCV <- train(y~., data = regData, "glmnet",
                    tuneLength = 2, 
                    trControl = mkcvControl)
glmnetLGO <- train(y~., data = regData, "glmnet",
                    tuneLength = 2,
                    trControl = lgoControl)
glmnetLOO <- train(y~., data = regData, "glmnet",
                    tuneLength = 2,
                    trControl = looControl)

###

set.seed(1)
glmStepAICBT <- train(y~., data = regData, "glmStepAIC",
                    tuneLength = 2,
                      trace = -1,
                    trControl = boot632Control)
glmStepAICBT632 <- train(y~., data = regData, "glmStepAIC",
                    tuneLength = 2,
                      trace = 0,
                    trControl = boot632Control)
glmStepAICKCV <- train(y~., data = regData, "glmStepAIC",
                    tuneLength = 2,
                      trace = 0, 
                    trControl = kcvControl)
glmStepAICMKCV <- train(y~., data = regData, "glmStepAIC",
                    tuneLength = 2,
                      trace = 0, 
                    trControl = mkcvControl)
glmStepAICLGO <- train(y~., data = regData, "glmStepAIC",
                    tuneLength = 2,
                      trace = 0,
                    trControl = lgoControl)
glmStepAICLOO <- train(y~., data = regData, "glmStepAIC",
                    tuneLength = 2,
                      trace = 0,
                    trControl = looControl)

###

set.seed(1)
icrBT <- train(y~., data = regData, "icr",
                    tuneLength = 2,
                    trControl = boot632Control)
icrBT632 <- train(y~., data = regData, "icr",
                    tuneLength = 2,
                    trControl = boot632Control)
icrKCV <- train(y~., data = regData, "icr",
                    tuneLength = 2, 
                    trControl = kcvControl)
icrMKCV <- train(y~., data = regData, "icr",
                    tuneLength = 2, 
                    trControl = mkcvControl)
icrLGO <- train(y~., data = regData, "icr",
                    tuneLength = 2,
                    trControl = lgoControl)
icrLOO <- train(y~., data = regData, "icr",
                    tuneLength = 2,
                    trControl = looControl)


###

set.seed(1)
knnBT <- train(y~., data = regData, "knn",
                    tuneLength = 2,
                    trControl = boot632Control)
knnBT632 <- train(y~., data = regData, "knn",
                    tuneLength = 2,
                    trControl = boot632Control)
knnKCV <- train(y~., data = regData, "knn",
                    tuneLength = 2, 
                    trControl = kcvControl)
knnMKCV <- train(y~., data = regData, "knn",
                    tuneLength = 2, 
                    trControl = mkcvControl)
knnLGO <- train(y~., data = regData, "knn",
                    tuneLength = 2,
                    trControl = lgoControl)
knnLOO <- train(y~., data = regData, "knn",
                    tuneLength = 2,
                    trControl = looControl)


###

set.seed(1)
larsBT <- train(y~., data = regData, "lars",
                    tuneLength = 2,
                    trControl = boot632Control)
larsBT632 <- train(y~., data = regData, "lars",
                    tuneLength = 2,
                    trControl = boot632Control)
larsKCV <- train(y~., data = regData, "lars",
                    tuneLength = 2, 
                    trControl = kcvControl)
larsMKCV <- train(y~., data = regData, "lars",
                    tuneLength = 2, 
                    trControl = mkcvControl)
larsLGO <- train(y~., data = regData, "lars",
                    tuneLength = 2,
                    trControl = lgoControl)
larsLOO <- train(y~., data = regData, "lars",
                    tuneLength = 2,
                    trControl = looControl)

###

set.seed(1)
lars2BT <- train(y~., data = regData, "lars2",
                    tuneLength = 2,
                    trControl = boot632Control)
lars2BT632 <- train(y~., data = regData, "lars2",
                    tuneLength = 2,
                    trControl = boot632Control)
lars2KCV <- train(y~., data = regData, "lars2",
                    tuneLength = 2, 
                    trControl = kcvControl)
lars2MKCV <- train(y~., data = regData, "lars2",
                    tuneLength = 2, 
                    trControl = mkcvControl)
lars2LGO <- train(y~., data = regData, "lars2",
                    tuneLength = 2,
                    trControl = lgoControl)
lars2LOO <- train(y~., data = regData, "lars2",
                    tuneLength = 2,
                    trControl = looControl)

###

set.seed(1)
lassoBT <- train(y~., data = regData, "lasso",
                    tuneLength = 2,
                    trControl = boot632Control)
lassoBT632 <- train(y~., data = regData, "lasso",
                    tuneLength = 2,
                    trControl = boot632Control)
lassoKCV <- train(y~., data = regData, "lasso",
                    tuneLength = 2, 
                    trControl = kcvControl)
lassoMKCV <- train(y~., data = regData, "lasso",
                    tuneLength = 2, 
                    trControl = mkcvControl)
lassoLGO <- train(y~., data = regData, "lasso",
                    tuneLength = 2,
                    trControl = lgoControl)
lassoLOO <- train(y~., data = regData, "lasso",
                    tuneLength = 2,
                    trControl = looControl)
## TODO lassoLOO failed


###

set.seed(1)
lmBT <- train(y~., data = regData, "lm",
                    tuneLength = 2,
                    trControl = boot632Control)
lmBT632 <- train(y~., data = regData, "lm",
                    tuneLength = 2,
                    trControl = boot632Control)
lmKCV <- train(y~., data = regData, "lm",
                    tuneLength = 2, 
                    trControl = kcvControl)
lmMKCV <- train(y~., data = regData, "lm",
                    tuneLength = 2, 
                    trControl = mkcvControl)
lmLGO <- train(y~., data = regData, "lm",
                    tuneLength = 2,
                    trControl = lgoControl)
lmLOO <- train(y~., data = regData, "lm",
                    tuneLength = 2,
                    trControl = looControl)



###

set.seed(1)
lmStepAICBT <- train(y~., data = regData, "lmStepAIC",
                    tuneLength = 2,
                    trControl = boot632Control)
lmStepAICBT632 <- train(y~., data = regData, "lmStepAIC",
                    tuneLength = 2,
                    trControl = boot632Control)
lmStepAICKCV <- train(y~., data = regData, "lmStepAIC",
                    tuneLength = 2, 
                    trControl = kcvControl)
lmStepAICMKCV <- train(y~., data = regData, "lmStepAIC",
                    tuneLength = 2, 
                    trControl = mkcvControl)
lmStepAICLGO <- train(y~., data = regData, "lmStepAIC",
                    tuneLength = 2,
                    trControl = lgoControl)
lmStepAICLOO <- train(y~., data = regData, "lmStepAIC",
                    tuneLength = 2,
                    trControl = looControl)

if(FALSE)
  {
    ## these all failed:
    
###

set.seed(1)
logicBagBT <- train(y~., data = logicData, "logicBag",
                    tuneLength = 2,
                    trControl = boot632Control)
logicBagBT632 <- train(y~., data = logicData, "logicBag",
                    tuneLength = 2,
                    trControl = boot632Control)
logicBagKCV <- train(y~., data = logicData, "logicBag",
                    tuneLength = 2, 
                    trControl = kcvControl)
logicBagMKCV <- train(y~., data = logicData, "logicBag",
                    tuneLength = 2, 
                    trControl = mkcvControl)
logicBagLGO <- train(y~., data = logicData, "logicBag",
                    tuneLength = 2,
                    trControl = lgoControl)
logicBagLOO <- train(y~., data = logicData, "logicBag",
                    tuneLength = 2,
                    trControl = looControl)

Error in dimnames(x) <- dn : 
  length of 'dimnames' [2] not equal to array extent
> traceback()
17: `colnames<-`(`*tmp*`, value = "X")
16: generateTruthTab(ltree)
15: FUN(X[[2L]], ...)
14: lapply(lmodel[[i]]$trees, getPImps, type = log.out$type)
13: logic.pimp(log.out)
12: vim.logicFS(log.out, useN = useN, onlyRemove = onlyRemove, prob.case = prob.case, 
        addMatImp = addMatImp)
11: logic.bagging.default(as.matrix(trainX), trainY, ntrees = tuneValue$.ntrees, 
        nleaves = tuneValue$.nleaves, ...)
10: logic.bagging(as.matrix(trainX), trainY, ntrees = tuneValue$.ntrees, 
        nleaves = tuneValue$.nleaves, ...)

}


###

set.seed(1)
logregBT <- train(y~., data = logicData, "logreg",
                    tuneLength = 2,
                    trControl = boot632Control)
logregBT632 <- train(y~., data = logicData, "logreg",
                    tuneLength = 2,
                    trControl = boot632Control)
logregKCV <- train(y~., data = logicData, "logreg",
                    tuneLength = 2, 
                    trControl = kcvControl)
logregMKCV <- train(y~., data = logicData, "logreg",
                    tuneLength = 2, 
                    trControl = mkcvControl)
logregLGO <- train(y~., data = logicData, "logreg",
                    tuneLength = 2,
                    trControl = lgoControl)
logregLOO <- train(y~., data = logicData, "logreg",
                    tuneLength = 2,
                    trControl = looControl)


###

set.seed(1)
M5RulesBT <- train(y~., data = regData, "M5Rules",
                    tuneLength = 2,
                    trControl = boot632Control)
M5RulesBT632 <- train(y~., data = regData, "M5Rules",
                    tuneLength = 2,
                    trControl = boot632Control)
M5RulesKCV <- train(y~., data = regData, "M5Rules",
                    tuneLength = 2, 
                    trControl = kcvControl)
M5RulesMKCV <- train(y~., data = regData, "M5Rules",
                    tuneLength = 2, 
                    trControl = mkcvControl)
M5RulesLGO <- train(y~., data = regData, "M5Rules",
                    tuneLength = 2,
                    trControl = lgoControl)
M5RulesLOO <- train(y~., data = regData, "M5Rules",
                    tuneLength = 2,
                    trControl = looControl)



###

set.seed(1)
neuralnetBT <- train(y~., data = regData, "neuralnet",
                    tuneLength = 2,
                    trControl = boot632Control)
neuralnetBT632 <- train(y~., data = regData, "neuralnet",
                    tuneLength = 2,
                    trControl = boot632Control)
neuralnetKCV <- train(y~., data = regData, "neuralnet",
                    tuneLength = 2, 
                    trControl = kcvControl)
neuralnetMKCV <- train(y~., data = regData, "neuralnet",
                    tuneLength = 2, 
                    trControl = mkcvControl)
neuralnetLGO <- train(y~., data = regData, "neuralnet",
                    tuneLength = 2,
                    trControl = lgoControl)
neuralnetLOO <- train(y~., data = regData, "neuralnet",
                    tuneLength = 2,
                    trControl = looControl)


###

set.seed(1)
nnetBT <- train(y~., data = regData, "nnet",
                    tuneLength = 2,
                trace = FALSE, linout = TRUE,
                    trControl = boot632Control)
nnetBT632 <- train(y~., data = regData, "nnet",
                    tuneLength = 2,
                trace = FALSE, linout = TRUE,
                    trControl = boot632Control)
nnetKCV <- train(y~., data = regData, "nnet",
                    tuneLength = 2,
                trace = FALSE, linout = TRUE, 
                    trControl = kcvControl)
nnetMKCV <- train(y~., data = regData, "nnet",
                    tuneLength = 2,
                trace = FALSE, linout = TRUE, 
                    trControl = mkcvControl)
nnetLGO <- train(y~., data = regData, "nnet",
                    tuneLength = 2,
                trace = FALSE, linout = TRUE,
                    trControl = lgoControl)
nnetLOO <- train(y~., data = regData, "nnet",
                    tuneLength = 2,
                trace = FALSE, linout = TRUE,
                    trControl = looControl)


###

set.seed(1)
nodeHarvestBT <- train(y~., data = regData, "nodeHarvest",
                    tuneLength = 2,
                       silent = TRUE,
                    trControl = boot632Control)
nodeHarvestBT632 <- train(y~., data = regData, "nodeHarvest",
                    tuneLength = 2,
                       silent = TRUE,
                    trControl = boot632Control)
nodeHarvestKCV <- train(y~., data = regData, "nodeHarvest",
                    tuneLength = 2,
                       silent = TRUE, 
                    trControl = kcvControl)
nodeHarvestMKCV <- train(y~., data = regData, "nodeHarvest",
                    tuneLength = 2,
                       silent = TRUE, 
                    trControl = mkcvControl)
nodeHarvestLGO <- train(y~., data = regData, "nodeHarvest",
                    tuneLength = 2,
                       silent = TRUE,
                    trControl = lgoControl)
nodeHarvestLOO <- train(y~., data = regData, "nodeHarvest",
                    tuneLength = 2,
                       silent = TRUE,
                    trControl = looControl)


###

set.seed(1)
partDSABT <- train(y~., data = regData, "partDSA",
                    tuneLength = 2,
                    trControl = boot632Control)
partDSABT632 <- train(y~., data = regData, "partDSA",
                    tuneLength = 2,
                    trControl = boot632Control)
partDSAKCV <- train(y~., data = regData, "partDSA",
                    tuneLength = 2, 
                    trControl = kcvControl)
partDSAMKCV <- train(y~., data = regData, "partDSA",
                    tuneLength = 2, 
                    trControl = mkcvControl)
partDSALGO <- train(y~., data = regData, "partDSA",
                    tuneLength = 2,
                    trControl = lgoControl)
partDSALOO <- train(y~., data = regData, "partDSA",
                    tuneLength = 2,
                    trControl = looControl)

