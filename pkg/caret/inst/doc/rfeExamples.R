library(caret)
data(BloodBrain)

x <- scale(bbbDescr[,-nearZeroVar(bbbDescr)])
x <- x[, -findCorrelation(cor(x), .8)]
x <- as.data.frame(x)

######################################################################
######################################################################

set.seed(1)
lmProfile <- rfe(x, logBBB,
                 sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
                 rfeControl = rfeControl(functions = lmFuncs(), 
                   number = 200))
set.seed(1)
lmProfile2 <- rfe(x, logBBB,
                  sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
                  rfeControl = rfeControl(functions = lmFuncs(), 
                    rerank = TRUE, 
                    number = 200))

######################################################################
######################################################################

rfFuncs2 <- function()
  {
    list(
         fit = function(x, y, ...)
         {
           library(randomForest)
           randomForest(x, y, ntree = 2000, importance = TRUE, ...)
         },
         pred = function(object, x)
         {
           predict(object, x)
         },
         rank = function(object, x, y)
         {
           vimp <- varImp(object)

           if(is.factor(y))
             {
               if(all(levels(y) %in% colnames(vimp)))
                 {
                   avImp <- apply(vimp[, levels(y), drop = TRUE],
                                  1,
                                  mean)
                   vimp$Overall <- avImp
                 }

             }
           
           vimp <- vimp[
                        order(
                              vimp$Overall,
                              decreasing = TRUE)
                        ,,
                        drop = FALSE]
           
           vimp$var <- rownames(vimp)                  
           vimp
         },
         selectSize = pickSizeTolerance,
         selectVar = pickVars)
  }


set.seed(1)
rfProfile <- rfe(x, logBBB,
                 sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
                 rfeControl = rfeControl(functions = rfFuncs2(), 
                   number = 200))
set.seed(1)
rfProfile2 <- rfe(x, logBBB,
                  sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
                  rfeControl = rfeControl(functions = rfFuncs2(), 
                    rerank = TRUE, 
                    number = 200))


lmProfile$results$Group  <- "OLS"
lmProfile2$results$Group <- "OLS, Re-ranked"
rfProfile$results$Group  <- "RandomForest"
rfProfile2$results$Group <- "RandomForest, Re-ranked"

all <- rbind(lmProfile$results,
             lmProfile2$results,
             rfProfile$results,
             rfProfile2$results)


xyplot(RMSE ~ Variables,
       data = all,
       groups = Group,
       lmProfile$results$Variables, 
       type = c("g", "p", "l"), 
       auto.key = list(columns = 2))


xyplot(Rsquared ~ Variables,
       data = all,
       groups = Group,
       lmProfile$results$Variables, 
       type = c("g", "p", "l"),
       ylab = expression(R^2),
       auto.key = list(columns = 2))


caret:::pickSizeBest(rfProfile$results, "RMSE", FALSE)
caret:::pickSizeTolerance(rfProfile$results,
                          metric = "RMSE",
                          maximize = FALSE)

tolerData <- rfProfile$results
tolerData$RMSE <- (min(tolerData$RMSE) -tolerData$RMSE)/
  min(tolerData$RMSE) * 100

tolerData$Rsquared <- (tolerData$Rsquared - max(tolerData$Rsquared))/
  max(tolerData$Rsquared) * 100

xyplot(RMSETol ~ Variables,
       data = rfProfile$results,
       lmProfile$results$Variables, 
       type = c("g", "p", "l"),
       main = paste("RandomForest (Single Ranking)\nTolerance =",
         expression(tolerance = 100 *(best - RMSE)/best)),
       ylab = "RMSE Tolerance"
       )

xyplot(RMSE + Rsquared ~ Variables,
       data = tolerData,
       type = c("g", "p", "l"),
       main ="RandomForest (Single Ranking)\nTolerances",
       ylab = "Tolerance",
       auto.key = list(columns = 2)
       )
