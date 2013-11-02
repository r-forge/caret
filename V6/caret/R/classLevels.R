levels.train <- function(x) {
  if(x$modelType == "Classification")
  {
    if(!isS4(x$finalModel)) {
      ## add a try here and or look for null values
      out <- x$finalModel$obsLevels
    } else out <- levels(x$finalModel)
  } else out <- NULL
  out
}


levels.ksvm <- function(x) {
  library(kernlab)
  lev(x)
}
levels.lssvm <- function(x) {
  library(kernlab)
  lev(x)
}
levels.gausspr <- function(x) {
  library(kernlab)
  lev(x)
}

levels.LdaClassic <- function(x) names(x@prior)
levels.QdaClassic <- function(x) names(x@prior)

levels.BinaryTree <- function(x) levels(x@data@get("response")[,1])
levels.RandomForest <- function(x) levels(x@data@get("response")[,1])

levels.lda <- function(x) names(x$prior)

levels.rda <- function(x) names(x$prior)

levels.gbm <- function(x) if(any(names(x) == "obsLevels")) x$obsLevels else NULL

levels.randomForest <- function(x) x$classes

levels.nnet <- function(x) x$coefnames

levels.pcaNNet <- function(x) x$model$coefnames

levels.gpls <- function(x) x$model$levs

levels.rpart <- function(x) attr(x, "ylevels")

levels.plsda <- function(x) x$obsLevels

levels.pamrtrained <- function(x) names(x$prior)

levels.knn3 <- function(x) levels(x$learn$y)

levels.NaiveBayes <- function(x) x$levels

levels.earth <- function(x) x$levels

levels.fda <- function(x) names(x$prior)

levels.bagFDA <- function(x) x$levels

levels.classbag <- function(x) levels(x$y)

levels.glm <- function(x) if(any(names(x) == "obsLevels")) x$obsLevels else NULL

levels.glmboost <- function(x) levels(x$response)
levels.gamboost <- function(x) levels(x$response)
levels.blackboost <- function(x) levels(x$response)

levels.glmnet <- function(x) if(any(names(x) == "obsLevels")) x$obsLevels else NULL

levels.sdda <- function(x) rownames(x$means)

levels.LogitBoost <- function(x) x$lablist

levels.LogitBoost <- function(x) x$lablist

levels.Weka_classifier <- function(x) x$levels

levels.slda <- function(x) names(x$mylda$prior)

levels.sda <- function(x)
{
  ## objects from package sparseLDA and sda have the
  ## same class name
  if(any(names(x) == "regularization")) names(x$prior) else x$classes
}

levels.splsda <- function(x)
{
  ## objects from package caret and spls have the
  ## same class name
  
  isSpls <- !is.null(x$class.fit)
  if(isSpls) names(x$class.fit$prior) else x$obsLevels
}


levels.mda <- function(x) unique(unlist(lapply(strsplit(rownames(x$means), ".", fixed = TRUE), function(x) x[1])))

levels.VBMP.obj <- function(x) if(any(names(x) == "obsLevels")) x$obsLevels else NULL

levels.nodeHarvest <- function(x) unique(unlist(lapply(strsplit(rownames(x$means), ".", fixed = TRUE), function(x) x[1])))

levels.stepclass <- function(x) levels(x$fit)

levels.plr <- function(x) unique(unlist(lapply(strsplit(rownames(x$means), ".", fixed = TRUE), function(x) x[1])))

levels.CSimca <- function(x) names(x@prior)

levels.RSimca <- function(x) names(x@prior)

if(FALSE)
{
  lvlList <- sort(ls(pattern = "levels\\."))
  lvlList <- gsub("levels\\.", "", lvlList)
  lvlList <- paste("S3method(levels, ", lvlList, ")", sep = "")
  cat(paste(lvlList, collapse = "\n", sep = ""))
}








































