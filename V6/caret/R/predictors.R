"predictors" <- function(x, ...){
    UseMethod("predictors")
  }

predictors.train <- function(x, ...)
  {
    out <- predictors(x$finalModel)
    if(all(is.na(out))) out <- x$xNames
    out
  }

hasTerms <- function(x)
  {
    objNames <- c(names(x), slotNames(x))
    "terms" %in% tolower(objNames)
  }

## basicVars tries to detect the actual variable that are used
## when a formula might include other terms (such as interactions)
## For example:
## > x
## [1] "medv" "crim" "zn"   "age" 
## > y
## [1] "crim"     "I(age^2)" "zn"   
## > basicVars(x, y)
## [1] "crim" "zn"   "age"

basicVars <- function(x, y)
  {
    hasVar <- rep(NA, length(x))
    for(i in seq(along = x))
      hasVar[i] <- length(grep(x[i], y, fixed = TRUE)) > 0
    x[hasVar] 
  }

predictors.terms <- function(x, ...)
  {
    if(is.null(x)) return(NA)
    everything <- all.vars(x)
    yName <- as.character(x[[2]])
    everything[!(everything %in% yName)]
  }

predictors.formula <- function(x, ...)
  {
    everything <- all.vars(x)
    yName <- as.character(x[[2]])
    everything[!(everything %in% yName)]
  }

predictors.list <- function(x, ...)
  {
    out <- lapply(x, predictors)
    names(out) <- names(x)
    out
  }

predictors.mvr <- function(x, ...) {
   code <- getModelInfo("pls", regex = FALSE)[[1]]
   checkInstall(code$library)
   for(i in seq(along = code$library)) 
     do.call("require", list(package = code$library[i]))
   code$predictors(x, ...)
  }

predictors.gbm <- function(x, ...) {
  code <- getModelInfo("gbm", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)  
  }

predictors.Weka_classifier <- function(x, ...) {
  code <- getModelInfo("J48", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.fda <- function(x, ...) {
  code <- getModelInfo("fda", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.earth <- function(x, ...) {
  code <- getModelInfo("earth", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.gausspr <- function(x, ...) {
  code <- getModelInfo("gaussprRadial", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.ksvm <- function(x, ...) {
  code <- getModelInfo("svmRadial", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.lssvm <- function(x, ...) {
  code <- getModelInfo("lssvmRadial", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.rvm <- function(x, ...) {
  code <- getModelInfo("rvmRadial", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}


predictors.gpls <- function(x, ...) {
  code <- getModelInfo("gpls", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.knn3 <- function(x, ...) {
  code <- getModelInfo("knn", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
  }

predictors.knnreg <- function(x, ...){
  code <- getModelInfo("knn", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.LogitBoost <- function(x, ...) {
  code <- getModelInfo("LogitBoost", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
  }

predictors.lda <- function(x, ...) {
  code <- getModelInfo("lda", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.qda <- function(x, ...) {
  code <- getModelInfo("qda", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.rda <- function(x, ...) {
  code <- getModelInfo("rda", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.multinom <- function(x, ...) {
  code <- getModelInfo("multinom", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.nnet <- function(x, ...) {
  code <- getModelInfo("nnet", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}


predictors.pcaNNet <- function(x, ...){
  code <- getModelInfo("pcaNNet", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.avNNet <- function(x, ...){
  code <- getModelInfo("avNNet", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.NaiveBayes <- function(x, ...){
  code <- getModelInfo("nb", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.pamrtrained <- function(x, newdata = NULL, threshold = NULL,  ...){
  code <- getModelInfo("pam", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, newdata = newdata, threshold = threshold,  ...)
}

## todo finalize this
predictors.superpc <- function(x, newdata = NULL, threshold = NULL, n.components = NULL, ...)
  {
    NA
  }

predictors.randomForest <- function(x, ...){
  code <- getModelInfo("rf", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.slda <- function(x, ...){
  code <- getModelInfo("slda", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.rpart <- function(x, surrogate = TRUE, ...){
  code <- getModelInfo("rpart", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, surrogate = surrogate, ...)
}

predictors.regbagg <- function(x, surrogate = TRUE, ...){
  code <- getModelInfo("treebag", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, surrogate = surrogate, ...)
}

predictors.classbagg <- function(x, surrogate = TRUE, ...){
  code <- getModelInfo("treebag", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, surrogate = surrogate, ...)
}

predictors.lm <- function(x, ...){
  code <- getModelInfo("lm", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.glmboost <- function(x, ...) {
  code <- getModelInfo("glmboost", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.blackboost <- function(x, ...) {
  code <- getModelInfo("blackboost", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.gamboost <- function(x, ...) {
  code <- getModelInfo("gamboost", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.BinaryTree <- function(x, surrogate = TRUE, ...) {
  code <- getModelInfo("ctree", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, surrogate = surrogate, ...)
}


predictors.bagEarth <- function(x, ...) {
  code <- getModelInfo("bagEarth", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.bagFDA <- function(x, ...){
  code <- getModelInfo("bagFDA", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.ppr <- function(x, ...){
  code <- getModelInfo("ppr", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.spls <- function(x, ...){
  code <- getModelInfo("spls", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.splsda <- function(x, ...){
  code <- getModelInfo("spls", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.glm <- function(x, ...){
  code <- getModelInfo("glm", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.mda <- function(x, ...){
  code <- getModelInfo("mda", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}


predictors.glmnet <- function(x, lambda = NULL, ...){
  code <- getModelInfo("glmnet", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, lambda = lambda, ...)
}

predictors.penfit <- function(x, ...){
  code <- getModelInfo("penalized", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.lars <- function(x, s = NULL, ...){
  code <- getModelInfo("lars", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, s = s, ...)
}

predictors.enet <- function(x, s = NULL, ...){
  code <- getModelInfo("enet", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, s = s, ...)
}

predictors.sda <- function(x, ...){
  code <- getModelInfo("sda", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.smda <- function(x, ...){
  code <- getModelInfo("smda", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.stepclass <- function(x, ...){
  code <- getModelInfo("stepLDA", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}


predictors.trocc <- function(x, ...){
  code <- getModelInfo("rocc", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.foba <- function(x, k = NULL, ...){
  code <- getModelInfo("foba", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, k = k, ...)
}

predictors.dsa <- function(x, cuts = NULL, ...){
  code <- getModelInfo("partDSA", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, cuts = cuts, ...)
}

predictors.RandomForest <- function(x, ...){
  code <- getModelInfo("cforest", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}


predictors.logreg <- function(x, ...){
  code <- getModelInfo("logreg", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}


predictors.logicBagg <- function(x, ...){
  code <- getModelInfo("logicBagg", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}


predictors.gam <- function(x, ...){
  code <- getModelInfo("gam", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}


predictors.C5.0 <- function(x, ...){
  code <- getModelInfo("C5.0", regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library)) 
    do.call("require", list(package = code$library[i]))
  code$predictors(x, ...)
}

predictors.canldaRes <- function(x, ...) names(x$vkpt)

