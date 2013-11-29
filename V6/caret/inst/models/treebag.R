modelInfo <- list(library = "ipred",
                  loop = NULL,
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL) data.frame(.parameter = "none"),
                  fit = function(x, y, wts, param, lev, last,classProbs, ...) {
                    theDots <- list(...)
                    if(!any(names(theDots) == "keepX")) theDots$keepX <- FALSE   
                    modelArgs <- c(list(X = x, y = y), theDots)
                    do.call("ipredbagg", modelArgs)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type = "prob"),
                  predictors = function(x, surrogate = TRUE, ...) {
                    code <- getModelInfo("rpart", regex = FALSE)[[1]]$predictors
                    eachTree <- lapply(x$mtree,
                                       function(u, surr) code(u$btree, surrogate = surr),
                                       surr = surrogate)
                    unique(unlist(eachTree))
                  },
                  varImp = function(object, ...) {
                    allImp <- lapply(object$fit, varImp, ...)
                    impDF <- as.data.frame(allImp)
                    meanImp <- apply(impDF, 1, mean)
                    out <- data.frame(Overall = meanImp)
                    rownames(out) <- names(meanImp)
                    out
                  },
                  tags = c("Tree-Based Model", "Ensemble Model", "Bagging"), 
                  sort = function(x) x)
