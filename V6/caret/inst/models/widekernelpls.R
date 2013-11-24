modelInfo <- list(library = "pls",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = 'ncomp',
                                          class = "numeric",
                                          label = '#Components'),
                  grid = function(x, y, len = NULL) 
                    data.frame(.ncomp = seq(1, min(ncol(x) - 1, len), by = 1)),
                  loop = function(grid) {     
                    grid <- grid[order(grid$.ncomp, decreasing = TRUE),, drop = FALSE]
                    loop <- grid[1,,drop = FALSE]
                    submodels <- list(grid[-1,,drop = FALSE])  
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {   
                    out <- if(is.factor(y))
                    {      
                      plsda(x, y, method = "oscorespls", ncomp = param$.ncomp, ...)
                    } else {
                      dat <- x
                      dat$.outcome <- y
                      plsr(.outcome ~ ., data = dat, method = "widekernelpls", ncomp = param$.ncomp, ...)
                    }
                    out
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {                    
                    out <- if(modelFit$problemType == "Classification")
                    {
                      if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                      out <- predict(modelFit, newdata, type="class")
                      
                    } else as.vector(pls:::predict.mvr(modelFit, newdata, ncomp = max(modelFit$ncomp)))
                    
                    if(!is.null(submodels))
                    {
                      ## We'll merge the first set of predictions below
                      tmp <- vector(mode = "list", length = nrow(submodels))
                      
                      if(modelFit$problemType == "Classification")
                      {
                        if(length(submodels$.ncomp) > 1)
                        {
                          tmp <- as.list(predict(modelFit, newdata, ncomp = submodels$.ncomp))
                        } else tmp <- list(predict(modelFit, newdata, ncomp = submodels$.ncomp))
                        
                      } else {
                        tmp <- as.list(
                          as.data.frame(
                            apply(predict(modelFit, newdata, ncomp = submodels$.ncomp), 3, function(x) list(x))))
                      }
                      
                      out <- c(list(out), tmp)   
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- predict(modelFit, newdata, type = "prob",  ncomp = modelFit$tuneValue$.ncomp)
                    if(length(dim(out)) == 3){
                      if(dim(out)[1] > 1) {
                        out <- out[,,1]
                      } else {
                        out <- as.data.frame(t(out[,,1]))
                      }
                    }
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      
                      for(j in seq(along = submodels$.ncomp))
                      {
                        tmpProb <- predict(modelFit, newdata, type = "prob",  ncomp = submodels$.ncomp[j])
                        if(length(dim(tmpProb)) == 3){
                          if(dim(tmpProb)[1] > 1) {
                            tmpProb <- tmpProb[,,1]
                          } else {
                            tmpProb <- as.data.frame(t(tmpProb[,,1]))
                          }
                        } 
                        tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels,drop = FALSE])
                      }
                      out <- tmp
                    }                        
                    out
                    },
                    tags = c("Partial Least Squares", "Feature Extraction", "Linear Classifier"),
                  sort = function(x) x[order(-x$ncomp),,drop = FALSE])
