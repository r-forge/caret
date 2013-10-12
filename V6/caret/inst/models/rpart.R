modelInfo <- list(library = "rpart",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('cp'),
                                          class = c("numeric"),
                                          label = c("Complexity Parameter")),
                  grid = function(x, y, len = NULL){
                    dat <- x
                    dat$.outcome <- y
                    initialFit <- rpart(.outcome ~ .,
                                        data = dat,
                                        control = rpart.control(cp = 0))$cptable
                    initialFit <- initialFit[order(-initialFit[,"CP"]), , drop = FALSE]
                    
                    if(nrow(initialFit) < len)
                    {
                      tuneSeq <- data.frame(.cp = seq(min(initialFit[, "CP"]), 
                                                      max(initialFit[, "CP"]), 
                                                      length = len))
                    } else tuneSeq <-  data.frame(.cp = initialFit[1:len,"CP"])
                    colnames(tuneSeq) <- ".cp"
                    tuneSeq
                  },
                  loop = function(grid) {
                    grid <- grid[order(grid$.cp, decreasing = FALSE),, drop = FALSE]
                    loop <- grid[1,,drop = FALSE]
                    submodels <- list(grid[-1,,drop = FALSE])
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    theDots <- list(...)
                    if(any(names(theDots) == "control"))
                    {
                      theDots$control$cp <- param$.cp
                      theDots$control$xval <- 0 
                      ctl <- theDots$control
                      theDots$control <- NULL
                    } else ctl <- rpart.control(cp = param$.cp, xval = 0)   
                    
                    ## check to see if weights were passed in (and availible)
                    if(!is.null(wts)) theDots$weights <- wts    
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = x,
                                        control = ctl),
                                   theDots)
                    modelArgs$data$.outcome <- y
                    
                    out <- do.call("rpart", modelArgs)
                    out           
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {                  
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    
                    pType <- if(modelFit$problemType == "Classification") "class" else "vector"
                    out  <- predict(modelFit, newdata, type=pType)
                    
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out

                      for(j in seq(along = submodels$.cp))
                      {
                        prunedFit <- prune.rpart(modelFit, cp = submodels$.cp[j])
                        tmp[[j+1]]  <- predict(prunedFit, newdata, type=pType)
                      }
                      out <- tmp
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    out <- predict(modelFit, newdata, type = "prob")
                    
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      for(j in seq(along = submodels$.cp))
                      {
                        prunedFit <- prune.rpart(modelFit, cp = submodels$.cp[j])
                        tmpProb <- predict(prunedFit, newdata, type = "prob")
                        tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels])
                      }
                      out <- tmp
                    }                              
                    out
                  },
                  tags = c("Tree-Based Model", "Implicit Feature Selection"),
                  sort = function(x) x)
