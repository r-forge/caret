modelInfo <- list(library = "rpart",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('maxdepth'),
                                          class = c("numeric"),
                                          label = c("Max Tree Depth")),
                  grid = function(x, y, len = NULL){
                    dat <- x
                    dat$.outcome <- y
                    initialFit <- rpart(.outcome ~ .,
                                        data = dat,
                                        control = rpart.control(cp = 0))$cptable
                    initialFit <- initialFit[order(-initialFit[,"CP"]), "nsplit", drop = FALSE]
                    initialFit <- initialFit[initialFit[,"nsplit"] > 0 & initialFit[,"nsplit"] <= 30, , drop = FALSE]
                    if(dim(initialFit)[1] < len)
                    {
                      cat("note: only", nrow(initialFit),
                        "possible values of the max tree depth from the initial fit.\n",
                        "Truncating the grid to", nrow(initialFit), ".\n\n")
                      tuneSeq <-  as.data.frame(initialFit)
                    } else tuneSeq <-  as.data.frame(initialFit[1:len,])
                    colnames(tuneSeq) <- ".maxdepth"
                    tuneSeq
                  },
                  loop = function(grid) {
                    grid <- grid[order(grid$.maxdepth, decreasing = TRUE),, drop = FALSE]
                    loop <- grid[1,,drop = FALSE]
                    submodels <- list(grid[-1,,drop = FALSE])
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    theDots <- list(...)
                    if(any(names(theDots) == "control"))
                    {
                      theDots$control$maxdepth <- param$.maxdepth
                      theDots$control$xval <- 0 
                      ctl <- theDots$control
                      theDots$control <- NULL    
                    } else ctl <- rpart.control(maxdepth = param$.maxdepth, xval = 0)  
                    
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
                    ## Models are indexed by Cp so approximate the Cp for
                    ## the value of maxdepth
                    depth2cp <- function(x, depth)
                    {
                      out <- approx(x[,"nsplit"], x[,"CP"], depth)$y
                      out[depth > max(x[,"nsplit"])] <- min(x[,"CP"]) * .99
                      out
                    }
                    
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    
                    pType <- if(modelFit$problemType == "Classification") "class" else "vector"
                    out  <- predict(modelFit, newdata, type=pType)
                    
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      cpValues <- depth2cp(modelFit$cptable, submodels$.maxdepth)
                      for(j in seq(along = cpValues))
                      {
                        prunedFit <- prune.rpart(modelFit, cp = cpValues[j])
                        tmp[[j+1]]  <- predict(prunedFit, newdata, type=pType)
                      }
                      out <- tmp
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    depth2cp <- function(x, depth)
                    {
                      out <- approx(x[,"nsplit"], x[,"CP"], depth)$y
                      out[depth > max(x[,"nsplit"])] <- min(x[,"CP"]) * .99
                      out
                    }
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    out <- predict(modelFit, newdata, type = "prob")
                    
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      cpValues <- depth2cp(modelFit$cptable, submodels$.maxdepth)
                      
                      for(j in seq(along = cpValues))
                      {
                        prunedFit <- prune.rpart(modelFit, cp = cpValues[j])
                        tmpProb <- predict(prunedFit, newdata, type = "prob")
                        tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, drop = FALSE])
                      }
                      out <- tmp
                    }                              
                    out
                  },
                  tags = c("Tree-Based Model", "Implicit Feature Selection"),
                  sort = function(x) x)
