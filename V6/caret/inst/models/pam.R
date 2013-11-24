modelInfo <- list(library = "pamr",
                  type = "Classification",
                  parameters = data.frame(parameter = 'threshold',
                                          class = "numeric",
                                          label = 'Shrinkage Threshold'),
                  grid = function(x, y, len = NULL) {
                    cc <- complete.cases(x) & complete.cases(y)
                    x <- x[cc,,drop = FALSE]
                    y <- y[cc]
                    initialThresh <- pamr.train(list(x=t(x), y=y))$threshold
                    initialThresh <- initialThresh[-c(1, length(initialThresh))]
                    tuneSeq <- data.frame(.threshold = seq(from = min(initialThresh),
                                                           to = max(initialThresh), 
                                                           length = len))
                  },
                  loop = function(grid) {   
                    grid <- grid[order(grid$.threshold, decreasing = TRUE),, drop = FALSE]
                    loop <- grid[1,,drop = FALSE]
                    submodels <- list(grid[-1,,drop = FALSE])       
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    pamr.train(list(x = t(x), y = y), threshold = param$.threshold, ...),
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- pamr.predict(modelFit,
                                        t(newdata),
                                        threshold = modelFit$tuneValue$.threshold)

                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      for(j in seq(along = submodels$.threshold))
                      {
                        tmp[[j+1]] <- pamr.predict(modelFit,
                                                   t(newdata),
                                                   threshold = submodels$.threshold[j])
                      }
                      out <- tmp
                    }
                    out         
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- pamr.predict(modelFit, t(newdata),
                                        threshold = modelFit$tuneValue$.threshold, 
                                        type= "posterior")
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      
                      for(j in seq(along = submodels$.threshold))
                      {
                        tmpProb <-  pamr.predict(modelFit, t(newdata),
                                                 threshold =  submodels$.threshold[j], 
                                                 type= "posterior")
                        tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels,drop = FALSE])
                      }
                      out <- tmp
                    }   
                    out
                  },
                  tags = c("Prototype Models", "Implicit Feature Selection", "Linear Classifier"),
                  sort = function(x) x)
