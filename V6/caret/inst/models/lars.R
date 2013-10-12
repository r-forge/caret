modelInfo <- list(library = "lars",
                  type = "Regression",
                  parameters = data.frame(parameter = 'fraction',
                                          class = "numeric",
                                          label = 'Fraction'),
                  grid = function(x, y, len = NULL)
                    expand.grid(.fraction = seq(0.05, 1, length = len)),
                  loop = function(grid) {   
                    grid <- grid[order(grid$.fraction, decreasing = TRUE),, drop = FALSE]
                    loop <- grid[1,,drop = FALSE]
                    submodels <- list(grid[-1,,drop = FALSE])     
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    lars(as.matrix(x), y, ...),
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit,
                                   as.matrix(newdata),
                                   type = "fit",
                                   mode = "fraction",
                                   s = modelFit$tuneValue$.fraction)$fit
                    
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      
                      for(j in seq(along = submodels$.fraction))
                      {
                        tmp[[j+1]] <- predict(modelFit,
                                              as.matrix(newdata),
                                              type = "fit",
                                              mode = "fraction",
                                              s = submodels$.fraction[j])$fit
                      }
                      out <- tmp
                    }
                    out        
                  },
                  tags = c("Linear Regression", "Implicit Feature Selection", "L1 Regularization"),
                  prob = NULL,
                  sort = function(x) x)
