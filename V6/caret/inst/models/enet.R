modelInfo <- list(library = "elasticnet",
                  type = "Regression",
                  parameters = data.frame(parameter = c('fraction', 'lambda'),
                                          class = c("numeric", "numeric"),
                                          label = c('Fraction of Full Solution', 'Weight Decay')),
                  grid = function(x, y, len = NULL) 
                    expand.grid(.lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)),
                                .fraction = seq(0.05, 1, length = len)),
                  loop = function(grid) {   
                    grid <- grid[order(grid$.lambda, grid$.fraction, decreasing = TRUE),, drop = FALSE]
                    uniqueLambda <- unique(grid$.lambda)
                    loop <- data.frame(.lambda = uniqueLambda)
                    loop$.fraction <- NA
                    
                    submodels <- vector(mode = "list", length = length(uniqueLambda))
                    
                    for(i in seq(along = uniqueLambda))
                    {
                      subFrac <- grid[grid$.lambda == uniqueLambda[i],".fraction"]
                      loop$.fraction[loop$.lambda == uniqueLambda[i]] <- subFrac[which.max(subFrac)]
                      submodels[[i]] <- data.frame(.fraction = subFrac[-which.max(subFrac)])
                    }     
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    enet(as.matrix(x), y, lambda = param$.lambda)  
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, as.matrix(newdata), 
                                   s = modelFit$tuneValue$.fraction, 
                                   mode = "fraction")$fit
                    
                    if(!is.null(submodels))
                    {
                      if(nrow(submodels) > 1)
                      {
                        out <- c(
                          list(if(is.matrix(out)) out[,1]  else out),
                          as.list(
                            as.data.frame(
                              predict(modelFit,
                                      newx = as.matrix(newdata),
                                      s = submodels$.fraction,
                                      mode = "fraction")$fit)))
                        
                      } else {
                        tmp <- predict(modelFit,
                                       newx = as.matrix(newdata),
                                       s = submodels$.fraction,
                                       mode = "fraction")$fit
                        out <- c(list(if(is.matrix(out)) out[,1]  else out),  list(tmp))
                      }
                    }
                    out      
                  },
                  prob = NULL,
                  sort = function(x) x)
