modelInfo <- list(library = "Cubist",
                  loop = function(grid) {     
                    ## Here, we want `loop` to be a data frame with the unique values
                    ## of `committees`. We don't need `neighbors` until `predit.cubist` 
                    ## is used.
                                
                    coms <- unique(grid$.committees)
                    loop <- data.frame(.committees = coms)
                    
                    submodels <- vector(mode = "list", length = length(coms))
                    
                    ## For each value of committees, find the largest 
                    ## value of `neighbors` and assign it to `loop`. 
                    ## Then save the rest to a data frame and add it to
                    ## `submodels`.
                    for(i in seq(along = coms))
                      submodels[[i]] <- grid[grid$.committees == coms[i],
                                             ".neighbors", 
                                             drop = FALSE]

                    list(loop = loop, submodels = submodels)
                  },
                  type = "Regression",
                  parameters = data.frame(parameter = c('committees', 'neighbors'),
                                          class = rep('numeric', 2),
                                          label = c('#Committees', '#Instances')),
                  grid = function(x, y, len = NULL) expand.grid(.neighbors = c(0, 5, 9),
                                                                .committees = c(1, 10, 20)),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    out <- cubist(x, y, committees =  param$.committees, ...)
                    if(last) out$tuneValue$.neighbors <- param$.neighbors
                    out
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.null(submodels))
                    {
                      out <- vector(mode = "list", length = nrow(submodels))                
                      for(j in seq(along = submodels$.neighbors))
                        out[[j]] <- predict(modelFit, newdata, neighbors = submodels$.neighbors[j])
                    } else out <- predict(modelFit, newdata, neighbors = modelFit$tuneValue$.neighbors)
                    out                
                  },
                  tags = c("Rule-Based Models", "Boosting", "Ensemble Model", "Prototype Models"),
                  prob = NULL,
                  sort = function(x) x)
