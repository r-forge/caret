modelInfo <- list(library = "gbm",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('n.trees', 'interaction.depth', 'shrinkage'),
                                          class = c("numeric", "numeric", "numeric"),
                                          label = c('# Boosting Iterations', 'Max Tree Depth', 'Shrinkage')),
                  grid = function(x, y, len = NULL) expand.grid(.interaction.depth = seq(1, len),
                                                                .n.trees = floor((1:len) * 50),
                                                                .shrinkage = .1),
                  loop = function(grid) {     
                    loop <- ddply(grid, c(".shrinkage", ".interaction.depth"),
                                  function(x) c(.n.trees = max(x$.n.trees)))
                    submodels <- vector(mode = "list", length = nrow(loop))
                    for(i in seq(along = loop$.n.trees))
                    {
                      index <- which(grid$.interaction.depth == loop$.interaction.depth[i] & 
                                       grid$.shrinkage == loop$.shrinkage[i])
                      trees <- grid[index, ".n.trees"] 
                      submodels[[i]] <- data.frame(.n.trees = trees[trees != loop$.n.trees[i]])
                    }    
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, weights, classProbs, ...) { 
                    ## train will figure out whether we are doing classification or reggression
                    ## from the class of the outcome and automatically specify the value of
                    ## 'distribution' in the control file. If the user wants to over-ride this,
                    ## this next bit will allow this.
                    theDots <- list(...)
                    if(any(names(theDots) == "distribution"))
                    {
                      modDist <- theDots$distribution
                      theDots$distribution <- NULL
                    } else {
                      if(is.numeric(y))
                      {
                        modDist <- "gaussian"
                      } else modDist <- if(length(lev) == 2)  "bernoulli" else "multinomial"
                    }
                    
                    ## check to see if weights were passed in (and availible)
                    if(!is.null(wts)) theDots$w <- wts     
                    if(is.factor(y) && length(lev) == 2) y <- ifelse(y == lev[1], 1, 0)
                    
                    modArgs <- list(x = x,
                                    y = y,
                                    interaction.depth = param$.interaction.depth,
                                    n.trees = param$.n.trees,
                                    shrinkage = param$.shrinkage, 
                                    distribution = modDist)
                    
                    if(length(theDots) > 0) modArgs <- c(modArgs, theDots)
                    
                    do.call("gbm.fit", modArgs)
                    },
                  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
                    if(modelFit$problemType == "Classification")
                    {
                      gbmProb <- predict(modelFit, newdata, type = "response",
                                         n.trees = modelFit$tuneValue$.n.trees)
                      gbmProb[is.nan(gbmProb)] <- NA
                      if(modelFit$distribution$name != "multinomial")
                      {
                        out <- ifelse(gbmProb >= .5, modelFit$obsLevels[1], modelFit$obsLevels[2])
                        ## to correspond to gbmClasses definition above
                      } else {
                        out <- colnames(gbmProb)[apply(gbmProb, 1, which.max)]
                      }
                    } else {
                      out <- predict(modelFit, newdata, type = "response",
                                     n.trees = modelFit$tuneValue$.n.trees)
                    }
                    
                    if(!is.null(submodels))
                    {
                      tmp <- predict(modelFit, newdata, type = "response", n.trees = submodels$.n.trees)
                      
                      if(modelFit$problemType == "Classification")
                      {
                        if(modelFit$distribution$name != "multinomial")
                        {
                          if(is.vector(tmp)) tmp <- matrix(tmp, ncol = 1)
                          tmp <- apply(tmp, 2,
                                       function(x, nm = modelFit$obsLevels) ifelse(x >= .5, nm[1], nm[2]))
                          
                        } else {
                          tmp <- apply(tmp, 3,
                                       function(y, nm = modelFit$obsLevels) nm[apply(y, 1, which.max)])
                        }
                      }
                      if(!is.list(tmp)) tmp <- split(tmp, rep(1:ncol(tmp), each = nrow(tmp)))
                      out <- c(list(out), tmp)
                    }
                    out  
                  },
                  prob = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
                    out <- predict(modelFit, newdata, type = "response",
                                   n.trees = modelFit$tuneValue$.n.trees)
                    
                    if(modelFit$distribution$name != "multinomial") 
                    {
                      out <- data.frame(a = out, b = 1-out) 
                      names(out) <-  modelFit$obsLevels
                    } else out <- as.data.frame(out[,,1])
                    if(!is.null(submodels))
                    {
                      tmp <- predict(modelFit, newdata, type = "response", n.trees = submodels$.n.trees)
                      
                      if(modelFit$problemType == "Classification")
                      {
                        if(modelFit$distribution$name != "multinomial")
                        {
                          if(is.vector(tmp)) tmp <- matrix(tmp, ncol = 1)
                          tmp <- apply(tmp, 2,
                                       function(x, nm = modelFit$obsLevels)
                                       {
                                         x <- data.frame(x = x, y = 1 - x)
                                         colnames(x) <- nm
                                         x
                                       })
                        } else {
                          ## Does anyone know of a better
                          ## way to convert an array to a
                          ## list of matrices or data
                          ## frames?                   
                          tmp <- apply(tmp, 3, function(x) data.frame(x))
                        }
                      }
                      out <- c(list(out), tmp)
                    } 
                    out
                  },
                  sort = function(x) x)
