modelInfo <- list(library = "glmnet",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('alpha', 'lambda'),
                                          class = c("numeric", "numeric"),
                                          label = c('Mixing Percentage', 'Regularization Parameter')),
                  grid = function(x, y, len = NULL) 
                    expand.grid(.alpha = seq(0.1, 1, length = len),
                                .lambda = seq(.1, 3, length = 3 * len)),
                  loop = function(grid) {   
                    uniqueAlpha <- unique(grid$.alpha)
                    loop <- data.frame(.alpha = uniqueAlpha)
                    loop$.lambda <- NA
                    submodels <- vector(mode = "list", length = length(uniqueAlpha))
                    
                    for(i in seq(along = uniqueAlpha))
                      submodels[[i]] <- data.frame(.lambda = subset(grid, 
                                                                    subset = .alpha == uniqueAlpha[i])$.lambda)
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    numLev <- if(is.character(y) | is.factor(y)) length(levels(y)) else NA
                    
                    theDots <- list(...)
                    
                    if(all(names(theDots) != "family"))
                    {
                      if(!is.na(numLev))
                      {
                        fam <- ifelse(numLev > 2, "multinomial", "binomial")
                      } else fam <- "gaussian"
                      
                      theDots$family <- fam   
                    }
                    
                    ## pass in any model weights
                    if(!is.null(wts)) theDots$weights <- wts
                    
                    modelArgs <- c(list(x = as.matrix(x),
                                        y = y,
                                        alpha = param$.alpha),
                                   theDots)
                    
                    out <- do.call("glmnet", modelArgs) 
                    out 
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    
                    if(!is.null(submodels))
                    {
                      if(length(modelFit$obsLevels) < 2)
                      {
                        out <- as.list(as.data.frame(predict(modelFit, newdata, s = submodels$.lambda)))
                      } else {
                        out <- predict(modelFit, newdata, s = submodels$.lambda, type = "class")
                        out <- as.list(as.data.frame(out, stringsAsFactors = FALSE))
                      }
                    } else {
                      
                      if(is.null(modelFit$lambdaOpt))
                        stop("optimal lambda not saved by train; needs a single lambda value")
                      if(length(modelFit$obsLevels) < 2)
                      {
                        out <- predict(modelFit, newdata, s = modelFit$lambdaOpt)[,1]
                      } else {
                        out <- predict(modelFit, newdata, s = modelFit$lambdaOpt, type = "class")[,1]
                      }
                    }
                    out       
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    obsLevels <- if("classnames" %in% names(modelFit)) modelFit$classnames else NULL
                    if(length(obsLevels) == 2)
                    {
                      if(!is.null(submodels))
                      {
                        probs <- predict(modelFit,
                                         as.matrix(newdata),
                                         s = submodels$.lambda,
                                         type = "response")
                        
                        probs <- as.list(as.data.frame(probs))
                        probs <- lapply(probs,
                                        function(x, lev)
                                        {
                                          tmp <- data.frame(x, 1-x)
                                          names(tmp) <- lev
                                          tmp
                                        },
                                        lev = modelFit$obsLevels)
                        
                      } else {
                        probs <- predict(modelFit,
                                         as.matrix(newdata),
                                         s = modelFit$lambdaOpt,
                                         type = "response")
                        probs <- cbind(1-probs, probs)
                        colnames(probs) <- modelFit$obsLevels
                      }
                    } else {
                      if(!is.null(submodels))
                      {
                        ## This generates a 3d array
                        probs <- predict(modelFit,
                                         as.matrix(newdata),
                                         s = submodels$.lambda,
                                         type = "response")
                        ## convert it to a list of 2d structures
                        probs <- apply(probs, 3, function(x) data.frame(x))
                      } else {
                        probs <- predict(modelFit,
                                         as.matrix(newdata),
                                         s = modelFit$lambdaOpt,
                                         type = "response")
                        probs <- probs[,,1]
                      }
                    }
                    
                    probs
                  },
                  tags = c("Generalized Linear Model", "Implicit Feature Selection", 
                           "L1 Regularization", "L2 Regularization", "Linear Classifier",
                           "Linear Regression"),
                  sort = function(x) x)
