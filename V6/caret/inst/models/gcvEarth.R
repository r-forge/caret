modelInfo <- list(library = "earth",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('degree'),
                                          class = c("numeric"),
                                          label = c('Product Degree')),
                  grid = function(x, y, len = NULL) {
                    data.frame(.degree = 1)
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    theDots <- list(...)
                    theDots$keepxy <- TRUE 
                    
                    modelArgs <- c(list(x = x, y = y,
                                        degree = param$.degree),
                                   theDots)
                    if(is.factor(y)) modelArgs$glm <- list(family=binomial)
                    
                    tmp <- do.call("earth", modelArgs)
                    
                    tmp$call["degree"] <-  param$.degree
                    tmp 
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(modelFit$problemType == "Classification")
                    {
                      out <- predict(modelFit, newdata,  type = "class")
                    } else {
                      out <- predict(modelFit, newdata)
                    }
                    as.vector(out)            
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata, type= "response")
                    out <- cbind(1-out, out)
                    colnames(out) <-  modelFit$obsLevels
                    out
                  },
                  predictors = function(x, ...) {
                    vi <- varImp(x)
                    notZero <- sort(unique(unlist(lapply(vi, function(x) which(x > 0)))))
                    if(length(notZero) > 0) rownames(vi)[notZero] else NULL
                  },
                  tags = c("Multivariate Adaptive Regression Splines", "Implicit Feature Selection"),
                  sort = function(x) x)
