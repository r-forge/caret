modelInfo <- list(library = "RWeka",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('threshold', 'pruned'),
                                          class = c("numeric", "character"),
                                          label = "Confidence Threshold", 'Pruning'),
                  grid = function(x, y, len = NULL) 
                    data.frame(.threshold = 0.25, .pruned = "yes"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "control"))
                    {
                      theDots$control$U <- ifelse(param$.pruned == "No", TRUE, FALSE)
                      theDots$control$C <- param$.threshold
                      ctl <- theDots$control
                      theDots$control <- NULL
                      
                    } else ctl <- Weka_control(N = ifelse(param$.pruned == "No", TRUE, FALSE),
                                               C = param$.threshold) 
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = dat,
                                        control = ctl),
                                   theDots)
                    
                    out <- do.call("PART", modelArgs) 
                    out      
                    },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type = "probability"),
                  tags = c("Rule-Based Model", "Implicit Feature Selection"),
                  sort = function(x) x)
