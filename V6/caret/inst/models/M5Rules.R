modelInfo <- list(library = "RWeka",
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = c('pruned', 'smoothed'),
                                          class = rep("character", 2),
                                          label = c('Pruned', 'Smoothed')),
                  grid = function(x, y, len = NULL) expand.grid(.pruned = c("Yes", "No"), 
                                                                .smoothed = c("Yes", "No")),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "control"))
                    {
                      theDots$control$N <- ifelse(param$.pruned == "No", TRUE, FALSE)
                      theDots$control$U <- ifelse(param$.smoothed == "No", TRUE, FALSE)
                      ctl <- theDots$control
                      theDots$control <- NULL
                      
                    } else ctl <- Weka_control(N = ifelse(param$.pruned == "No", TRUE, FALSE),
                                               U = ifelse(param$.smoothed == "No", TRUE, FALSE)) 
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        control = ctl),
                                   theDots)
                    modelArgs$data <- x
                    modelArgs$data$.outcome <- y
                    
                    out <- do.call("M5Rules", modelArgs) 
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = NULL,
                  tags = c("Rule-Based Model", "Linear Regression", "Implicit Feature Selection"),
                  sort = function(x) x)
