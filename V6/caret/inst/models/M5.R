modelInfo <- list(library = "RWeka",
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = c('pruned', 'smoothed', 'rules'),
                                          class = rep("character", 3),
                                          label = c('Pruned', 'Smoothed', 'Rules')),
                  grid = function(x, y, len = NULL) expand.grid(.pruned = c("Yes", "No"), 
                                                                .smoothed = c("Yes", "No"), 
                                                                .rules = c("Yes", "No")),
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
                    
                    out <- do.call(if(param$.rules == "Yes") "M5Rules" else "M5P", modelArgs) 
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = NULL,
                  sort = function(x) x)
