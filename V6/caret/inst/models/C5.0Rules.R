modelInfo <- list(library = "C50",
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = c('parameter'),
                                          class = c("character"),
                                          label = c('none')),
                  grid = function(x, y, len = NULL) data.frame(.parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    C5.0(x = x, y = y, rules = TRUE, ...),
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata, type= "prob"),
                  tags = c("Rule-Based Model", "Implicit Feature Selection"),
                  sort = function(x) x)
