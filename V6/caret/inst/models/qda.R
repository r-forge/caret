modelInfo <- list(library = "MASS",
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL) data.frame(.parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) qda(x, y, ...)  ,
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata)$class,
                  prob = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata)$posterior,
                  tags = "Discriminant Analysis",
                  sort = function(x) x)
