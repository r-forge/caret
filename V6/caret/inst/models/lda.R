modelInfo <- list(library = "MASS",
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = "parameter",
                                          class = NA,
                                          label = "parameter"),
                  grid = function(x, y, len = NULL) data.frame(.parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, weights, classProbs, ...) lda(x, y, ...)  ,
                  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) 
                    predict(modelFit, newdata)$class,
                  prob = function(modelFit, newdata, preProc = NULL, submodels = NULL) 
                    predict(modelFit, newdata)$posterior,
                  sort = function(x) x)
