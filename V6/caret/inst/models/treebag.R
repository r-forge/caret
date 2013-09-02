modelInfo <- list(library = "ipred",
                  loop = NULL,
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = "parameter",
                                          class = NA,
                                          label = "parameter"),
                  grid = function(x, y, len = NULL) data.frame(.parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, weights, ...) {
                    theDots <- list(...)
                    if(!any(names(theDots) == "keepX")) theDots$keepX <- FALSE   
                    modelArgs <- c(list(X = x, y = y), theDots)
                    do.call("ipredbagg", modelArgs)
                  },
                  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, preProc = NULL, submodels = NULL)
                    predict(modelFit, newdata, type = "prob"),
                  sort = function(x) x)
