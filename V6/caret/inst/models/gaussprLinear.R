modelInfo <- list(library = "kernlab",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('parameter'),
                                          class = c("character"),
                                          label = c('Parameter')),
                  grid = function(x, y, len = NULL) data.frame(.parameter = "none"),
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    gausspr(x = as.matrix(x), y = y,
                            kernel = vanilladot, kpar = list(), ...)         
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {            
                    out <- predict(modelFit, as.matrix(newdata))
                    if(is.matrix(out)) out <- out[,1]
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    predict(modelFit, as.matrix(newdata), type = "probabilities")
                  },
                  tags = c("Kernel Methods", "Support Vector Machines"),
                  sort = function(x) x)
