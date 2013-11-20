modelInfo <- list(library = "elasticnet",
                  type = "Regression",
                  parameters = data.frame(parameter = c('lambda'),
                                          class = c("numeric"),
                                          label = c('Weight Decay')),
                  grid = function(x, y, len = NULL) 
                    expand.grid(.lambda = c(0, 10 ^ seq(-1, -4, length = len - 1))),
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    enet(as.matrix(x), y, lambda = param$.lambda)  
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    predict(modelFit, as.matrix(newdata), 
                            s = 1, 
                            mode = "fraction")$fit
                  },
                  tags = c("Linear Regression", "L2 Regularization"),
                  prob = NULL,
                  sort = function(x) x)
