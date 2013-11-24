modelInfo <- list(library = "kernlab",
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = c("sigma"),
                                          class = "numeric",
                                          label = "Sigma"),
                  grid = function(x, y, len = NULL) {
                    library(kernlab)
                    sigmas <- sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)   
                    data.frame(.sigma = mean(sigmas[-2]))
                    },
                  fit = function(x, y, wts, param, lev, last,classProbs, ...) {
                    kernlab:::rvm(x = as.matrix(x), y = y,
                                  kernel = rbfdot,
                                  kpar = list(sigma = param$.sigma),
                                  ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata),
                  prob = NULL,
                  tags = c("Kernel Method", "Relevance Vector Machines", "Radial Basis Function"),
                  sort = function(x) x)
