modelInfo <- list(library = "kernlab",
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = "parameter",
                                          class = NA,
                                          label = "parameter"),
                  grid = function(x, y, len = NULL) data.frame(.parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    kernlab:::rvm(x = as.matrix(x), y = y,
                                  kernel = vanilladot(),
                                  ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = NULL,
                  tags = c("Kernel Methods", "Relevance Vector Machines", "Linear Regression"),
                  sort = function(x) x)
