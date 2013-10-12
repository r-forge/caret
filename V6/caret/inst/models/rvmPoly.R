modelInfo <- list(library = "kernlab",
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = c("scale", "degree"),
                                          class = c("numeric", "numeric"),
                                          label = c("Scale", "Polynomial Degree")),
                  grid = function(x, y, len = NULL) {
                    expand.grid(.degree = seq(1, min(len, 3)),      
                                .scale = 10 ^((1:len) - 4))
                    },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    kernlab:::rvm(x = as.matrix(x), y = y,
                                  kernel = polydot,
                                  kpar = list(degree = param$.degree,
                                              scale = param$.scale,
                                              offset = 1),
                                  ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = NULL,
                  tags = c("Kernel Methods", "Relevance Vector Machines"),
                  sort = function(x) x)
