modelInfo <- list(library = "rrcovHD",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = 'parameter',
                                          class = "character",
                                          label = 'parameter'),
                  grid = function(x, y, len = NULL) {
                    data.frame(parameter = "none")
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    RSimca(x, y, ...),
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata)@classification,
                  prob = NULL,
                  tags = c('Robust Methods'),
                  sort = function(x) x)
