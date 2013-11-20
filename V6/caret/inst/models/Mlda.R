modelInfo <- list(library = "HiDimDA",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('parameter'),
                                          class = c('numeric'),
                                          label = c('none')),
                  grid = function(x, y, len = NULL) 
                    data.frame(.parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    Mlda(x, y, q = param$.q, maxq = param$.q, ...),
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata)$class
                    out <- modelFit$obsLevels[as.numeric(out)]
                    out
                  },
                  prob = NULL,
                  tags = c("Discriminant Analysis"),
                  sort = function(x) x)
