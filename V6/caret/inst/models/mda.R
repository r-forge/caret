modelInfo <- list(library = "mda",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('subclasses'),
                                          class = c('numeric'),
                                          label = c('#Subclasses Per Class')),
                  grid = function(x, y, len = NULL) 
                    data.frame(.subclasses = (1:len) + 1),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y
                    mda(as.formula(".outcome ~ ."), data = dat, 
                        subclasses = param$.subclasses, ...)      
                  },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type = "posterior"),
                  predictors = function(x, ...) predictors(x$terms),
                  tags = c("Discriminant Analysis"),
                  sort = function(x) x)
