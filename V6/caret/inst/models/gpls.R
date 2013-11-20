modelInfo <- list(library = "gpls",
                  loop = NULL,
                  type = c('Classification'),
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('K.prov'),
                                          class = c('numeric'),
                                          label = c('#Components')),
                  grid = function(x, y, len = NULL) data.frame(.K.prov =seq(1, len)),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    gpls(x, y, K.prov = param$.K.prov, ...),
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata)$class,
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata)$predicted
                    out <- cbind(out, 1-out)
                    colnames(out) <-  modelFit$obsLevels
                    out
                  },
                  tags = c("Gaussian Process", "Bayesian Analysis"),
                  sort = function(x) x)
