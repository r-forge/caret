modelInfo <- list(library = "mgcv",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('select', 'method'),
                                          class = c('logical', 'character'),
                                          label = c('Feature Selection', 'Method')),
                  grid = function(x, y, len = NULL) 
                    expand.grid(.select = c(TRUE, FALSE), .method = "GCV.Cp"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y
                    modForm <- caret:::smootherFormula(x)
                    dist <- if(is.factor(y)) binomial() else  gaussian()
                    mgcv:::gam(modForm,
                               data = dat,
                               family = dist,
                               select = param$.select,
                               method = param$.method,
                               ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(modelFit$problemType == "Classification")
                    {
                      probs <-  predict(modelFit, newdata, type = "response")
                      out <- ifelse(probs < .5,
                                    modelFit$obsLevel[1],
                                    modelFit$obsLevel[2])
                    } else {
                      out <- predict(modelFit, newdata, type = "response")
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    out <- predict(modelFit, newdata, type = "response")
                    out <- cbind(1-out, out)
                    ## glm models the second factor level, we treat the first as the
                    ## event of interest. See Details in ?glm
                    colnames(out) <-  modelFit$obsLevels
                    out
                  },
                  tags = c("Generalized Linear Model"),
                  sort = function(x) x)
