modelInfo <- list(library = "gam",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('span', 'degree'),
                                          class = c('numeric', 'numeric'),
                                          label = c('Span', 'Degree')),
                  grid = function(x, y, len = NULL) 
                    expand.grid(.span = .5, .degree = 1:2),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y
                    
                    gam:::gam(caret:::smootherFormula(x,
                                              smoother = "lo",
                                              span = param$.span,
                                              degree = param$.degree),
                              data = dat,
                              family =  if(is.factor(y)) binomial() else  gaussian(),
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
