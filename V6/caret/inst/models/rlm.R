modelInfo <- list(library = "MASS",
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = "parameter",
                                          class = NA,
                                          label = "parameter"),
                  grid = function(x, y, len = NULL) data.frame(.parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, weights, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y
                    if(!is.null(wts))
                    {
                      out <- rlm(.outcome ~ ., data = dat, weights = wts, ...)
                    } else out <- rlm(.outcome ~ ., data = dat, ...)
                    out
                  },
                  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = NULL,
                  sort = function(x) x)
