modelInfo <- list(library = NULL,
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = "parameter",
                                          class = NA,
                                          label = "parameter"),
                  grid = function(x, y, len = NULL) data.frame(.parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, weights, ...) {
                    dat <- x
                    dat$.outcome <- y
                    if(!is.null(wts))
                    {
                      out <- lm(.outcome ~ ., data = dat, weights = wts, ...)
                    } else out <- lm(.outcome ~ ., data = dat, ...)
                    out
                  },
                  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) predict(modelFit, newdata),
                  prob = NULL,
                  sort = function(x) x)
