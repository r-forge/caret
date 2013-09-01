modelInfo <- list(library = NULL,
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = "k",
                                          class = "numeric",
                                          label = "#Neighbors"),
                  grid = function(x, y, len = NULL) data.frame(.k = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0]),
                  fit = function(x, y, wts, param, lev, last, weights, ...) {
                    if(is.factor(y))
                    {
                      knn3(as.matrix(x), y, k = param$.k, ...)
                    } else {
                      knnreg(as.matrix(x), y, k = param$.k, ...)
                    }
                  },
                  predict = function(modelFit, newdata, preProc = NULL, param = NULL) {
                    if(modelFit$problemType == "Classification")
                    {
                      out <- as.character(predict(modelFit, newdata,  type = "class"))
                    } else {
                      out <- predict(modelFit, newdata)
                    }
                    out
                  },
                  prob = function(modelFit, newdata, preProc = NULL, param = NULL){
                    predict(modelFit, newdata, type = "prob")
                  },
                  sort = function(x) x)
