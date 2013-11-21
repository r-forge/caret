modelInfo <- list(library = "rrcov",
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL) data.frame(.parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    rrcov:::Linda(x, y, ...),
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata)@classification,
                  prob = function(modelFit, newdata, submodels = NULL) {
                    probs <- predict(modelFit, newdata)@posterior
                    colnames(probs) <- names(modelFit@prior)
                    probs
                  },
                  tags = "Discriminant Analysis",
                  sort = function(x) x)
