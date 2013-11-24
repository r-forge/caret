modelInfo <- list(library = c("klaR", "MASS"),
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c("maxvar", "direction"),
                                          class = c("numeric", "character"),
                                          label = c('Maximum #Variables', 'Search Direction')),
                  grid = function(x, y, len = NULL) 
                    data.frame(.maxvar = Inf, .direction = "both"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...){
                    out <- stepclass(x, y,
                                     method = "lda",
                                     maxvar = param$.maxvar,
                                     direction = as.character(param$.direction),
                                     ...)
                    out$fit <- lda(x[, predictors(out), drop = FALSE], y, ...)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit$fit, newdata[,  predictors(modelFit), drop = FALSE])$class,
                  prob = function(modelFit, newdata, submodels = NULL){
                    predict(modelFit$fit, newdata[, predictors(modelFit), drop = FALSE])$posterior
                  },
                  tags = c("Discriminant Analysis", "Feature Selection Wrapper", "Linear Classifier"),
                  sort = function(x) x)
