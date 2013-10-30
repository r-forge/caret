modelInfo <- list(library = c("earth", "mda"),
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = c("degree", "nprune"),
                                          class = c("numeric", "numeric"),
                                          label = c('Product Degree', '#Terms')),
                  grid = function(x, y, len = NULL) {
                    dat <- if(!is.data.frame(x)) as.data.frame(x) else x
                    dat$.outcome <- y
                    
                    mod <- fda( .outcome~., data = dat, method = earth, pmethod = "none")
                    maxTerms <- nrow(mod$fit$dirs) - 1
                    
                    maxTerms <- min(200, floor(maxTerms * .75) + 2)
                    data.frame(.nprune = unique(floor(seq(2, to = maxTerms, length = len))),
                               .degree = 1)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y
                    fda(.outcome ~ ., data = dat, method = earth, 
                        degree = param$.degree,
                        nprune = param$.nprune, ...)
                  },
                  tags = c("Multivariate Adaptive Regression Splines", "Implicit Feature Selection"),
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit , newdata),
                  prob = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata, type= "posterior"),
                  sort = function(x) x)
