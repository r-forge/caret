modelInfo <- list(library = c("earth", "mda"),
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = c("degree", "nprune"),
                                          class = c("numeric", "numeric"),
                                          label = c('Product Degree', '#Terms')),
                  grid = function(x, y, len = NULL) {
                    dat <- x
                    dat$.outcome <- y
                    
                    mod <- fda( .outcome~., data = dat, method = earth, pmethod = "none")
                    maxTerms <- nrow(mod$fit$dirs) - 1
                    
                    maxTerms <- min(200, floor(maxTerms * .75) + 2)
                    data.frame(.nprune = unique(floor(seq(2, to = maxTerms, length = len))),
                               .degree = 1)
                  },
                  fit = function(x, y, wts, param, lev, last, weights, ...) {
                    dat <- x
                    dat$.outcome <- y
                    fda(.outcome ~ ., data = dat, method = earth, 
                        degree = param$.degree,
                        nprune = param$.nprune, ...)
                  },
                  predict = function(modelFit, newdata, preProc = NULL, param = NULL) as.character(predict(modelFit , newdata)),
                  prob = function(modelFit, newdata, preProc = NULL, param = NULL) predict(modelFit, newdata, type= "posterior"),
                  sort = function(x) x)
