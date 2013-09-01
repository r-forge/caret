modelInfo <- list(library = NULL,
                  loop = NULL,
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = "parameter",
                                          class = NA,
                                          label = "parameter"),
                  grid = function(x, y, len = NULL) data.frame(.parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, weights, ...) {
                    dat <- x
                    dat$.outcome <- y
                    if(length(levels(y)) > 2) stop("glm models can only use 2-class outcomes")
                    
                    theDots <- list(...)
                    if(!any(names(theDots) == "family"))
                    {
                      theDots$family <- if(is.factor(y)) binomial() else gaussian()              
                    }
                    
                    ## pass in any model weights
                    if(!is.null(wts)) theDots$weights <- wts
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), data = dat), theDots)
                    
                    out <- do.call("glm", modelArgs)
                    ## When we use do.call(), the call infformation can contain a ton of
                    ## information. Inlcuding the contenst of the data. We eliminate it.
                    out$call <- NULL
                    out
                  },
                  predict = function(modelFit, newdata, preProc = NULL, param = NULL) {
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
                  prob = function(modelFit, newdata, preProc = NULL, param = NULL){
                    out <- predict(modelFit, newdata, type = "response")
                    out <- cbind(1-out, out)
                    ## glm models the second factor level, we treat the first as the
                    ## event of interest. See Details in ?glm
                    dimnames(out)[[2]] <-  modelFit$obsLevels
                    out
                  },
                  sort = function(x) x)
