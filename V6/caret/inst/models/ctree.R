modelInfo <- list(library = "party",
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = 'mincriterion',
                                          class = 'numeric',
                                          label = '1 - P-Value Threshold'),
                  grid = function(x, y, len = NULL) {
                    data.frame(.mincriterion = seq(from = .99, to = 0.01, length = len))
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y

                    theDots <- list(...)

                    if(any(names(theDots) == "controls"))
                      {
                        theDots$controls@gtctrl@mincriterion <- param$.mincriterion
                        ctl <- theDots$controls
                        theDots$controls <- NULL

                      } else ctl <- do.call(getFromNamespace("ctree_control", "party"), 
                                            list(mincriterion = param$.mincriterion))

                    ## pass in any model weights
                    if(!is.null(wts)) theDots$weights <- wts

                    modelArgs <- c(
                                   list(
                                        formula = as.formula(".outcome ~ ."),
                                        data = dat,
                                        controls = ctl),
                                   theDots)

                    out <- do.call(getFromNamespace("ctree", "party"), modelArgs)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    out <- predict(modelFit, newdata)
                    if(!is.null(modelFit@responses@levels$.outcome)) out <- as.character(out)
                    if(is.matrix(out)) out <- out[,1]
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    obsLevels <- levels(modelFit@data@get("response")[,1])
                    rawProbs <- treeresponse(modelFit, newdata)
                    probMatrix <- matrix(unlist(rawProbs), ncol = length(obsLevels), byrow = TRUE)
                    out <- data.frame(probMatrix)
                    colnames(out) <- obsLevels
                    rownames(out) <- NULL
                    out
                  },
                  tags = c('Tree-Based Model', "Implicit Feature Selection"),
                  levels = function(x) levels(x@data@get("response")[,1]),
                  sort = function(x) x)
