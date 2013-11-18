modelInfo <- list(library = "party",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('maxdepth'),
                                          class = c('numeric'),
                                          label = c('Max Tree Depth')),
                  grid = function(x, y, len = NULL) {
                    data.frame(.maxdepth = 1:len)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y

                    theDots <- list(...)
                    if(any(names(theDots) == "controls"))
                    {
                      theDots$controls@tgctrl@maxdepth <- param$.maxdepth
                      theDots$controls@gtctrl@mincriterion <- 0
                      ctl <- theDots$controls
                      theDots$controls <- NULL
                      
                    } else ctl <- do.call(getFromNamespace("ctree_control", "party"), 
                                          list(maxdepth = param$.maxdepth,
                                               mincriterion = 0))

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
                    out <- predict(modelFit, newdata)
                    if(!is.null(modelFit@responses@levels$.outcome)) out <- as.character(out)
                    if(is.matrix(out)) out <- out[,1]
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    obsLevels <- levels(modelFit@data@get("response")[,1])
                    rawProbs <- treeresponse(modelFit, newdata)
                    probMatrix <- matrix(unlist(rawProbs), ncol = length(obsLevels), byrow = TRUE)
                    out <- data.frame(probMatrix)
                    colnames(out) <- obsLevels
                    rownames(out) <- NULL
                    out
                  },
                  tags = c('Tree-Based Model'),
                  sort = function(x) x)
