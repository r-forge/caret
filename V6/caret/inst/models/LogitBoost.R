modelInfo <- list(library = "caTools",
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = 'nIter',
                                          class = 'numeric',
                                          label = '# Boosting Iterations'),
                  grid = function(x, y, len = NULL) data.frame(.nIter = (1:len)*10),
                  fit = function(x, y, wts, param, lev, last, weights, ...) {
                    ## There is another package with a function called `LogitBoost`
                    ## so we call using the namespace
                    caTools::LogitBoost(as.matrix(x), y, nIter = param$.nIter)
                  },
                  predict = function(modelFit, newdata, preProc = NULL, param = NULL) {
                    ## This model was fit with the maximum value of nIter
                    out <- caTools::predict.LogitBoost(modelFit, newdata, type="class")
                    ## param contains one of the elements of 'submodels'. In this 
                    ## case, 'submodels' is a data frame with the other values of
                    ## nIter. We loop over these to get the other predictions.
                    if(!is.null(param))
                    {                   
                      ## Save _all_ the predictions in a list
                      tmp <- out
                      out <- vector(mode = "list", length = nrow(param) + 1)
                      out[[1]] <- tmp
                      
                      for(j in seq(along = param$.nIter))
                      {
                        out[[j+1]] <- caTools::predict.LogitBoost(modelFit,
                                                                  newdata,
                                                                  nIter = param$.nIter[j])
                      }
                    }
                    out                   
                  },
                  prob = function(modelFit, newdata, preProc = NULL, param = NULL) {
                    out <- caTools::predict.LogitBoost(modelFit, newdata, type = "raw")
                    ## I've seen them not be on [0, 1]
                    out <- t(apply(out, 1, function(x) x/sum(x)))
                    if(!is.null(param))
                    {
                      tmp <- vector(mode = "list", length = nrow(param) + 1)
                      tmp[[1]] <- out
                      
                      for(j in seq(along = param$.nIter))
                      {                           
                        tmpProb <- caTools::predict.LogitBoost(modelFit,
                                                               newdata,
                                                               type = "raw",
                                                               nIter = param$.nIter[j])
                        tmpProb <- out <- t(apply(tmpProb, 1, function(x) x/sum(x)))
                        tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels])           
                      }
                      out <- tmp
                    }                       
                    out
                  },
                  sort = function(x) x)
