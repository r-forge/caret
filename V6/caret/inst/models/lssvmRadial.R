modelInfo <- list(library = "kernlab",
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('sigma'),
                                          class = c("numeric"),
                                          label = c('Sigma')),
                  grid = function(x, y, len = NULL) {
                    library(kernlab)
                    sigmas <- sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)  
                    expand.grid(.sigma = mean(sigmas[-2]))
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    lssvm(x = as.matrix(x), y = y,
                          kernel = rbfdot,
                          kpar = list(sigma = param$.sigma), ...)         
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {  
                    out <- predict(modelFit, as.matrix(newdata))
                    if(is.matrix(out)) out <- out[,1]
                    out
                  },
                  prob = NULL,
                  tags = c("Kernel Methods", "Support Vector Machines"),
                  sort = function(x) x)
