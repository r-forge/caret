modelInfo <- list(library = "kernlab",
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('degree', 'scale'),
                                          class = c("numeric", "numeric"),
                                          label = c('Polynomial Degree', 'Scale')),
                  grid = function(x, y, len = NULL) {
                    expand.grid(.degree = seq(1, min(len, 3)),      
                                .scale = 10 ^((1:len) - 4))
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    lssvm(x = as.matrix(x), y = y,
                          kernel = polydot(degree = param$.degree,
                                           scale = param$.scale,
                                           offset = 1), ...)         
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {  
                    out <- predict(modelFit, as.matrix(newdata))
                    if(is.matrix(out)) out <- out[,1]
                    out
                  },
                  prob = NULL,
                  tags = c("Kernel Methods", "Support Vector Machines"),
                  sort = function(x) x)
