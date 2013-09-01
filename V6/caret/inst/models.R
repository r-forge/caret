out <- list(library = NULL,
            loop = NULL,
            type = NULL,
            parameters = data.frame(parameter = ".parameter",
                                    class = NA,
                                    label = "parameter"),
            grid = NULL,
            fit = NULL,
            predict = NULL,
            prob = NULL,
            sort = NULL)


lm <- list(library = NULL,
        loop = NULL,
        type = "Regression",
        parameters = data.frame(parameter = ".parameter",
                                class = NULL,
                                label = "parameter"),
        grid = function(x, y, len = NULL, pp = NULL) data.frame(.parameter = "none"),
        fit = function(x, y, wts, param, lev, last, weights, ...) {
          dat <- x
          dat$.outcome <- y
          if(!is.null(wts))
          {
            out <- lm(.outcome ~ ., data = dat, weights = wts, ...)
          } else out <- lm(modFormula, data, ...)
          out
        },
        predict = function(modelFit, newdata, preProc = NULL, param = NULL) predict(modelFit, newdata),
        prob = NULL,
        sort = function(x) x)

glm <- list(library = NULL,
           loop = NULL,
           type = "Regression",
           parameters = data.frame(parameter = ".parameter",
                                   class = NULL,
                                   label = "parameter"),
           grid = function(x, y, len = NULL, pp = NULL) data.frame(.parameter = "none"),
           fit = function(x, y, wts, param, lev, last, weights, ...) {
             dat <- x
             dat$.outcome <- y
             
             theDots <- list(...)
             if(!any(names(theDots) == "family"))
             {
               theDots$family <- if(is.factor(y)) binomial() else gaussian()              
             }
             
             ## pass in any model weights
             if(!is.null(wts)) theDots$weights <- wts
             
             modelArgs <- c(list(formula = modFormula, data = dat), theDots)
             
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
