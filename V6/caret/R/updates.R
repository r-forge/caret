update.train <- function(object, param = NULL, ...)
  {
    ## check for original data
    if(is.null(object$trainingData)) stop("original training data is needed; use returnData = TRUE in trainControl()")
    
    ## check parameter names; what about custom?
    if(is.null(param)) return(object)
    if(is.list(param)) param <- as.data.frame(param)
    if(!is.data.frame(param)) stop("param should be a data frame or a named list")
    if(nrow(param) > 1) stop("only one set of parameters should be specified")

    paramNames <- as.character(marsTune$modelInfo$parameter$parameter)
    if(length(paramNames) != ncol(param))
      stop(paste("Parameters should be", paste(".", paramNames, sep = "", collapse = ", ")))
    if(any(sort(names(param)) != sort(paste(".", paramNames, sep = ""))))
      stop(paste("Parameters should be", paste(".", paramNames, sep = "", collapse = ", ")))
    
    ## get pre-processing options
    if(!is.null(object$preProcess))
      {
        ppOpt <- list(options = object$preProcess$method)
        if(length(object$control$preProcOptions) > 0) ppOpt <- c(ppOpt,object$control$preProcOptions)
      } else ppOpt <- NULL
    
    ## refit model with new parameters
    args <- list(data = object$trainingData, 
                 method = object$method, 
                 tuneValue = param, 
                 obsLevels = levels(object$trainingData$.outcome),
                 pp = ppOpt,
                 last = TRUE,
                 custom = object$control$custom$model)
    if(length(object$dots) > 0) args <- c(args, object$dots)
    finalFinalModel <- do.call("createModel", args)
    object$finalModel <- finalFinalModel$fit
    object$preProcess <- finalFinalModel$preProc
    object$bestTune <- param
    

    ## modify objects so print method reflects update
    object$update <- param

    ## return object
    object
  }
