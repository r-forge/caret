### In this file, there are a lot of functions form caret that are
### references using the explicit namespace operator (:::). For some
### reason, with some parallel processing technologies and foreach,
### functions inside of caret cannot be found despite using the
### ".packages" argument and calling the caret package via library().


nominalTrainWorkflow2 <- function(x, y, wts, info, method, ppOpts, ctrl, lev, testing = FALSE, ...)
{
  suppressPackageStartupMessages(library(foreach))
  library(caret)
  loadNamespace("caret")
  ppp <- list(options = ppOpts)
  ppp <- c(ppp, ctrl$preProcOptions)
  
  printed <- format(info$loop, digits = 4)
  colnames(printed) <- gsub("^\\.", "", colnames(printed))
  
  ## For 632 estimator, add an element to the index of zeros to trick it into
  ## fitting and predicting the full data set.
  
  resampleIndex <- ctrl$index
  if(ctrl$method %in% c("boot632"))
  {
    resampleIndex <- c(list("AllData" = rep(0, nrow(dat))), resampleIndex)
    ctrl$indexOut <- c(list("AllData" = rep(0, nrow(dat))),  ctrl$indexOut)
  }
  `%op%` <- getOper(ctrl$allowParallel && getDoParWorkers() > 1)
  
  pkgs <- c("methods", "caret")
  if(!is.null(method$library)) pkgs <- c(pkgs, method$library)
  
  result <- foreach(iter = seq(along = resampleIndex), .combine = "c", .verbose = FALSE, .packages = pkgs, .errorhandling = "stop") %:%
    foreach(parm = 1:nrow(info$loop), .combine = "c", .verbose = FALSE, .packages = pkgs, .errorhandling = "stop")  %op%
{
  if(!(length(ctrl$seeds) == 1 && is.na(ctrl$seeds))) set.seed(ctrl$seeds[[iter]][parm])
  
  library(caret)
  if(ctrl$verboseIter) caret:::progress(printed[parm,,drop = FALSE],
                                        names(resampleIndex), iter)
  
  if(names(resampleIndex)[iter] != "AllData")
  {
    modelIndex <- resampleIndex[[iter]]
    holdoutIndex <- ctrl$indexOut[[iter]]
  } else {
    modelIndex <- 1:nrow(dat)
    holdoutIndex <- modelIndex
  }
  
  if(testing) cat("pre-model\n")
  
  mod <- try(
    caret:::createModel2(x = x[modelIndex,,drop = FALSE ],
                         y = y[modelIndex],
                         wts = wts[modelIndex],
                         method = method,
                         tuneValue = info$loop[parm,,drop = FALSE],
                         obsLevels = lev,
                         pp = ppp,
                         classProbs = ctrl$classProbs,
                         ...),
    silent = TRUE)
  
  if(class(mod)[1] != "try-error")
  {
    predicted <- try(
      caret:::predictionFunction2(method = method,
                                  modelFit = mod$fit,
                                  newdata = x[holdoutIndex,, drop = FALSE],
                                  preProc = mod$preProc,
                                  param = info$submodels[[parm]]),
      silent = TRUE)
    
    if(class(predicted)[1] == "try-error")
    {
      wrn <- paste(colnames(printed[parm,,drop = FALSE]),
                   printed[parm,,drop = FALSE],
                   sep = "=",
                   collapse = ", ")
      wrn <- paste("predictions failed for ", names(resampleIndex)[iter],
                   ": ", wrn, " ", as.character(predicted), sep = "")
      if(ctrl$verboseIter) cat(wrn, "\n")
      warning(wrn)
      rm(wrn)
      
      ## setup a dummy results with NA values for all predictions
      nPred <- length(holdoutIndex)
      if(!is.null(lev))
      {
        predicted <- rep("", nPred)
        predicted[seq(along = predicted)] <- NA
      } else {
        predicted <- rep(NA, nPred)
      }
      if(!is.null(info$submodels[[parm]]))
      {
        tmp <- predicted
        predicted <- vector(mode = "list", length = nrow(info$submodels[[parm]]) + 1)
        for(i in seq(along = predicted)) predicted[[i]] <- tmp
        rm(tmp)
      }
    }
  } else {
    wrn <- paste(colnames(printed[parm,,drop = FALSE]),
                 printed[parm,,drop = FALSE],
                 sep = "=",
                 collapse = ", ")
    wrn <- paste("model fit failed for ", names(resampleIndex)[iter],
                 ": ", wrn, " ", as.character(mod), sep = "")
    if(ctrl$verboseIter) cat(wrn, "\n")
    warning(wrn)
    rm(wrn)
    
    ## setup a dummy results with NA values for all predictions
    nPred <- length(holdoutIndex)
    if(!is.null(lev))
    {
      predicted <- rep("", nPred)
      predicted[seq(along = predicted)] <- NA
    } else {
      predicted <- rep(NA, nPred)
    }
    if(!is.null(info$submodels[[parm]]))
    {
      tmp <- predicted
      predicted <- vector(mode = "list", length = nrow(info$submodels[[parm]]) + 1)
      for(i in seq(along = predicted)) predicted[[i]] <- tmp
      rm(tmp)
    }
  }
  
  if(testing) print(head(predicted))
  if(ctrl$classProbs)
  {
    if(class(mod)[1] != "try-error")
    {
      probValues <- caret:::probFunction2(method = method,
                                          modelFit = mod$fit,
                                          newdata = x[holdoutIndex,, drop = FALSE],
                                          preProc = mod$preProc,
                                          param = info$submodels[[parm]])
    } else {
      probValues <- as.data.frame(matrix(NA, nrow = nPred, ncol = length(lev)))
      colnames(probValues) <- lev
      if(!is.null(info$submodels[[parm]]))
      {
        tmp <- probValues
        probValues <- vector(mode = "list", length = nrow(info$submodels[[parm]]) + 1)
        for(i in seq(along = probValues)) probValues[[i]] <- tmp
        rm(tmp)
      }
    }
    if(testing) print(head(probValues))
  }
  
  ##################################
  
  if(!is.null(info$submodels))
  {
    ## merge the fixed and seq parameter values together
    allParam <- caret:::expandParameters(info$loop[parm,,drop = FALSE], info$submodels[[parm]])
    allParam <- allParam[complete.cases(allParam),, drop = FALSE]
    
    #     ## For ctree, we had to repeat the first value
    #     if(method == "ctree") allParam <- allParam[!duplicated(allParam),, drop = FALSE]
    #     
    #     ## For glmnet, we fit all the lambdas but x$fixed has an NA
    #     if(method == "glmnet") allParam <- allParam[complete.cases(allParam),, drop = FALSE]
    
    ## collate the predicitons across all the sub-models
    predicted <- lapply(predicted,
                        function(x, y, lv)
                        {
                          if(!is.factor(x) & is.character(x)) x <- factor(as.character(x), levels = lv)
                          data.frame(pred = x, obs = y, stringsAsFactors = FALSE)
                        },
                        y = y[holdoutIndex],
                        lv = lev)
    if(testing) print(head(predicted))
    
    ## same for the class probabilities
    if(ctrl$classProbs)
    {
      for(k in seq(along = predicted)) predicted[[k]] <- cbind(predicted[[k]], probValues[[k]])
    }
    
    if(ctrl$savePredictions)
    {
      tmpPred <- predicted
      for(modIndex in seq(along = tmpPred))
      {
        tmpPred[[modIndex]]$rowIndex <- holdoutIndex
        tmpPred[[modIndex]] <- cbind(tmpPred[[modIndex]], allParam[modIndex,,drop = FALSE])
      }
      tmpPred <- rbind.fill(tmpPred)
      tmpPred$Resample <- names(resampleIndex)[iter]
    } else tmpPred <- NULL
    
    ## get the performance for this resample for each sub-model
    thisResample <- lapply(predicted,
                           ctrl$summaryFunction,
                           lev = lev,
                           model = method)
    if(testing) print(head(thisResample))
    ## for classification, add the cell counts
    if(length(lev) > 1)
    {
      cells <- lapply(predicted,
                      function(x) caret:::flatTable(x$pred, x$obs))
      for(ind in seq(along = cells)) thisResample[[ind]] <- c(thisResample[[ind]], cells[[ind]])
    }
    thisResample <- do.call("rbind", thisResample)          
    thisResample <- cbind(allParam, thisResample)
    
  } else {       
    if(is.factor(y)) predicted <- factor(as.character(predicted), levels = lev)
    tmp <-  data.frame(pred = predicted,
                       obs = y[holdoutIndex],
                       stringsAsFactors = FALSE)
    ## Sometimes the code above does not coerce the first
    ## columnn to be named "pred" so force it
    names(tmp)[1] <- "pred"
    if(ctrl$classProbs) tmp <- cbind(tmp, probValues)
    
    if(ctrl$savePredictions)
    {
      tmpPred <- tmp
      tmpPred$rowIndex <- holdoutIndex
      tmpPred <- cbind(tmpPred, info$loop[parm,,drop = FALSE])
      tmpPred$Resample <- names(resampleIndex)[iter]
    } else tmpPred <- NULL
    
    ##################################
    thisResample <- ctrl$summaryFunction(tmp,
                                         lev = lev,
                                         model = method)
    
    ## if classification, get the confusion matrix
    if(length(lev) > 1) thisResample <- c(thisResample, caret:::flatTable(tmp$pred, tmp$obs))
    thisResample <- as.data.frame(t(thisResample))
    thisResample <- cbind(thisResample, info$loop[parm,,drop = FALSE])
    
  }
  thisResample$Resample <- names(resampleIndex)[iter]
  
  if(ctrl$verboseIter) caret:::progress(printed[parm,,drop = FALSE],
                                        names(resampleIndex), iter, FALSE)
  list(resamples = thisResample, pred = tmpPred)
}
  
  resamples <- rbind.fill(result[names(result) == "resamples"])
  pred <- if(ctrl$savePredictions)  rbind.fill(result[names(result) == "pred"]) else NULL
  if(ctrl$method %in% c("boot632"))
  {
    perfNames <- names(ctrl$summaryFunction(data.frame(obs = dat$.outcome, pred = sample(dat$.outcome)),
                                            lev = lev,
                                            model = method))
    apparent <- subset(resamples, Resample == "AllData")
    apparent <- apparent[,!grepl("^\\.cell|Resample", colnames(apparent)),drop = FALSE]
    names(apparent)[which(names(apparent) %in% perfNames)] <- paste(names(apparent)[which(names(apparent) %in% perfNames)],
                                                                    "Apparent", sep = "")
    names(apparent) <- gsub("^\\.", "", names(apparent))
    if(any(!complete.cases(apparent[,!grepl("^cell|Resample", colnames(apparent)),drop = FALSE])))
    {
      warning("There were missing values in the apparent performance measures.")
    }        
    resamples <- subset(resamples, Resample != "AllData")
  }
  names(resamples) <- gsub("^\\.", "", names(resamples))
  
  if(any(!complete.cases(resamples[,!grepl("^cell|Resample", colnames(resamples)),drop = FALSE])))
  {
    warning("There were missing values in resampled performance measures.")
  }
  
  out <- ddply(resamples[,!grepl("^cell|Resample", colnames(resamples)),drop = FALSE],
               ## TODO check this for seq models
               gsub("^\\.", "", colnames(info$loop)),
               caret:::MeanSD, 
               exclude = gsub("^\\.", "", colnames(info$loop)))
  
  if(ctrl$method %in% c("boot632"))
  {
    out <- merge(out, apparent)
    for(p in seq(along = perfNames))
    {
      const <- 1-exp(-1)
      out[, perfNames[p]] <- (const * out[, perfNames[p]]) +  ((1-const) * out[, paste(perfNames[p],"Apparent", sep = "")])
    }
  }
  
  list(performance = out, resamples = resamples, predictions = if(ctrl$savePredictions) pred else NULL)
}



looTrainWorkflow2 <- function(x, y, wts, info, method, ppOpts, ctrl, lev, testing = FALSE, ...)
{
  suppressPackageStartupMessages(library(foreach))
  library(caret)
  loadNamespace("caret")
  
  ppp <- list(options = ppOpts)
  ppp <- c(ppp, ctrl$preProcOptions)
  
  printed <- format(info$loop)
  colnames(printed) <- gsub("^\\.", "", colnames(printed))
  
  `%op%` <- getOper(ctrl$allowParallel && getDoParWorkers() > 1)
  
  pkgs <- c("methods", "caret")
  if(!is.null(method$library)) pkgs <- c(pkgs, method$library)
  
  result <- foreach(iter = seq(along = ctrl$index), .combine = "rbind", .verbose = FALSE, .packages = pkgs, .errorhandling = "stop") %:%
    foreach(parm = 1:nrow(info$loop), .combine = "rbind", .verbose = FALSE, .packages = pkgs, .errorhandling = "stop") %op%
{
  
  if(!(length(ctrl$seeds) == 1 && is.na(ctrl$seeds))) set.seed(ctrl$seeds[[iter]][parm])
  if(testing) cat("after loops\n")
  library(caret)
  if(ctrl$verboseIter) caret:::progress(printed[parm,,drop = FALSE],
                                        names(ctrl$index), iter, TRUE)
  
  mod <- caret:::createModel2(x = x[ctrl$index[[iter]],,drop = FALSE ],
                              y = y[ctrl$index[[iter]] ],
                              wts = wts[ctrl$index[[iter]] ],
                              method = method,
                              tuneValue = info$loop[parm,,drop = FALSE],
                              obsLevels = lev,
                              pp = ppp,
                              classProbs = ctrl$classProbs,
                              ...)
  
  holdoutIndex <- -unique(ctrl$index[[iter]])
  
  predicted <- caret:::predictionFunction2(method = method,
                                           modelFit = mod$fit,
                                           newdata = x[-ctrl$index[[iter]],, drop = FALSE],
                                           preProc = mod$preProc,
                                           param = info$submodels[[parm]])
  
  if(testing) print(head(predicted))
  if(ctrl$classProbs)
  {
    probValues <- caret:::probFunction(method = method,
                                       modelFit = mod$fit,
                                       newdata = x[holdoutIndex,, drop = FALSE],
                                       preProc = mod$preProc,
                                       param = info$submodels[[parm]])
    if(testing) print(head(probValues))
  }
  
  ##################################
  
  if(!is.null(info$submodels))
  {
    ## collate the predictions across all the sub-models
    predicted <- lapply(predicted,
                        function(x, y, lv)
                        {
                          if(!is.factor(x) & is.character(x)) x <- factor(as.character(x), levels = lv)
                          data.frame(pred = x, obs = y, stringsAsFactors = FALSE)
                        },
                        y = y[holdoutIndex],
                        lv = lev)
    if(testing) print(head(predicted))
    
    ## same for the class probabilities
    if(ctrl$classProbs)
    {
      for(k in seq(along = predicted)) predicted[[k]] <- cbind(predicted[[k]], probValues[[k]])
    }
    predicted <- do.call("rbind", predicted)
    allParam <- caret:::expandParameters(info$loop, info$submodels[[parm]])
    predicted <- cbind(predicted, allParam)
    ## if saveDetails then save and export 'predicted'
  } else {
    
    if(is.factor(y)) predicted <- factor(as.character(predicted),
                                         levels = lev)
    predicted <-  data.frame(pred = predicted,
                             obs = y[holdoutIndex],
                             stringsAsFactors = FALSE)
    if(ctrl$classProbs) predicted <- cbind(predicted, probValues)
    predicted <- cbind(predicted, info$loop[parm,,drop = FALSE])
  }
  if(ctrl$verboseIter) caret:::progress(printed[parm,,drop = FALSE],
                                        names(ctrl$index), iter, FALSE)
  predicted
}
  
  names(result) <- gsub("^\\.", "", names(result))
  out <- ddply(result,
               as.character(method$parameter$parameter),
               ctrl$summaryFunction,
               lev = lev,
               model = method)
  list(performance = out, predictions = result)
}


oobTrainWorkflow2 <- function(x, y, wts, info, method, ppOpts, ctrl, lev, testing = FALSE, ...)
{
  library(caret)
  ppp <- list(options = ppOpts)
  ppp <- c(ppp, ctrl$preProcOptions)
  printed <- format(info$loop)
  colnames(printed) <- gsub("^\\.", "", colnames(printed))
  `%op%` <- getOper(ctrl$allowParallel && getDoParWorkers() > 1)
  result <- foreach(parm = 1:nrow(info$loop), .packages = c("methods", "caret"), .combine = "rbind") %op%
{
  library(caret)
  if(ctrl$verboseIter) caret:::progress(printed[parm,,drop = FALSE], "", 1, TRUE)
  
  mod <- caret:::createModel2(x = x,
                              y = y,
                              wts = wts,
                              method = method,
                              tuneValue = info$loop[parm,,drop = FALSE],
                              obsLevels = lev,
                              pp = ppp,
                              classProbs = ctrl$classProbs,
                              ...)

  out <- switch(class(mod$fit)[1],
                randomForest = caret:::rfStats(mod$fit),
                RandomForest = caret:::cforestStats(mod$fit),
                bagEarth =, bagFDA = caret:::bagEarthStats(mod$fit),
                regbagg =, classbagg = caret:::ipredStats(mod$fit))
  if(ctrl$verboseIter) caret:::progress(printed[parm,,drop = FALSE], "", 1, FALSE)
  
  cbind(as.data.frame(t(out)), info$loop[parm,,drop = FALSE])
}
  names(result) <- gsub("^\\.", "", names(result))
  result
}

