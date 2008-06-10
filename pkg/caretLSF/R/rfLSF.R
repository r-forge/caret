rfLSF <- function(x, y, workers = 10, control = lsf.ctrl(), ...)
  {
    library(Rlsf)
    library(randomForest)
    theCall <- match.call()

    lsfJobs <- vector(mode = "list", length = workers)

    cat("submitting jobs ...\n")
    
    for(i in 1:workers)
      {
        lsfJobs[[i]] <- lsf.submit2(
                                    func = randomForest,           
                                    x = x,
                                    y = y,
                                    ...,
                                    ctrl = control)
      }

    rfModels <- jobMonitor(lsfJobs) 


    realRF <- unlist(lapply(rfModels, function(x) class(x) == "randomForest"))


    if(all(!realRF)) stop("something is wrong: no good oibjects")
    if(any(!realRF)) rfModels <- rfModels[!realRF]
    

    # to avoid conflicts with other combine functions
    rfComb <- randomForest:::combine
    allRF <-  do.call(
                      "rfComb",
                      rfModels)

    allRF$call["x"] <- theCall["x"]
    allRF$call["y"] <- theCall["y"]

    
    allRF
  }
