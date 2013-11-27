setwd("/Users/kuhna03/Code/caret/V6/RegressionTests")

##############################################################

Old <- "5.17-7"
New <- "6.00-5"

oldResults <- list.files(file.path(getwd(), Old), pattern = "RData")
newResults <- list.files(file.path(getwd(), New), pattern = "RData")

oldOrphan <- oldResults[!(oldResults %in% newResults)]
newOrphan <- newResults[!(newResults %in% oldResults)]

common <- intersect(oldResults, newResults)

checkModels <- function(model, opath, npath){
  for(i in model) {
    thisMod <- gsub(".RData", "", i, fixed = TRUE)
    rlt <- checkResults(i,opath, npath)  
    cat("############################################################\n")
    cat(thisMod, "\n")
    if(is.null(rlt)) cat("no results!\n") else print(rlt)
    cat("\n")
    rm(rlt)
  }
}

checkResults <- function(model, opath, npath){
  nontest <- c("model", "opath", "npath", "nontest")
  load(file.path(getwd(), opath, model))
  oldObj <- ls()
  oldObj <- oldObj[!(oldObj %in% nontest)]
  oldResults <- vector(mode = "list", length = length(oldObj))
  names(oldResults) <- oldObj
  for(i in seq(along = oldObj)) oldResults[[i]] <- get(oldObj[i])
  rm(list = oldObj)
  
  load(file.path(getwd(), npath, model))
  newObj <- ls()
  newObj <- newObj[!(newObj %in% nontest)] 
  newResults <- vector(mode = "list", length = length(newObj))
  names(newResults) <- newObj  
  for(i in seq(along = newObj)) newResults[[i]] <- get(newObj[i])
  
  commonObj <- grep("test_", intersect(names(newResults), names(oldResults)), 
                    fixed = TRUE, value = TRUE)
  testResults <- vector(mode = "list", length = length(commonObj))
  names(commonObj) <- commonObj
  for(i in commonObj) {
    
    if(class(newResults[[i]])[1] == "train") {
      testResults[[i]] <- all.equal(newResults[[i]]$results, 
                                    oldResults[[i]]$results,
                                    tolerance = 0.001)
    } else testResults[[i]] <- all.equal(newResults[[i]], 
                                         oldResults[[i]],
                                         tolerance = 0.001)
  }
  testResults <- testResults[testResults != ""]
  testResults <- testResults[!unlist(lapply(testResults, is.null))]
  invisible(testResults)
}

checkModels(common, Old, New)
