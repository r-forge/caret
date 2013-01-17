library(R2HTML)
library(caret)

setwd("~/Code/caret/html/")

startPath <- getwd()

## pull printOutput and others out to this file

###########################################################################

printOutput <- function(foo, wd = 70)
  {
    options(width = 70)
    tt <- capture.output(foo, file = NULL)

    tt <- paste("<pre>", tt, "</pre>", sep = "")
    cat(paste(tt, collpase = "\n"))
    invisible(tt)
  }

###########################################################################

dPath <- paste("html_",format(Sys.time(), "%Y_%m_%d_%H_%M"), sep = "")
dir.create(dPath)
                 
rnwFiles <- c("index", "datasets", "misc", "preprocess", "visualizations",
              "bytag", "featureSelection", "training", "other",
              "varImp", "parallel", "splitting")

rnwFiles <- paste(rnwFiles, ".Rnw", sep = "")
file.copy(rnwFiles, file.path(getwd(), dPath, rnwFiles))

file.copy(list.files(pattern = "png$"),
          file.path(getwd(), dPath, list.files(pattern = "png$")))
file.copy("TrainAlgo",
          file.path(getwd(), dPath, "TrainAlgo"))
file.copy("style.css",
          file.path(getwd(), dPath, "style.css"))
dir.create(file.path(dPath, "images"))
file.copy(list.files(path = file.path(getwd(), "images"), 
                     pattern = "^img", 
                     full.names = TRUE),
          file.path(getwd(), dPath, "images"))
file.copy("style.css",
          file.path(getwd(), dPath, "style.css"))
file.copy("parallel.pdf",
          file.path(getwd(), dPath, "parallel.pdf"))
setwd(file.path(getwd(), dPath))
pathName <- paste(file.path(getwd()), "/", sep = "")

###########################################################################

for(i in seq(along = rnwFiles))
  {
    cat("###########################################################################\n",
        "Sweaving", rnwFiles[i], "\n")
    Sweave(rnwFiles[i],
           driver = RweaveHTML,
           output = gsub(".Rnw", ".html", rnwFiles[i]))

  }


unlink(list.files(pattern = "Rnw$"))



