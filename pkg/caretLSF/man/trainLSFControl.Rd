\name{trainLSFControl}
\alias{trainLSFControl}
\title{Control parameters for train}
\description{Control of printing and resampling for \code{\link{trainLSF}}}
\usage{
trainLSFControl(
                method = "boot", 
                number = ifelse(method == "cv", 10, 25), 
                verboseIter = TRUE, 
                returnData = TRUE, 
                p = 0.5,
                selectionFunction = "best",
                index = NULL,
                numWorkers = 5,
                buffer = 20,
                pause = 10,   
                lsf = lsf.ctrl())
}
\arguments{
  \item{method}{The resampling method: \code{boot}, \code{cv},
    \code{LOOCV},  \code{LGOCV} (for repeated training/test splits), or
    \code{oob} (only for random forest, bagged trees, bagged earth, bagged flexible discriminant analysis, or conditional tree forest models)}
  \item{number}{Either the number of folds or number of resampling iterations}
  \item{verboseIter}{A logical for printing a training log.}
  \item{returnData}{A logical for saving the data}
  \item{p}{For leave-group out cross-validation: the training percentage}
  \item{selectionFunction}{the function used to select the optimal tuning parameter. This can be a name of the function or the funciton itself. See \code{\link[caret]{best}} for details and other options.}
  \item{index}{a list with elements for each resampling iteration. Each
    list element is the sample rows used for training at that
    iteration.}
  \item{numWorkers}{the number of nodes that the job should be split
    across}
  \item{buffer}{a multiplier for the job kill threshold: how many times slower than the slowest job does a task have to be before it is stopped}    
  \item{pause}{number of seconds between checking the current jobs}    

  \item{lsf}{a list of optional control parameters for submitting
    jobs. See \code{\link[Rlsf]{lsf.ctrl}} for more details}
  
}

\value{
 An echo of the parameters specified
}
\author{max Kuhn}

\keyword{utilities}
