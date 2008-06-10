\name{rfLSF}
\alias{rfLSF}

\title{Parallel RandomForest}
\description{
RandomForest model building in parallel using LSF
}
\usage{
rfLSF(x, y, workers = 10, control = lsf.ctrl(), ...)
}

\arguments{
  \item{x}{a data frame or a matrix of predictor}
  \item{y}{ A response vector. If a factor, classification is assumed, otherwise regression is assumed. If omitted, \code{\link[randomForest]{randomForest}} will run in unsupervised mode.}
  \item{workers}{the number of compute nodes. Note that the value of \code{ntree} will be used for each compute node. }
  \item{control}{an optional control object for starting parallel jobs}
  \item{\dots}{ various options to pass to \code{\link[randomForest]{randomForest.default}} }
}
\details{
The function calls \code{\link[randomForest]{randomForest.default}} on several nodes and uses \code{\link[randomForest]{combine}} put all of the models back together. 

Note that the call of the output object will mirror the values of \code{x} and \code{y} passed to \code{rfLSF}.
}
\value{
a \code{\link[randomForest]{randomForest}} object
}

\author{Max Kuhn }

\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
\dontrun{
  ## Classification:
  ##data(iris)
  set.seed(71)
  iris.rf <- rfLSF(
                   iris[, 1:4], 
                   iris$Species, 
                   importance=TRUE,
                   proximity=TRUE)

}
}

\keyword{ models }

