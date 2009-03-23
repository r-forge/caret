\name{rfeControl}
\alias{rfeControl}
\title{Controlling the Feature Selection Algorithms}
\description{
This function generates a control object that can be used to specify the details of the feature selection algorithms used in this package.
}
\usage{
rfeControl(
           functions = NULL,
           metric = NULL, 
           rerank = FALSE,
           method = "boot",
           saveDetails = FALSE,
           number = ifelse(method == "cv", 10, 25),
           verbose = TRUE,
           returnResamp = "all",
           p = .75,
           index = NULL)
}
\arguments{
  \item{functions}{a list of functions for model fitting, prediction and variable importance (see Details below)}
  \item{metric}{a string that specifies what summary metric will be used to select the optimal model. Possible values are "RMSE" and "Rsquared" for 
     regression and "Accuracy" and "Kappa" for classification.} 
  \item{rerank}{a logical: should variable importance be re-calculated each time features are removed? }
  \item{method}{The external resampling method: \code{boot}, \code{cv},
    \code{LOOCV} or  \code{LGOCV} (for repeated training/test splits}
  \item{number}{Either the number of folds or number of resampling iterations}
  \item{saveDetails}{a logical to save the predictions and variable importances from the selection process}
  \item{verbose}{a logical to print a log for each external resampling iteration}
  \item{returnResamp}{A character string indicating how much of the resampled summary metrics should be saved. Values can be ``final'', ``all'' or ``none''}
  \item{p}{For leave-group out cross-validation: the training percentage}
  \item{index}{a list with elements for each external resampling iteration. Each list element is the sample rows used for training at that iteration.}
}
\details{
add bit about the functions
}
\value{
describe the output object
}
\references{Guyon, am + mc }
\author{Max Kuhn }

\seealso{ \code{\link{rfe}} }
\examples{
# a dont run example
}

\keyword{ utilities }

