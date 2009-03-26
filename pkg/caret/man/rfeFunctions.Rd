\name{rfe}
\alias{caretFuncs}
\alias{lmFuncs}
\alias{rfFuncs}
\alias{treebagFuncs}
\alias{ldaFuncs}
\alias{nbFuncs}
\alias{pickSizeBest}
\alias{pickSizeTolerance}
\alias{pickVars}

\title{Backwards Feature Selection Helper Functions}
\description{
Ancillary fuctions for backwards selection
}
\usage{
pickSizeTolerance(x, metric, tol = 1.5, maximize) 
pickSizeBest(x, metric, maximize) 

pickVars(y, size) 

caretFuncs
lmFuncs
rfFuncs
treebagFuncs
ldaFuncs
nbFuncs
}

\arguments{
  \item{x}{a matrix or data frame with the performance metric of interest}
  \item{metric}{a character string with the name of the performance metric that
                should be used to choose the appropriate number of variables }
  \item{maximize}{a logical; should the metric be maximized? }
  \item{tol}{a scalar to denote the acceptable difference in optimal performance 
             (see Details below) }
  \item{size}{an integer for the number of variables to retain}
}
\details{
This page describes the functions that are used in backwards selection (aka recursive 
feature elimination). The funcitons described here are passed to the algorithm via the 
\code{functions} argument of \code{\link{rfeControl}}. The necessary funcitons are:

\itemize{
   \item \code{fit}: a function with arguments \code{x} (the predictors), \code{y} (the 
          response),  \code{first} (a logical indicating that all possible predictors are in 
          \code{x}) and \code{...} (arguments to pass to the fit function). This function
          should return a model object
   \item \code{pred}: a function with arguments \code{object} (the model fit object from the
         \code{fit} function) and \code{x} (variables for prediction). The output should be
         a numeric or factor vector.
   \item \code{rank}:  a function with arguments \code{object} (the model fit object from the
         \code{fit} function), \code{x} (the current predictors from the training set) and
         \code{y} (the response from the training data). This function should return a single
         numeric vector (???) with a value corresponding to each variable. Higher values should
         denote more important variables.
   \item \code{selectVar}:   a function with arguments \code{x} (for a matrix with columns for 
          the performance metrics and the number of variables, called "\code{Variables}"), 
          \code{metric} (a character string of the performance measure to optimize) and \code{maximize} 
          (a logical for whether the metric should be maximized). The retrun value should be a 
          single integer corresponding to the subset size that is optimal. Examples included 
          in this package are \code{pickSizeBest} and \code{pickSizeTolerance}
   \item \code{selectSize}: inputs are \code{y} (a list of the outputs of the \code{rank} function)
         and \code{size} (the number of variables to select). The return value should be a character
         string of variable names of length \code{size}. XXXX Does this work with interactions or other terms?
    }
}

\author{Max Kuhn}

\seealso{\code{\link{rfeControl}}, \code{\link{rfe}}}

\keyword{models}
