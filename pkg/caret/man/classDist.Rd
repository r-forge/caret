\name{classDist}
\Rdversion{1.1}
\alias{classDist.default}
\alias{classDist}
\alias{predict.classDist}
\title{
Compute and predict the distances to class centroids
}
\description{
This function computes the class centroids and covariance matrix for a training set for determining Mahalanobis distances of samples to each class centroid.
}
\usage{
classDist(x, ...)

\method{classDist}{default}(x, y, vars = NULL, ...)

\method{predict}{classDist}(object, newdata, trans = log, ...)

}
\arguments{
  \item{x}{a matrix or data frame of predictor variables}
  \item{y}{a factor vector of class labels}
  \item{vars}{An optional list of variables (by name or index) that 
              should be used in the distance calculation. \code{NULL}
              indicates that all variables should be used.}
  \item{newdata}{a matrix or data frame. If \code{vars} was 
                 previously specified, these columns should be in
                 \code{newdata}}
  \item{trans}{an optional function that can be applied to each class
               distance. \code{trans = NULL} will not apply a 
               function}
  \item{\dots}{optional arguments to pass (not currently used)}
}
\details{
The covariance matrix is calculated for each class.

The function will check for non-singular matrices.
}
\value{
for \code{classDist}, an object of class \code{classDist} with 
elements:
  \item{values }{a list with elements for each class. Each element 
                 contains a mean vector for the class centroid and the
                 inverse of the class covariance matrix}
  \item{classes}{a character vector of class labels}
  \item{vars}{the value passed to \code{vars}}
  \item{call}{the function call}
  \item{p}{the number of variables after filtering}
  \item{n}{a vector of samples sizes per class}

For \code{predict.classDist}, a matrix with columns for each class. The columns names are the names of the class with the prefix \code{dist.} 
}

\author{
Max Kuhn
}

\seealso{\code{\link[stats]{Mahalanobis}}}
\examples{
trainSet <- sample(1:150, 100)

distData <- classDist(iris[trainSet, 1:4], 
                      iris$Species[trainSet])

newDist <- predict(distData,
                   iris[-trainSet, 1:4])

splom(newDist, groups = iris$Species[-trainSet])
\keyword{ manip }

