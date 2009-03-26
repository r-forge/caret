classDist <- function (x, ...)  UseMethod("classDist")

classDist.default <- function(x, y, vars = NULL, ...)
{
  if(!is.factor(y)) stop("y should be a factor")
  if(!is.null(vars))
    {
      if(is.numeric(vars)) x <- x[, vars, drop = FALSE]
      if(is.character(vars)) x <- x[, colnames(x) %in% vars, drop = FALSE]
    }
  p <- ncol(x)
  x <- split(x, y)
  getStats <- function(u)
    {
      if(nrow(u) < ncol(u))
        stop("there must be more rows than columns for this class")
      A <- try(cov(u), silent = TRUE)
      if(class(A) == "try-error")
        stop("Cannot compute the covariance matrix")
      A <- try(solve(A), silent = TRUE)
      if(class(A) == "try-error")
        stop("Cannot invert the covariance matrix")
      list(means = mean(u, na.rm = TRUE),
           A = A)
    }
  structure(
            list(values = lapply(x, getStats),
                 classes = levels(y),
                 vars = vars,
                 call = match.call(),
                 p = p,
                 n = unlist(lapply(x, nrow))),
            class = "classDist")
}

print.classDist <- function(x, ...)
  {
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    cat("# variables", x$p, "\n")
    cat("# samples:",
        paste(
              paste(x$n, " (", names(x$n), ")", sep = ""),
              collapse = ", "),
        "\n")
    invisible(x)
  }

predict.classDist <- function(object, newdata, trans = log, ...)
{
  if(!is.null(object$vars))
    {
      if(is.numeric(object$vars)) newdata <- newdata[, object$vars, drop = FALSE]
      if(is.character(object$vars)) newdata <- newdata[, colnames(newdata) %in% object$vars, drop = FALSE]
    }
  pred <- function(a, x) mahalanobis(x, center = a$means, cov = a$A, inverted = TRUE)

  out <- lapply(object$values, pred, x = newdata)
  out <- do.call("cbind", out)
  colnames(out) <- paste("dist.", colnames(out), sep = "")
  if(!is.null(trans)) out <- apply(out, 2, trans)
  out
}
