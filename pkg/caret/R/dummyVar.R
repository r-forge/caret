## A copy of .Diag from the stats library follows.
## Why? I was referencing an internal funciton from the stats library 
## and CRAN changed the policy on this. I had just cleaned up almost
## two dozen changes caused by new CRAN policies *and* had just removed
## 54 package dependencies for caret when I submitted version 6.0-18. 
## Here is the level of nastiness that sometimes occurs when one specific
## person gets your update. 
##
## On 30/12/2013 19:49, Kuhn, Max wrote:
## MK > This is a substantial update.
## 
## MK > There is one note below which violates the updated maintainer policies.
## MK > I'm not sure how to handle since it is the secret sauce for the contrast
## MK > function that I have. Rather than duplicating code in this package, I'd
## MK > like to take some time to find a better solution.
## 
## Brian D. Ripley > Please resubmit when you have done so.
## 
## MK > Thanks and happy New Year,
## 
## Brian D. Ripley > It would be happier not having to waste time on people 
## Brian D. Ripley > pushing their own interests like this!
  
BDRISAH <- function(nms, sparse) {
  ## no error checking here
  n <- as.integer(length(nms))
  d <- c(n,n)
  dn <- list(nms, nms)
  if(sparse) {
    if(is.null(tryCatch(loadNamespace("Matrix"), error = function(e)NULL)))
      stop(gettextf("%s needs package 'Matrix' correctly installed",
                    "contr*(.., sparse=TRUE)"),
           domain = NA)
    new("ddiMatrix", diag = "U", Dim = d, Dimnames = dn)
  } else
    array(c(rep.int(c(1, numeric(n)), n-1L), 1), d, dn)
}

contr.ltfr <- function (n, contrasts = TRUE, sparse = FALSE) 
{
  if (is.numeric(n) && length(n) == 1L) {
    if (n > 1L) 
      levels <- as.character(seq_len(n))
    else stop("not enough degrees of freedom to define contrasts")
  }
  else {
    levels <- as.character(n)
    n <- length(n)
  }
  contr <- BDRISAH(levels, sparse = sparse)
  if (contrasts) {
    if (n < 2L) stop(gettextf("contrasts not defined for %d degrees of freedom", n - 1L), domain = NA)
  }
  contr
}

contr.dummy <- function(n, ...)
  {
    if (is.numeric(n) && length(n) == 1L) {
      if (n > 1L) 
        levels <- as.character(seq_len(n))
      else stop("not enough degrees of freedom to define contrasts")
    }
    else {
      levels <- as.character(n)
      n <- length(n)
    }
    out <- diag(n)
    rownames(out) <- levels
    colnames(out) <- levels
    out
  }


"dummyVars" <-
  function(formula, ...){
    UseMethod("dummyVars")
  }
dummyVars.default <- function (formula, data, sep = ".", levelsOnly = FALSE, fullRank = FALSE, ...) 
{
  formula <- as.formula(formula)
  if(!is.data.frame(data)) data <- as.data.frame(data)

  vars <- all.vars(formula)
  if(any(vars == "."))
    {
      vars <- vars[vars != "."]
      vars <- unique(c(vars, colnames(data)))
    }
  isFac <- unlist(lapply(data[,vars,drop = FALSE], is.factor))
  if(sum(isFac) > 0)
    {
      facVars <- vars[isFac] 
      lvls <- lapply(data[,facVars,drop = FALSE], levels)
      if(levelsOnly)
      {
        tabs <- table(unlist(lvls))
        if(any(tabs > 1))
        {
          stop(paste("You requested `levelsOnly = TRUE` but",
                     "the following levels are not unique",
                     "across predictors:",
                     paste(names(tabs)[tabs > 1], collapse = ", ")))
        }
      }
    } else {
      facVars <- NULL
      lvls <- NULL
    }
  trms <- attr(model.frame(formula, data), "terms")
  out <- list(call = match.call(),
              form = formula,
              vars = vars,
              facVars = facVars,
              lvls = lvls,
              sep = sep,
              terms = trms,
              levelsOnly = levelsOnly,
              fullRank = fullRank)
  class(out) <- "dummyVars"
  out

}


print.dummyVars <- function(x, ...)
  {
    cat("Dummy Variable Object\n\n")
    cat("Formula: ")
    print(x$form)
    cat(length(x$vars),  " variables, ", length(x$facVars), " factors\n", sep = "")
    if(!is.null(x$sep) & !x$levelsOnly) cat("Variables and levels will be separated by '",
                                            x$sep, "'\n", sep = "")
    if(x$levelsOnly) cat("Factor variable names will be removed\n")
    if(x$fullRank) cat("A full rank encoding is used") else cat("A less than full rank encoding is used") 
    cat("\n")
    invisible(x)
  }


predict.dummyVars <- function(object, newdata, na.action = na.pass, ...)
  {
    if(is.null(newdata)) stop("newdata must be supplied")
    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
    if(!all(object$vars %in% names(newdata))) stop(
                                                   paste("Variable(s)",
                                                         paste("'", object$vars[object$vars %in% names(newdata)],
                                                               "'", sep = "",
                                                               collapse = ", "),
                                                         "are not in newdata"))
    Terms <- object$terms
    Terms <- delete.response(Terms)
    if(!object$fullRank)
    {
      oldContr <- options("contrasts")$contrasts
      newContr <- oldContr
      newContr["unordered"] <- "contr.ltfr"
      options(contrasts = newContr)
    }
    m <- model.frame(Terms, newdata, na.action = na.action, xlev = object$xlevels)

    x <- model.matrix(Terms, m)
    if(!object$fullRank) options(contrasts = oldContr)

    if(object$levelsOnly)
      {
        for(i in object$facVars)
          {
            colnames(x) <- gsub(paste("^", i, sep = ""), "", colnames(x))
            colnames(x) <- gsub(paste(":", i, sep = ""), ":", colnames(x), fixed = TRUE)
          }
      }
    if(!is.null(object$sep) & !object$levelsOnly)
      {
        for(i in object$facVars)
          {
            colnames(x) <- gsub(paste("^", i, sep = ""), paste(i, object$sep, sep = ""), colnames(x))
            colnames(x) <- gsub(paste(":", i, sep = ""), paste(":", i, object$sep, sep = ""), colnames(x), fixed = TRUE)
          }
      }  
    x[, colnames(x) != "(Intercept)", drop = FALSE]
  }


