suppressMessages(library(caret))

assert <- function(assertion, ..., fail=FALSE, file=stderr()) {
  s <- deparse(substitute(assertion), width.cutoff=500L)
  if (! assertion) {
    if (length(list(...)) > 0) {
      msg <- sprintf('assertion failure: %s: %s', s, sprintf(...))
    } else {
      msg <- sprintf('assertion failure: %s')
    }

    if (fail) {
      stop(msg, call.=FALSE)
    } else {
      for (fobj in file)
        cat(paste0(msg, '\n'), file=fobj)
    }
  }
  assertion
}

runUnitTests <- function(x, y, f) {
  legalTypes <- c('Classification', 'Regression')
  loopFormals <- c('grid')
  gridFormals <- c('x', 'y', 'len')
  fitFormals <- c('x', 'y', 'wts', 'param', 'lev', 'last', 'classProbs', '...')
  predictFormals <- c('modelFit', 'newdata', 'submodels')
  probFormals <- c('modelFit', 'newdata', 'submodels')
  # varImpFormals <- c('object', 'estimate', '...')
  sortFormals <- c('x')

  models <- getModelInfo()
  assert(! is.null(models), 'unable to load models', fail=TRUE)

  log <- file('testrun.log', 'w')
  files <- list(log, stderr())
  cat(sprintf('Date: %s\n\n', date()), file=log)

  for (i in seq(along=models)) {
    modelName <- names(models)[[i]]
    cat(sprintf('testing %s...\n', modelName))
    m <- models[[i]]
    tryCatch({
      for(i in seq(along = m$library)) {
        assert(require(m$library[i], character.only=TRUE, quietly=TRUE),
               'unable to load package %s', m$library[i], file=files)
      }

      assert(is.character(m$type) && length(m$type) %in% c(1, 2) &&
             all(m$type %in% legalTypes),
             "type is set incorrectly",
             file=files)
      assert(is.null(m$loop) || is.function(m$loop),
             "loop isn't NULL or a function",
             file=files)
      assert(! is.function(m$loop) || all(names(formals(m$loop)) == loopFormals),
             "loop has incorrect arguments",
             file=files)
      assert(is.data.frame(m$parameters),
             "parameters isn't a data frame",
             file=files)
      assert(is.function(m$grid),
             "grid isn't a function",
             file=files)
      assert(! is.function(m$grid) || all(names(formals(m$grid)) == gridFormals),
             "grid has incorrect arguments",
             file=files)
      assert(is.function(m$fit),
             "fit isn't a function",
             file=files)
      assert(! is.function(m$fit) || all(names(formals(m$fit)) == fitFormals),
             "fit has incorrect arguments",
             file=files)
      assert(is.function(m$predict),
             "predict isn't a function",
             file=files)
      assert(! is.function(m$predict) || all(names(formals(m$predict)) == predictFormals),
             "predict has incorrect arguments",
             file=files)
      assert(is.null(m$prob) || is.function(m$prob),
             "prob isn't NULL or a function",
             file=files)
      assert(! is.function(m$prob) || all(names(formals(m$prob)) == probFormals),
             "prob has incorrect arguments",
             file=files)
      assert(is.null(m$varImp) || is.function(m$varImp),
             "varImp isn't NULL or a function",
             file=files)
      fargs <- names(formals(m$varImp))
      fargc <- length(fargs)
      assert(! is.function(m$varImp) || (fargs[1] == 'object' && fargs[fargc] == '...'),
             "varImp has incorrect arguments",
             file=files)
      assert(is.character(m$tag),
             "tag is not character type",
             file=files)
      assert(is.function(m$sort),
             "sort isn't a function",
             file=files)
      assert(! is.function(m$sort) || all(names(formals(m$sort)) == sortFormals),
             "sort has incorrect arguments",
             file=files)
    },
    error=function(e) {
      cat(paste0('error: ', conditionMessage(e), '\n'), file=stderr())
      cat(sprintf('error testing model %s: continuing to next model\n', modelName))
      cat(sprintf('error: %s: %s\n', modelName, conditionMessage(e)), file=log)
    })
  }

  cat('\nSession info:\n\n', file=log)
  sink(log)
  print(sessionInfo())
  sink()
  close(log)
}

options(warn=1)
training <- twoClassSim(100)
testing <- twoClassSim(500)
trainX <- training[, -ncol(training)]
trainY <- training$Class

runUnitTests(trainX, trainY)
