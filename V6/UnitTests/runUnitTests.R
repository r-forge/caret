suppressMessages(library(caret))

local({
  log <- function(fmt, ..., files=list(stderr())) {
    msg <- sprintf(fmt, ...)
    for (fobj in files)
      cat(paste0(msg, '\n'), file=fobj)
  }

  assert <- function(assertion, ..., fail=FALSE, files=list(stderr())) {
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
        for (fobj in files)
          cat(paste0(msg, '\n'), file=fobj)
      }
    }
    assertion
  }

  runUnitTests <- function(models, x, y, f) {
    pkgs <- search()
    legalTypes <- c('Classification', 'Regression')
    loopFormals <- c('grid')
    gridFormals <- c('x', 'y', 'len')
    fitFormals <- c('x', 'y', 'wts', 'param', 'lev', 'last', 'classProbs', '...')
    predictFormals <- c('modelFit', 'newdata', 'submodels')
    probFormals <- c('modelFit', 'newdata', 'submodels')
    # varImpFormals <- c('object', 'estimate', '...')
    sortFormals <- c('x')

    logfile <- file('testrun.log', 'w')
    files <- list(logfile, stderr())
    cat(sprintf('Date: %s\n\n', date()), file=logfile)

    for (i in seq(along=models)) {
      modelName <- names(models)[[i]]
      log('\nTesting %s...', modelName, files=files)
      m <- models[[i]]
      tryCatch({
        withCallingHandlers({
          for(i in seq(along = m$library)) {
            assert(require(m$library[i], character.only=TRUE, quietly=TRUE),
                   'unable to load package %s', m$library[i], files=files)
          }

          assert(is.character(m$type) && length(m$type) %in% c(1, 2) &&
                 all(m$type %in% legalTypes),
                 "type is set incorrectly",
                 files=files)
          assert(is.null(m$loop) || is.function(m$loop),
                 "loop isn't NULL or a function",
                 files=files)
          assert(! is.function(m$loop) || all(names(formals(m$loop)) == loopFormals),
                 "loop has incorrect arguments",
                 files=files)
          assert(is.data.frame(m$parameters),
                 "parameters isn't a data frame",
                 files=files)
          assert(is.function(m$grid),
                 "grid isn't a function",
                 files=files)
          assert(! is.function(m$grid) || all(names(formals(m$grid)) == gridFormals),
                 "grid has incorrect arguments",
                 files=files)
          assert(is.function(m$fit),
                 "fit isn't a function",
                 files=files)
          assert(! is.function(m$fit) || all(names(formals(m$fit)) == fitFormals),
                 "fit has incorrect arguments",
                 files=files)
          assert(is.function(m$predict),
                 "predict isn't a function",
                 files=files)
          assert(! is.function(m$predict) ||
                 all(names(formals(m$predict)) == predictFormals),
                 "predict has incorrect arguments",
                 files=files)
          assert(is.null(m$prob) || is.function(m$prob),
                 "prob isn't NULL or a function",
                 files=files)
          assert(! is.function(m$prob) || all(names(formals(m$prob)) == probFormals),
                 "prob has incorrect arguments",
                 files=files)
          assert(is.null(m$varImp) || is.function(m$varImp),
                 "varImp isn't NULL or a function",
                 files=files)
          fargs <- names(formals(m$varImp))
          fargc <- length(fargs)
          assert(! is.function(m$varImp) ||
                 (fargs[1] == 'object' && fargs[fargc] == '...'),
                 "varImp has incorrect arguments",
                 files=files)
          assert(is.character(m$tag),
                 "tag is not character type",
                 files=files)
          assert(is.function(m$sort),
                 "sort isn't a function",
                 files=files)
          assert(! is.function(m$sort) || all(names(formals(m$sort)) == sortFormals),
                 "sort has incorrect arguments",
                 files=files)

          tuneGrid <- m$grid(x, y, 3)
          assert(is.data.frame(tuneGrid))

          if ("Classification" %in% m$type) {
            wts <- rep(1L, nrow(x))  # also try NULL?
            tuneValue <- tuneGrid[1,,drop=FALSE]
            obsLevels <- levels(y)
            log("Calling fit function for %s", modelName, files=files)
            modelFit <- m$fit(x, y, wts=wts, param=tuneValue,
                           lev=obsLevels, last=FALSE,
                           classProbs=is.function(m$prob))
            print(class(modelFit))
            assert(! is.null(modelFit),
                   "fit function returned a NULL",
                   fail=TRUE,
                   files=files)

            if (!isS4(modelFit)) {
              modelFit$vNames <- colnames(x)
              modelFit$problemType <- "Classification"
              modelFit$tuneValue <- tuneValue
              modelFit$obsLevels <- obsLevels
            }

            log("Calling predict function for %s", modelName, files=files)
            if (interactive())
              debug(m$predict)
            xp <- m$predict(modelFit, x)
            assert(! is.null(xp),
                   "predict function returned a NULL",
                   files=files)

            if (is.function(m$prob)) {
              log("Calling prob function for %s", modelName, files=files)
              p <- m$prob(modelFit, x)
              assert(! is.null(p),
                     "prob function returned a NULL",
                     files=files)
            }
          }
        },
        error=function(e) {
          e$calls <- sys.calls()
          signalCondition(e)
        })
      },
      error=function(e) {
        if (length(e$calls) > 0) {
          cat('traceback (most recent call first):\n', file=logfile)
          calls <- rev(e$calls)[c(-1, -2)]
          for (xcall in calls) {
            if (identical(xcall[[1]], as.name('withCallingHandlers')))
              break
            cat('> ', file=logfile)
            sink(logfile)
            print(xcall)
            sink()
          }
        }
        cat(paste0('error: ', conditionMessage(e), '\n'), file=stderr())
        cat(sprintf('error testing model %s: continuing to next model\n', modelName))
        cat(sprintf('error: %s: %s\n', modelName, conditionMessage(e)), file=logfile)
      })

      # Clean up after testing the model
      rm(modelFit)
      gvars <- ls(name=.GlobalEnv)
      if (length(gvars) > 0) {
        log("Deleting global variable(s): %s", paste(gvars, collapse=" "), files=files)
        rm(list=gvars, pos=.GlobalEnv)
      }
      curpkgs <- search()
      rmpkgs <- curpkgs[! curpkgs %in% pkgs]
      if (length(rmpkgs) > 0) {
        log("Detaching package(s): %s",
            paste(sub('^package:', '', rmpkgs), collapse=' '),
            files=files)
        for (pkg in rmpkgs) {
          detach(name=pkg, unload=FALSE, character.only=TRUE, force=FALSE)
        }
        curpkgs <- search()
      }
      assert(all(curpkgs == pkgs),
             "failed to detach packages",
             files=files)
    }

    cat('\nSession info:\n\n', file=logfile)
    sink(logfile)
    print(sessionInfo())
    sink()
    close(logfile)
  }

  options(warn=1)
  training <- twoClassSim(100)
  testing <- twoClassSim(500)
  trainX <- training[, -ncol(training)]
  trainY <- training$Class

  models <- getModelInfo()
  args <- commandArgs(trailingOnly=TRUE)
  if (length(args) > 0) {
    args <- args[args %in% names(models)]
    nms <- as.list(args)
    names(nms) <- args
    models <- lapply(nms, function(nm) models[[nm]])
  }
  if (interactive())
    debug(runUnitTests)
  runUnitTests(models, trainX, trainY)
})
