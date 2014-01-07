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

  checkFormals <- function(fun, expectedFormals, ...,
                           fail=FALSE, files=list(stderr())) {
    if (is.function(fun)) {
      fms <- formals(fun)
      fargs <- names(fms)
      alen <- length(fms)
      xlen <- length(expectedFormals)
      if (expectedFormals[[xlen]] == '...') {
        assert(alen >= xlen, ..., fail=fail, files=files)
        assert(all(fargs[1:(xlen-1)] == expectedFormals[1:(xlen-1)]), ...,
               fail=fail, files=files)
        assert(fargs[[alen]] == '...', ..., fail=fail, files=files)
      } else {
        assert(alen == xlen, ..., fail=fail, files=files)
        assert(all(names(fms) == expectedFormals), ..., fail=fail, files=files)
      }
    }
  }

  runUnitTests <- function(models, logfile, verbose, x, y) {
    pkgs <- search()
    legalTypes <- c('Classification', 'Regression')
    loopFormals <- c('grid')
    gridFormals <- c('x', 'y', 'len')
    fitFormals <- c('x', 'y', 'wts', 'param', 'lev', 'last',
                    'classProbs', '...')
    predictFormals <- c('modelFit', 'newdata', 'submodels')
    probFormals <- c('modelFit', 'newdata', 'submodels')
    predictorsFormals <- c('x', '...')
    varImpFormals <- c('object', '...')
    levelsFormals <- c('x')
    sortFormals <- c('x')
    expectedNames <- c('label', 'library', 'type', 'parameters', 'grid',
                       'loop', 'fit', 'predict', 'prob',
                       'predictors', 'varImp', 'levels', 'tags', 'sort')

    files <- list(logfile, stderr())
    cat(sprintf('Date: %s\n\n', date()), file=logfile)

    for (i in seq(along=models)) {
      modelName <- names(models)[[i]]
      log('\nTesting %s...', modelName, files=files)
      m <- models[[i]]

      modelNames <- names(m)
      unknownNames <- modelNames[! modelNames %in% expectedNames]
      missingNames <- expectedNames[! expectedNames %in% modelNames]
      if (length(missingNames) > 0) {
        log('warning: missing model methods: %s',
            paste(missingNames, collapse=' '),
            files=files)
      }
      if (length(unknownNames) > 0) {
        log('warning: unknown model methods: %s',
            paste(unknownNames, collapse=' '),
            files=files)
      }

      status <- 0
      for(i in seq(along = m$library)) {
        if (! require(m$library[i], character.only=TRUE, quietly=TRUE)) {
          log('warning: unable to load package %s', m$library[i], files=files)
          status <- -1
        }
      }
      if (status != 0) {
        next
      }

      tryCatch({
        withCallingHandlers({
          assert(is.character(m$label) && length(m$label) == 1,
                 "label isn't character type with one element",
                 files=files)
          assert(is.character(m$type) && length(m$type) %in% c(1, 2) &&
                 all(m$type %in% legalTypes),
                 "type is set incorrectly",
                 files=files)
          assert(is.null(m$loop) || is.function(m$loop),
                 "loop isn't NULL or a function",
                 files=files)
          checkFormals(m$loop, loopFormals,
                       "loop has incorrect arguments",
                       files=files)
          assert(is.data.frame(m$parameters) &&
                 is.factor(m$parameters$parameter) &&
                 is.factor(m$parameters$class) &&
                 is.factor(m$parameters$label),
                 "parameters is not a correct data frame",
                 files=files)
          assert(is.function(m$grid),
                 "grid isn't a function",
                 files=files)
          checkFormals(m$grid, gridFormals,
                       "grid has incorrect arguments",
                       files=files)
          assert(is.function(m$fit),
                 "fit isn't a function",
                 files=files)
          checkFormals(m$fit, fitFormals,
                       "fit has incorrect arguments",
                       files=files)
          assert(is.function(m$predict),
                 "predict isn't a function",
                 files=files)
          checkFormals(m$predict, predictFormals,
                       "predict has incorrect arguments",
                       files=files)
          assert(is.null(m$prob) || is.function(m$prob),
                 "prob isn't NULL or a function",
                 files=files)
          checkFormals(m$prob, probFormals,
                       "prob has incorrect arguments",
                       files=files)
          assert(is.null(m$predictors) || is.function(m$predictors),
                 "predictors isn't NULL or a function",
                 files=files)
          checkFormals(m$predictors, predictorsFormals,
                       "predictors has incorrect arguments",
                       files=files)
          assert(is.null(m$varImp) || is.function(m$varImp),
                 "varImp isn't NULL or a function",
                 files=files)
          checkFormals(m$varImp, varImpFormals,
                       "varImp has incorrect arguments",
                       files=files)
          assert(is.null(m$levels) || is.function(m$levels),
                 "levels isn't NULL or a function",
                 files=files)
          checkFormals(m$levels, levelsFormals,
                       "levels has incorrect arguments",
                       files=files)
          assert(is.character(m$tags),
                 "tags is not character type",
                 files=files)
          assert(is.function(m$sort),
                 "sort isn't a function",
                 files=files)
          checkFormals(m$sort, sortFormals,
                       "sort has incorrect arguments",
                       files=files)

          tuneGrid <- m$grid(x, y, 3)
          assert(is.data.frame(tuneGrid))

          if (is.function(m$loop)) {
            trainInfo <- m$loop(tuneGrid)
            assert(is.list(trainInfo) &&
                   all(c('loop', 'submodels') %in% names(trainInfo)),
                   "loop function didn't return a correct list",
                   files=files)
            assert(is.data.frame(trainInfo$loop),
                   "loop()$loop is not data frame",
                   files=files)
            assert(is.list(trainInfo$submodels) ||
                   is.null(trainInfo$submodels),
                   "loop()$submodels is not a list or NULL",
                   files=files)
            for (sm in trainInfo$submodels) {
              assert(is.data.frame(sm),
                     "loop()$submodels doesn't contain all data frames",
                     files=files)
              assert(all(colnames(sm) %in% names(trainInfo$loop)),
                     "loop()$submodels contains a data frame with bad columns",
                     files=files)
            }
          }

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
        cat(sprintf('error: %s: %s\n', modelName, conditionMessage(e)),
            file=logfile)
      })

      # Clean up after testing the model
      suppressWarnings(rm(modelFit))
      gvars <- ls(name=.GlobalEnv)
      if (length(gvars) > 0) {
        log("Deleting global variable(s): %s", paste(gvars, collapse=" "),
            files=files)
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
  }

  # Default option values
  dbg <- character(0)
  logname <- 'testrun.log'
  modelfiles <- character(0)
  pattern <- NULL
  verbose <- FALSE

  # Process command line
  args <- commandArgs(trailingOnly=TRUE)
  i <- 1
  while (i <= length(args)) {
    a <- args[[i]]
    if (a == '-d') {
      i <- i + 1
      dbg <- c(dbg, args[[i]])
    } else if (a == '-l') {
      i <- i + 1
      logname <- args[[i]]
    } else if (a == '-p') {
      i <- i + 1
      pattern <- args[[i]]
    } else if (a == '-v') {
      verbose <- TRUE
    } else {
      modelfiles <- args[i:length(args)]
      # Only keep ".R" files that exist
      modelfiles <- modelfiles[grep('\\.R$', modelfiles) &
                               file.exists(modelfiles)]
      names(modelfiles) <- sub('\\.R$', '', basename(modelfiles))
      if (length(modelfiles) == 0) {
        cat('found no R model files\n', file=stderr())
        quit(status=1, save='no')
      }
      break
    }
    i <- i + 1
  }

  options(warn=1)
  training <- twoClassSim(100)
  testing <- twoClassSim(500)
  trainX <- training[, -ncol(training)]
  trainY <- training$Class

  # Get the models list
  if (length(modelfiles) > 0) {
    # Model file names were specified on the command line
    models <- lapply(modelfiles, function(mfile) {
      source(mfile, local=TRUE)
      modelInfo
    })
  } else {
    models <- getModelInfo()
  }

  # Filter models based on -p argument
  if (! is.null(pattern)) {
    models <- models[grep(pattern, names(models))]
    if (length(models) == 0) {
      cat(sprintf('no models were selected by pattern %s\n', pattern),
          file=stderr())
      quit(status=1, save='no')
    }
  }

  # Set debug flag on specified model methods if interactive
  if (length(dbg) > 0 && interactive()) {
    for (mod in models) {
      for (meth in dbg) {
        debug(mod[[meth]])
      }
    }
  }

  # Open the log file
  logfile <- file(logname, 'w')
  on.exit(close(logfile))

  # Run the unit tests
  runUnitTests(models, logfile, verbose, trainX, trainY)
})
