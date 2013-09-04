Creating Your Own Model in train
========

You can pass a list of information to the `method` argument in` train`. For models that are built-in to the package, you can just pass the method name as before. 

There are some basic components of the list for custom models. A breif description is below for each then, after setting up and example, each will be described in detail. The list should have hte following elments:

* `library` is a character vector of package names that will be needed to fit the model or calculate predictions. `NULL` can also be used. 
* `type` is a simple character vector with values `"Classification"`, `"Regression"` or both. 
* `parameters` is a data frame with three simple attributes for each tuning parameter (if any): the argument name (e.g. `mtry`), the type of data in the parameter grid and textual labels for hte parameter.
* `grid` is a function that is used to create the tuning grid (unless the user gives the exact values of the parameters via `tuneGrid`)
* `fit` is a function that fits the model
* `predict` is the funciton that creates predicitons
* `prob` is a function that can be used to create class probabilities (if applicable)
* `sort` is a function that sorts the parameter form most complex to least
* `loop` is funciton for advanced users for models that can create multiple submodel predicitons form the same object.

In the `caret` package, the subdirectory `models` has all the code for each model that `train` interfaces with and these can be used as prototypes for your model. 

Let's create a new model for a classification support vector machine using the Laplacian kernel function. We will use hte `kernlab` package's `ksvm` function. The kernel has two parameters: the standard cost parameter for SVMs and one kernel parameter (`sigma`).


To start, we'll create a new list:

    lpSVM <- list(type = "Classification",
                  library = "kernlab")
    
This model can also be used for regression but we will constrain things here for simplicity. For other SVM models, the type value would be `c("Classificaiton", "Regression")`. 

The `library` value checks to see if this package is installed and loads it whenever it is needed (e.g. before modeling or prediction).

The parameters Element
----------------------

We have to create some basic information for the parameters in the form of a data frame. The first column is the name of the parameter. Teh convention is to use the argument name in the model function (e.g. the `ksvm` function here). Those values are `C` and `sigma`. Each is a number and we can give them labels of "Cost" and "Sigma", respectively. The `parameters` element would then be:

    prm <- data.frame(parameter = c("C", "sigma"),
                      class = rep("numeric", 2),
                      label = c("Cost", "Sigma"))
                      
Now we assign it to the model list:

   lpSVM$parameter <- prm
    
Values of `type` can indicate numeric, character or logical data types. 

The grid Element
----------------

This should be a function that takes parameters: `x` and `y` (for the predictors and outcome data) as well as `len`. The latter is the value of `tuneLength` that is potentially passed in through `train`. 

The output should be a data frame of tuning parameter combinations with a column for each parameter. The column names should be the parameter name (e.g. the values of `prm$parameter`) preceeded by a dot. In our case, let's vary the cost parameter on the log 2 scale. For the sigma parmeter, we can use the `kernlab` function `sigest` to pre-estimate the value. Following `ksvm` we take the average of the low and high estimtes. Here is a function we could use:

    svmGrid <- function(x, y, len = NULL) {
      library(kernlab)
      ## This produces low, middle and high values for sigma 
      ## (i.e. a vector with 3 elements). 
      sigmas <- sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)  
      expand.grid(.sigma = mean(sigmas[-2]),
                  .C = 2 ^((1:len) - 3))
    }

Again, the user can pass their own grid via `train`'s `tuneGrid` option or they can use this code to create a default grid. 

We assign this function to the overall model list:

    lpSVM$grid <- svmGrid
   
The fit Element
----------------

Here is where we fit the model. This `fit` funciton has several arguments:

* `x`, `y`: the current data used to fit the model
* `wts`: optional instance weights (not applicable for this particular model)
* `param`: the current tuning parameter values
* `lev`: the class levels of the outcome (or `NULL` in regression)
* `last`: a logical for whether the current fit is the final fit
* `weights`
* `classProbs`: a lgical for whether class probabilities should be computed.

Here is something we could use for this model:

    svmFit <- function(x, y, wts, param, lev, last, 
                       weights, classProbs, ...) { 
      ksvm(x = as.matrix(x), y = y,
           kernel = rbfdot,
           kpar = list(sigma = param$.sigma),
           C = param$.C,
           prob.model = classProbs,
           ...)
     }
     
     lpSVM$fit <- svmFit

A few notes about this:

* Notice that the package is not loaded in the code. It is loaded prior to this function being called so it won't hurt if you laod it again (but that's not needed).
* The `ksvm` function requires a _matrix_ or predictors. If the original data were a data frame, this would throw and error. 
* The tuning parameters are references in the `param` data frame. There is always a single row in this data frame. 
* The probability model is fit based on the value of `classProbs`. This value is determined by the value given in `trainControl`. 
* The three dots allow the user to pass options in from `train` to, in this case, the `ksvm` function. For example, if the user wanted to set the chace size for the function, they could list `cache = 80` and this argument will be pass from `train` to `ksvm`.  
* Any pre-processing that was requested in the call to `train` have been done. For example, if `preProc = "center"` was orignally requested, the columns of `x` seen within this function are mean centered. 

The pred Element
----------------

This is a function that produces a vector or predicitons. In our case, these are class predictions but they could be numbers for regression models. 

The arguments are:

* `modelFit`: the model produced by the `fit` code shown above. 
* `newdata`: the predictor values of the instances being predicted (e.g. out-of-bag samples)
* `preProc` 
* `submodels`: this an optionl list of tuning parameters only used with the `loop` element discussed below. In most cases, it will be `NULL`.

Our function will be very simple:

    svmPred <- function(modelFit, newdata, preProc = NULL, 
                        submodels = NULL)
       predict(modelFit, newdata)
    lpSVM$pred <- svmPred

The funciton `predict.ksvm` will automatically create a factor vector as output. The funcitn could also produce character values. Either way, the innards of `train` will make them factors and ensure that the same levels as the original data are used. 

The prob Element
----------------

If a regresisn model is being used or if the classificaiotn model does not create class probabilties a value of `NULL` can be used here instead of a function. Otherwise, the function arguments are the same as the  `pred` function. The output should be a matrix or data frame of class probabilties with a column for each class. The column names should be the class levels. 

We can use:

    svmProb <- function(modelFit, newdata, preProc = NULL, 
                        submodels = NULL)
       predict(modelFit, newdata, type="probabilities")
    lpSVM$prob <- svmProb

If you look at some of the SVM examples in the `models` directory, the real functions used by `train` are much more complicated so that they can deal with model failures, probabilities that do not sum to 1 etc.

The sort Element
----------------

This is an optional function that sorts the tuning parameters from the simplest model to the most complex. There are times where this ordering is not obvious. This information is used when the performance values are tied across multiple parameters. We would probabily want to choose the least complex model in those cases. 

Here, we will sort by the cost value. Smaller values of `C` produce smoother class boundaries than larger values:


    svmSort <- function(x) x[order(x$C))])
    lpSVM$sort <- svmSort


An Illustration
---------------

We should now be ready to fit our model. 


The loop Element
----------------

This function can be used to create custom loops for models to tune over. In most cases, the function can just return the existing tuning grid.

For example, a LogitBoost model can be trained over the number of boosting iterations. In the `caTools` package, the `LogitBoost` function can be used to fit this model. For example:

    mod <- LogitBoost(as.matrix(x), y, nIter = 50)
    
If we were to tune the model evaluating models where the number of iterations [1] was 10, 20, 30, 40 and 50, the grid could be

    lbGrid <- data.frame(.nIter = (1:5)*10)    
    
 During resampling, `train` could loop over all five rows in `lbGrid` and fit five models. However, the `predict.LogitBoost` function has an argument called `nIter` that can produce, in this case, predictions from `mod` for all five models. 

Instead of `train` fitting five models, we could fit a single model with `nIter = 50` and derive predictions for all five models using only `mod`. 

The terminology used here is that `nIter` is a _sequential_ tuning parameter (and _fixed_ otherwise). 

The `loop` argument for models is used to produce two objects:

 * `loop`: this is the actual loop that is used by `train`. 
 *  `submodels` is a _list_ that has as many elements as there are rows in `loop`. The list has all the "extra" models that are derived for each model.
 
Going back to the LogitBoost example, we could have:

    loop <- data.frame(.nIter = 50)
    submodels <- list(data.frame(.nIter = (1:4)*10))
    
For this case, `train` first fits the `nIter = 50` model. When the model is predicted, that code has a `for` loop that iterates over the elements of `submodel[[1]]` to get the predictions for the other 4 models. 

In the end, predictions for all five models (for `nIter = (1:5)*10`) with a single model fit. 

There are other models built-in to `caret` that are used this way. There are a number of models that have multiple sequential tuning parameters.

If the `loop` argument is left `NULL` the results of `tuneGrid` are used as the simple loop and is recommended for most situations. Note that the machinery that is used to "derive" the extra predictions is up to the user to create, typically in the `pred` and `prob` elements of the custom model object. 
    
For the LogitBoost model, some simple code to create these objects would be:

    fullGrid <- data.frame(.nIter = (1:5)*10)

    ## Get the largest value of ncomp to fit the "full" model
    loop <- fullGrid[which.max(fullGrid$.nIter),,drop = FALSE]
    
    submodels <- fullGrid[-which.max(fullGrid$.nIter),,drop = FALSE]

    ## This needs to be excased in a list in case there are more
    ## than one tuning parameter
    submodels <- list(submodels)    
    
The results look like:

    > loop      .nIter    5     50  
    
 and
 
    > submodels    [[1]]      .nIter    1     10    2     20    3     30    4     40

For the LogitBoost custom model object, we could use this code in the 'pred' slot:

    function(modelFit, newdata, preProc = NULL, submodels = NULL) {
        ## This model was fit with the maximum value of nIter
        out <- caTools::predict.LogitBoost(modelFit, newdata, type="class")
        
        ## In this case, 'submodels' is a data frame with the other values of
        ## nIter. We loop over these to get the other predictions.
        if(!is.null(submodels))
        {
          ## Save _all_ the predictions in a list
          tmp <- out
          out <- vector(mode = "list", length = nrow(submodels) + 1)
          out[[1]] <- tmp
      
          for(j in seq(along = submodels$.nIter))
          {
            out[[j+1]] <- caTools::predict.LogitBoost(modelFit,
                                                      newdata,
                                                      nIter = submodels$.nIter[j])
          }
        }
        out                   
      }

After model training (i.e. predicting new samples), the value of `submodels` is set to `NULL` and the code produces a single set of predictions. The `prob` slot works in the same way. The only difference is that the values saved in the outgoing lists are matrices or data frames of probabilities for each class. 
    
Another model with a sequential tuning parameter is cubist. One parameter, the number of committees, is fixed. The other tuning parameters is the number of nearest neighbors and this is only required when `predict.cubist` is used.

Suppose we have this tuning grid:

    grid <- expand.grid(.neighbors = c(0, 5, 9),
                        .committees = c(1, 10, 20))

For very value of `committees` we only need to fit the model with the largest number of nieghbors (9).

Just for illustration, we will remove one so that we don't have the full grid of parameters:

    grid <- grid[-1,]

The loop should be a data frame with the unique values of committees and the largest number of neighbors and the corresponding number of committees. We can start with this:

    coms <- unique(grid$.committees)        ## c(1, 10, 20)
    loop <- data.frame(.committees = coms)
                    
    ## Create an empty slot for later
    loop$.neighbors <- NA
    
For each value of committees, find the largest  value of `neighbors` and assign it to a row of `loop`.  For the current value of `committees`, assign the other values of `neighbors` to a data frame and add it to `submodels`.

    for(i in seq(along = coms))
    {
      nn <- grid[grid$.committees == coms[i],".neighbors"]
      loop$.neighbors[loop$.committees == coms[i]] <- nn[which.max(nn)]
      submodels[[i]] <- data.frame(.neighbors = nn[-which.max(nn)])
    }

In the end, we get:

    > loop      .committees .neighbors    1           1          9    2          10          9    3          20          9
                 
and 

    > submodels    [[1]]      .neighbors    1          5    [[2]]      .neighbors    1          0    2          5    [[3]]      .neighbors    1          0    2          5
                 
As a result, the `fit` part of the model object will fit:

    mod <- cubist(x, y, committees = 1)

When the `pred` code is invoked, we initial get predictions for `neighbors = 9` then loop over `submodels[[1]]` to get ther rest. On the first iteration, `submodels[[1]]` has a single value of `neighbors` but subsequent iterations with have more than one. 

The `pred` code looks like this:


    
Footnotes:

[1] In practice, we would want nIter to be an odd number to avoid ties in the predictions when there are two classes    