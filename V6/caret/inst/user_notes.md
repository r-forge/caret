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