library(caret)
library(doParallel)

trainStack.outOfFoldPredictions <- function(specs, folds, x, y, verbose = F) {
  predictions = list()
  for (j in names(specs)) {
    predictions[[j]] <- y
  }
  scores = list()
  for (j in names(specs)) {
    scores[[j]] <- list()
  }
  models = list()
  for (i in names(folds)) {
    if (verbose) print(paste("Fold:", i))
    fold <- folds[[i]]
    Xtrain <- x[fold, ]
    Ytrain <- y[fold]
    Xeval <- x[-fold, ]
    Yeval <- y[-fold]
    for (j in names(specs)) {
      if (verbose) print(paste("Model:", j))
      spec <- specs[[j]]
      trainArgs <- c(list(x = Xtrain, y = (Ytrain)), spec$params)
      if (is.null(spec$parallel)) {
        registerDoSEQ()
      } else {
        registerDoParallel(cores = spec$parallel)
      }
      model <- do.call(caret::train, trainArgs)
      Yhat <- predict(model, Xeval)
      predictions[[j]][-fold] <- Yhat
      scores[[j]][[i]] <- caret::postResample(Yhat, (Yeval))
    }
  }
  for (j in names(specs)) {
    print(paste("Fitting", j, "on entire dataset"))
    trainArgs <- c(list(x = x, y = y), spec$params)
    if (is.null(spec$parallel)) {
      registerDoSEQ()
    } else {
      registerDoParallel(cores = spec$parallel)
    }
    model <- do.call(caret::train, trainArgs)
    models[[j]] <- model
  }
  return(list(
    predictions = predictions,
    scores = scores,
    models = models
  ))
}


#' Train a stacked model using caret
#'
#' @param x Predictors
#' @param y Response
#' @param layers Stack layers. A list of lists of models.
#' @param folds CV folds, as created by caret::createFolds
#' @param verbose Output progress messages
#' @export
#' @examples
#' stackLayers <- list(
#'  list(
#'    gbm2 = list(
#'      parallel = 4,
#'      params = list(
#'        method = "gbm",
#'        tuneGrid = expand.grid(
#'          n.trees = 300,
#'          interaction.depth = 2,
#'          shrinkage = 0.1,
#'          n.minobsinnode = 10
#'        ),
#'        trControl = trainControl(method = "none")
#'      )
#'    ),
#'    gbm10 = list(
#'      parallel = 4,
#'      params = list(
#'        method = "gbm",
#'        tuneGrid = expand.grid(
#'          n.trees = 300,
#'          interaction.depth = 10,
#'          shrinkage = 0.1,
#'          n.minobsinnode = 10
#'        ),
#'        trControl = trainControl(method = "none")
#'      )
#'    )
#'   ),
#'  list(
#'     gbm2 = list(
#'       parallel = 4,
#'       params = list(
#'         method = "gbm",
#'         tuneGrid = expand.grid(
#'           n.trees = 300,
#'           interaction.depth = 2,
#'           shrinkage = 0.1,
#'           n.minobsinnode = 10
#'         ),
#'         trControl = trainControl(method = "none")
#'       )
#'     )
#')
#')
#'
#'folds <- caret::createFolds(x, 10)
#'stackModel <- trainStack(x, y, stackLayers, folds)
trainStack <- function(x, y, layers, folds, verbose = F) {
  currentX <- x
  layerResults <- list()
  for (i in 1:length(layers)) {
    if (verbose) print(paste("layer", i))
    layerSpecs <- layers[[i]]
    oofResults <- trainStack.outOfFoldPredictions(layerSpecs, folds, currentX, y, verbose = verbose)
    layerResults[[i]] <- oofResults
    for (j in names(layerSpecs)) {
      if (verbose) print(paste("Summary for", j))
      if (verbose) print(unlist(lapply(oofResults$scores[[j]], function(x) x[1])))
      modelColumnName <- paste("Stack.L", i, "M", j, sep = ".")
      currentX[,modelColumnName] <- oofResults$predictions[[j]]
    }
  }
  return(layerResults)
}
