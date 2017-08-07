#' Predict using a stacked model
#'
#' @param stackModel Result of running trainStack
#' @param x     Predictors
#' @export
predictStack <- function(stackModel, x, verbose = F) {
  currentX <- x
  currentY <- vector("numeric", nrow(x))
  for (i in 1:length(stackModel)) {
    if (verbose) print(paste("Level", i))
    levelResults <- stackModel[[i]]
    for(j in names(levelResults$models)) {
      if (verbose) print(paste("Model:", j))
      model <- levelResults$models[[j]]
      Yhat <- predict(model, currentX)
      modelColumnName <- paste("Stack.L", i, "M", j, sep=".")
      currentX[,modelColumnName] <- Yhat
      currentY <- Yhat
    }
  }
  return(currentY)
}
