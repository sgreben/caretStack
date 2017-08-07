#' Predict using a stacked model
#'
#' @param model Result of running trainStack
#' @param x     Predictors
#' @export
predictStack <- function(model, x, verbose = F) {
  currentX <- x
  currentY <- vector("numeric", nrow(x))
  for (i in 1:length(model)) {
    if (verbose) print(paste("Level", i))
    levelResults <- model[[i]]
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
