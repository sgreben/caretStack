# caretStack

A small library to train stacked models using `caret::train`.

You give it a list of layers (each layer is a list of models) and it trains either a

- restacked (each layer uses all lower layers) or 
- non-restacked (each layer uses only the previous layer)

model.

## Installation

```R
devtools::install_github("sgreben/caretStack")
```

## Usage

The structure of a layers spec is as follows:

```R
layers <- list(
    list( # First layer
        modelName = list(
            parallel = NULL, # or an int = number of cores
            params = list (
                # params for caret::train
            )
        )
    ),
    list( # Second layer
        # Second layer models
    )
)
```

## Example

Here's a full example:

```R
library(caret)

gbm2 <- list(
  parallel = 4,
  params = list(
    method = "gbm",
    tuneGrid = expand.grid(
      n.trees = 300,
      interaction.depth = 2,
      shrinkage = 0.1,
      n.minobsinnode = 10
    ),
    trControl = trainControl(method = "none")
  )
)

gbm10 <- list(
  parallel = 4,
  params = list(
    method = "gbm",
    tuneGrid = expand.grid(
      n.trees = 300,
      interaction.depth = 10,
      shrinkage = 0.1,
      n.minobsinnode = 10
    ),
    trControl = trainControl(method = "none")
  )
)

xgb10 <- list(
  parallel = NULL,
  params = list(
    metric = "RMSE",
    method = "xgbTree",
    tuneGrid = data.frame(
      nrounds = 100,
      max_depth = 10,
      eta = 0.07,
      min_child_weight = 1.5,
      colsample_bytree = 0.5,
      subsample = 0.95,
      gamma = 0.045
    ),
    trControl = trainControl(method = "none")
  )
)

layers <- list(
  list(
    gbm2 = gbm2,
    gbm10 = gbm10,
    xgb10 = xgb10
  ),
  list(
    xgb10 = xgb10
  )
)

data(BostonHousing)
x <- BostonHousing[,!(names(BostonHousing) %in% c("medv"))]
x$chas <- ifelse(x$chas == "1", 1, 0)
y <- BostonHousing$medv

folds <- caret::createFolds(y, 5)
model <- caretStack::trainStack(x, y, layers, folds, verbose = T)

yHat <- caretStack::predictStack(model, x)
```