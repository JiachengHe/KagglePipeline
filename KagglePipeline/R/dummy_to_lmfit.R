#' @title Transform dummy variables to linear model fit
#' @description This function is a backend for factor_to_lmfit and text_to_features
#' @author Jiacheng He
#'
#'


dummy_to_lmfit <- function(df, yVar, dummy, trainIndex=NULL, alpha=0, lambda=0, trCon=NULL, cv_method="none") {

  if (is.null(trainIndex)) {
    trainIndex <- !is.na(df[[yVar]])
  }

  y_train <- df[[yVar]][trainIndex]

  if (is.factor(y_train)) {
    y_train <- factor(y_train, label = c("N", "Y"))
    classProbs <- TRUE

  } else if (is.numeric(y_train)) {
    classProbs = FALSE
  }

  if (is.null(trCon)) {
    trCon <- trainControl(method = cv_method, number = 5, returnData = FALSE,
                          savePredictions = "final", classProbs = classProbs)
  }

  glmnet_grid <- expand.grid(alpha = alpha, lambda = lambda)

  dummy_train <- dummy[trainIndex, ]
  dummy_test <- dummy[-trainIndex, ]

  lm_model <- train(dummy_train, y_train, method = "glmnet", trControl = trCon, tuneGrid = glmnet_grid)

  dummy_lmfit <- c()

  if (classProbs) {

    if (cv_method == "none") {
      dummy_lmfit[trainIndex] <- predict(lm_model, dummy_train, type = "prob")$Y
    } else { dummy_lmfit[trainIndex] <- arrange(lm_model$pred, rowIndex)$Y }

    dummy_lmfit[-trainIndex] <- predict(lm_model, dummy_test, type = "prob")$Y

  } else {

    if (cv_method == "none") { dummy_lmfit[trainIndex] <- predict(lm_model, dummy_train)
    } else { dummy_lmfit[trainIndex] <- arrange(lm_model$pred, rowIndex)$pred }

    dummy_lmfit[-trainIndex] <- predict(lm_model, dummy_test)
  }

  return(dummy_lmfit)
}
