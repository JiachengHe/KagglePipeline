#' @title Blend the tuned models altogether
#' @description After training many models, use a final model to combine them all together
#' @author Jiacheng He
#'
#' @param model_list A list of fine tuned models output by caret::train
#' @param X_train The training set X matrix
#' @param y_train The training set target variable
#' @param X_test The test set X matrix
#' @param method The method parameter passed to caret::train
#' @param ... Not defined yet
#'
#' @importFrom dplyr arrange
#' @importFrom caret train
#' @importFrom caret trainControl
#' @export
#' @return The blending prediction of all models



blending <- function(model_list, X_train, y_train, X_test, method, trControl=NULL, tuneLength=10, ...){

  K <- length(model_list)
  n_train <- nrow(X_train)
  n_test <- nrow(X_test)

  X_train_modelfit <- matrix(0, nrow = n_train, ncol = K)
  X_test_modelfit <- matrix(0, nrow = n_test, ncol = K)

  colnames(X_train_modelfit) <- paste0(names(model_list), "_fit")
  colnames(X_test_modelfit) <- paste0(names(model_list), "_fit")

  if (is.factor(y_train)){

    classProbs <- TRUE
    y_train <- factor(y_train, label = c("N", "Y"))

    for (k in 1:K) {

      model <- model_list[[k]]

      X_train_modelfit[,k] <- arrange(model$pred, rowIndex)$Y
      X_test_modelfit[,k] <- predict(model, X_test, type = "prob")$Y
    }
  } else if (is.numeric(y_train)) {

    classProbs = FALSE

    for (k in 1:K) {

      model <- model_list[[k]]

      X_train_modelfit[,k] <- arrange(model$pred, rowIndex)$pred
      X_test_modelfit[,k] <- predict(model, X_test)
    }
  }

  if (is.null(trControl)) {
    trControl <- trainControl(method = "cv", number = 5, returnData = FALSE,
                          savePredictions = "final", classProbs = classProbs, search = "random")
  }


  blend_model <- train(X_train_modelfit, y_train, method = method,
                       trControl = trControl, tuneLength = tuneLength)
  blend_model$call$tuneLength <- tuneLength

  y_pred <- predict(blend_model, X_test_modelfit)


  return(list(blend_model = blend_model, y_pred = y_pred, X_train_modelfit = X_train_modelfit,
              X_test_modelfit = X_test_modelfit))
}
