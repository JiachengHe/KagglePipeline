#' @title Insert linear projection of categorical variables
#' @description This function runs a regression on a set of dummies. Then replace it with the out-of-fold
#' prediction in the training set, and with prediction in the test set. (It only supports binary factor y
#' now. Will extend to other types later.)
#' @author Jiacheng He
#'
#' @param df The data frame which contains the variables
#' @param facVar The name of the factor variable
#' @param yVar The name of the left-hand-side target variable
#' @param trainIndex The indexes of training set observations
#' @param alpha alpha=0 for ridge, alpha=1 for LASSO.
#' @param lambda Regularization parameter
#'
#' @importFrom caret train
#' @importFrom caret trainControl
#' @importFrom Matrix sparse.model.matrix
#' @importFrom dplyr arrange
#' @importFrom dplyr '%>%'
#' @export
#' @return A data frame with new columns of the fitted values.

factor_to_lmfit <- function(df, facVar, yVar, trainIndex, alpha=0, lambda=0, cv_method="none") {

  y_train <- df[[yVar]][trainIndex]

  if (is.factor(y_train)) {
    y_train <- factor(y_train, label = c("N", "Y"))
    classProbs <- TRUE

  } else if (is.numeric(y_train)) {
    classProbs = FALSE
  }


  trCon <- trainControl(method = cv_method, number = 5, returnData = FALSE,
                        savePredictions = "final", classProbs = classProbs)

  glmnet_grid <- expand.grid(alpha = alpha, lambda = lambda)

  equation <- as.formula(paste("~", facVar))

  dummy <- sparse.model.matrix(equation, data = df)

  dummy_train <- dummy[trainIndex, ]
  dummy_test <- dummy[-trainIndex, ]

  lm_fit <- train(dummy_train, y_train, method = "glmnet", trControl = trCon, tuneGrid = glmnet_grid)

  facVar_lmfit <- paste0(facVar, "_lmfit")

  df[[facVar_lmfit]] <- NA

  if (classProbs) {

    if (cv_method == "none") {
      df[[facVar_lmfit]][trainIndex] <- predict(lm_fit, dummy_train, type = "prob")$Y
    } else { df[[facVar_lmfit]][trainIndex] <- arrange(lm_fit$pred, rowIndex)$Y }

    df[[facVar_lmfit]][-trainIndex] <- predict(lm_fit, dummy_test, type = "prob")$Y

  } else {

    if (cv_method == "none") { df[[facVar_lmfit]][trainIndex] <- predict(lm_fit, dummy_train)
    } else { df[[facVar_lmfit]][trainIndex] <- arrange(lm_fit$pred, rowIndex)$pred }

    df[[facVar_lmfit]][-trainIndex] <- predict(lm_fit, dummy_test)
  }

  return(df)
}
