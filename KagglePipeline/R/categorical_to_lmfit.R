#' @title Insert linear projection of categorical variables
#' @description This function runs a set of linear regressions of y on each of the specified categorical variables. Then insert the fitted
#' values of the linear regressions
#' @author Jiacheng He
#'
#' @param varName A character, the name of the categorical variable
#' @param y_train The target variable in the training set
#' @param df The data frame which contains the variables
#' @param alpha alpha=0 for ridge, alpha=1 for LASSO.
#' @param lambda Regularization parameter
#'
#' @importFrom caret train
#' @importFrom caret trainControl
#' @importFrom caret expand.grid
#' @importFrom Matrix sparse.model.matrix
#' @importFrom dplyr arrange
#' @export
#' @return A data frame with new columns of the fitted values.

categorical_to_lmfit <- function(varName, y_train, df, alpha=0, lambda=0) {

  y_train <- factor(y_train, label = c("N", "Y"))
  trCon <- trainControl(method = "cv", number = 5, returnData = FALSE,
                        savePredictions = "final", classProbs = TRUE)
  glmnet_grid <- expand.grid(alpha = alpha, lambda = lambda)
  equation <- as.formula(paste("~", varName))
  dummy <- sparse.model.matrix(equation, data = df)
  lm_fit <- train(dummy, y_train, method = "glmnet", trControl = trCon, tuneGrid = glmnet_grid)
  varName_lmfit <- paste0(varName, "_lmfit")
  df[[varName_lmfit]] <- arrange(lm_fit$pred, rowIndex)$Y
  return(df)
}
