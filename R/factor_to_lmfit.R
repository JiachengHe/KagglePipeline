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

factor_to_lmfit <- function(df, yVar, facVar, trainIndex, alpha=0, lambda=0, trCon=NULL, cv_method="none") {

  dummy <- as.formula(paste("~", facVar)) %>%
    sparse.model.matrix(data = df)

  facVar_lmfit <- paste0(facVar, "_lmfit")

  df[[facVar_lmfit]] <- dummy_to_lmfit(df, yVar, dummy, trainIndex, alpha=alpha, lambda=lambda,
                                       trCon=trCon, cv_method=cv_method)

  return(df)
}
