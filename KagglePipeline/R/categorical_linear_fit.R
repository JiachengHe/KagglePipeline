#' @title Insert linear projection of categorical variables
#' @description This function runs a set of linear regressions of y on each of the specified categorical variables. Then insert the fitted
#' values of the linear regressions
#' @author Jiacheng He
#'
#' @param var_list A list of names of categorical variables (characters)
#' @param y The name of the target variable (character)
#' @param df The data frame which contains the variables
#' @param replaced Whether to replace the original factor cloumns with the fitted value cloumns
#'
#' @export
#' @return A data frame with new columns of the fitted values.

categorical_linear_fit <- function(var_list, y, df, replaced = FALSE) {

  for (var in var_list) {
    form <- paste(y, "~", var)
    lm_fit <- lm(form, data = df)
    if (replaced) {
      df[[var]] <- lm_fit$fitted
    } else {
      var_fitted <- paste0(var, "_fitted")
      df[[var_fitted]] <- lm_fit$fitted
    }
  }
  return(df)
}
