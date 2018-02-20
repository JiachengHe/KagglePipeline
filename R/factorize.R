#' @title Turn categorical variables into the factor format
#'
#' @author Jiacheng He
#'
#' @export
#'


factorize <- function(df, var_list=NULL) {

  if (is.null(var_list)) {
    var_list <- names(df)[sapply(df, is.character)]
  }

  for (var in var_list) {
    df[[var]] <- factor(df[[var]])
  }

  return(df)
}
