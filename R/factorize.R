#' @title Turn categorical variables into the factor format
#'
#' @author Jiacheng He
#'
#' @export
#'


factorize <- function(df, var_list) {

  for (var in var_list) {
    df[[var]] <- factor(df[[var]])
  }
  return(df)

}
