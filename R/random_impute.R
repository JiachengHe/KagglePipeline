#' @title Randomly impute missing values
#' @description Draw from non-missing values, then replace missing values
#' @author Jiacheng He
#'
#'
#' @export


random_impute <- function(df, yVar=NULL, seed=1000) {

  set.seed(seed)

  for (p in 1:ncol(df)) {

    if (is.null(yVar) || names(df)[p] != yVar) {

      na <- is.na(df[[p]])
      num_na <- sum(na)

      if (num_na > 0) {
        df[[p]][na] <- sample(df[[p]][!na], size = num_na, replace = TRUE)
      }
    }
  }

  return(df)
}
