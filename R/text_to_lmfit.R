#' @title Turn text variable into linear fitted value
#' @author Jiacheng He
#'
#' @import glmnet
#' @import dplyr
#' @import tidytext
#' @import caret
#' @export
#'

text_to_lmfit <- function(df, textVar, yVar, id, form, n_gram=1, stop_rm=TRUE, stem=NULL, dtm_value="tf_idf",
                          trainIndex=NULL, alpha=0, lambda=0, trCon=NULL, cv_method="none", drop=TRUE) {

  dtm <- text_to_dtm(df, id, textVar, form, n_gram, stop_rm, stem, dtm_value)

  textVar_lmfit <- paste0(textVar, "_lmfit")

  df[[textVar_lmfit]] <- dummy_to_lmfit(df, yVar, dtm, trainIndex, alpha, lambda, trCon, cv_method)

  if (drop) { df[[textVar]] <- NULL }

  return(df)
}
