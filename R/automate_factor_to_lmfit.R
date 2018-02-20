#' @title Automate the factor_to_lmfit workflow for all categorical variables in the data frame
#' @description Automatically transform all categorical variables into linear fit
#' @author Jiacheng He
#'
#'
#' @export
#'


automate_factor_to_lmfit <- function(df, yVar, keep=FALSE) {

  trainIndex <- (df$train_or_test == "train")
  facVars <- names(df)[sapply(df, is.factor)]

  facVars <- facVars[facVars != "train_or_test"]

  nzv <- nearZeroVar(df[facVars], freqCut = 95/5, saveMetrics = TRUE)
  nzv_facVar <- rownames(nzv[nzv$nzv,])
  if (length(nzv_facVar) > 0) {
    cat(paste("These variables are deleted because of too small variation:\n\n", list(nzv_facVar)))
    facVars <- facVars[!(facVars %in% nzv_facVar)]
  }

  for (facVar in facVars) {
    df <- factor_to_lmfit(df, facVar, yVar, trainIndex)
    if (!keep) {
      df[[facVar]] <- NULL
    }
  }

  return(df)
}


