#' @title Automate the factor_to_lmfit workflow for all categorical variables in the data frame
#' @description Automatically transform all categorical variables into linear fit
#' @author Jiacheng He
#'
#' @importFrom caret nearZeroVar
#' @export
#'


automate_factor_to_lmfit <- function(df, yVar, facVars=NULL, drop=TRUE, drop_nzv=TRUE) {

  trainIndex <- (df$train_or_test == "train")
  if (is.null(facVars)) { facVars <- names(df)[sapply(df, is.factor)] }

  facVars <- facVars[!(facVars %in% c(yVar, "train_or_test"))]

  if (drop_nzv) {

    nzv <- nearZeroVar(df[facVars], freqCut = 95/5, saveMetrics = TRUE)
    nzv_facVar <- rownames(nzv[nzv$nzv,])
    if (length(nzv_facVar) > 0) {
      cat(paste("These variables are deleted because of too small variation:\n\n", list(nzv_facVar)))
      facVars <- facVars[!(facVars %in% nzv_facVar)]
    }
  }


  for (facVar in facVars) {
    df <- factor_to_lmfit(df, yVar, facVar, trainIndex)
    if (drop) {
      df[[facVar]] <- NULL
    }
  }

  return(df)
}


