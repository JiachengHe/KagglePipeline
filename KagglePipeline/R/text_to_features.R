#' @title Transform text column into numeric features
#' @description Features such as text length, sentiment, and linear model fit
#' @author Jiacheng He
#'
#' @param df The data frame
#' @param id The colnumn name of the id variable
#' @param col_text The column name of the text variable
#'
#' @export

text_to_features <- function(df, yVar, id, textVar, trainIndex=NULL, n_gram=1, stop_rm=TRUE, stem=NULL, dtm_value="tf_idf") {


  df %>%
    select_(id, textVar) %>%
    unnest_tokens_("term", textVar) %>%
    group_by_(id) %>%
    summarize(n = n())

  textVar_wordcount <- paste0(textVar, "_wordcount")
  df[[textVar_wordcount]] <-




  dtm <- text_to_dtm(df, id, textVar, n_gram=n_gram, stop_rm=stop_rm, stem=stem, dtm_value=dtm_value)

  if (nrow(df) != nrow(dtm)) {
    stop("Some documents are deleted. The document-term matrix has different number of rows
            from the original data frame")
  }

  textVar_lmfit <- paste0(textVar, "_lmfit")

  df[[textVar_lmfit]] <- dummy_to_lmfit(df, yVar, dtm, trainIndex, alpha=alpha, lambda=lambda,
                                       trCon=trCon, cv_method=cv_method)


}
