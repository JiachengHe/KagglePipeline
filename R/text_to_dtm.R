#' @title Transform a text column from a data frame into document-term matrix
#' @description Can perform stemming and removing stop words
#' @author Jiacheng He
#'
#' @param df The data frame
#' @param id The colnumn name of the id variable
#' @param textVar The column name of the text variable
#'
#' @import tidytext
#' @import SnowballC
#' @importFrom dplyr '%>%'
#' @importFrom dplyr select_
#' @importFrom dplyr mutate
#' @importFrom dplyr count_
#' @export
#'


text_to_dtm <- function(df, id, textVar, form, n_gram=1, stop_rm=TRUE, stem=NULL, dtm_value="tf_idf") {

  if (!form %in% c("tidy", "sparse", "dtm")) { stop('form must be either "tidy" or "sparse".') }

  text <- df %>%
    select_(id, textVar) %>%
    unnest_tokens_("term", textVar)

  if (stop_rm) {
    data(stop_words, package = "tidytext")
    text <- anti_join(text, stop_words, by = c("term"="word"))
  }

  if (!is.null(stem)) {
    text <- mutate(text, term = wordStem(term, language = stem))
  }

  text <- text %>%
    count_(c(id, "term")) %>%
    bind_tf_idf_("term", id, "n")

  if (form == "tidy") {
  } else if (form == "sparse") {
    text <- text %>% cast_sparse_(id, "term", dtm_value)
    if (nrow(text) != nrow(df)) {
      warning("Some documents are deleted. The document-term matrix has different number of rows
              from the original data frame")
    }
  } else if (form == "dtm") {
    text <- text %>% cast_dtm_(id, "term", dtm_value)
    if (nrow(text) != nrow(df)) {
      warning("Some documents are deleted. The document-term matrix has different number of rows
              from the original data frame")
    }
  }

  return(text)
}
