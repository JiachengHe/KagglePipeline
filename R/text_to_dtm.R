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


text_to_dtm <- function(df, id, textVar, n_gram=1, stop_rm=TRUE, stem=NULL, dtm_value="tf_idf") {

  text <- df %>%
    select_(id, textVar) %>%
    unnest_tokens_("term", textVar)

  if (stop_rm) {
    data(stop_words)
    text <- anti_join(text, stop_words, by = c("term"="word"))
  }

  if (!is.null(stem)) {
    text <- mutate(text, term = wordStem(term, language = stem))
  }

  text <- text %>%
    count_(c(id, "term")) %>%
    bind_tf_idf_("term", id, "n") %>%
    cast_sparse_(id, "term", dtm_value)

  if (nrow(text) != nrow(df)) {
    warning("Some documents are deleted. The document-term matrix has different number of rows
            from the original data frame")
  }

  return(text)
}
