#' @title Save the Kaggle submissions .csv files
#' @description Automate the numbering and saving of the "submission_.csv" files
#' @author Jiacheng He
#'
#' @importFrom stringr str_subset
#' @importFrom stringr str_extract
#' @importFrom dplyr '%>%'
#' @export

write_submission <- function(df_sub, path="submission/") {

  submission_files <- list.files(path) %>% str_subset("submission")

  if (length(submission_files) > 0) {
    j <- max(as.numeric(str_extract(submission_files, "\\-*\\d+\\.*\\d*"))) + 1
  } else {
    j <- 1
  }
  filename <- paste0(path, "submission_", j, ".csv")
  write.csv(df_sub, file = filename, row.names = FALSE)

  return(filename)
}
