#' @title Save the Kaggle submissions .csv files
#' @description Automate the numbering and saving of the "submission_.csv" files
#' @author Jiacheng He
#'
#' @importFrom stringr str_subset
#' @importFrom stringr str_extract
#' @export

save_submission <- function(df_sub, path="submissions/") {

  submission_files <- list.files(path) %>% str_subset("submission")
  j <- max(str_extract(submission_files, "\\d"))
  j <- as.numeric(j) + 1
  filename <- paste0(path, "submission_", j, ".csv")
  write.csv(df_sub, file = filename, row.names = FALSE)
}
