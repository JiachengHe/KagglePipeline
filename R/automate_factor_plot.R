#' @title Automate the factor_plot workflow when we have many categorical variables
#' @description A interactive interface to display the plots one by one. Meanwhile we can write codes in the
#' text editor to modify the categorical variables
#' @author Jiacheng He
#'
#' @param df The data frame
#' @param yVar The name of the target variable
#'
#' @importFrom dplyr select_if
#' @export


automate_factor_plot <- function(df, yVar) {

  y <- df[[yVar]]
  is_cat <- function(x) {is.factor(x) | is.character(x)}
  df <- select_if(df, is_cat)
  df[[yVar]] <- y

  k <- 1
  control_flow <- function(input, k){
    if (input == "") { k <- k + 1 }
    else if (input == "q") { break }
    else if (input == "z") { k <- k - 1 }
    else if (input == "x") { k <- k }
    else if (str_sub(input, 1, 3) == "var") {
      if (str_sub(input, 5, -1) %in% names(df)) { k <- which(names(df)==str_sub(input, 5, -1))}
      else {cat("Wrong variable name")}
    }
    return(k)
  }


  while (k <= ncol(df)) {
    if (k < 1) {
      k <- 1
      cat("This is already the first plot.\n")
    }

    print(k)

    print(factor_plot(df, names(df)[k], yVar, stat = "identity"))
    cat(paste("Average", yVar, "for", names(df)[k]))
    tmp <- readline(prompt = "")

    print(factor_plot(df, names(df)[k], yVar, stat = "count"))
    cat(paste("Count of each", names(df)[k]))
    tmp <- readline(prompt = "")
    input <- readline(prompt = "Press Enter for next plot:  ")

    k <- control_flow(input, k)
  }


  invisible(NULL)
}
