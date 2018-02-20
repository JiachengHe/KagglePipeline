#' @title Automate the factor_plot workflow when we have many categorical variables
#' @description A interactive interface to display the plots one by one. Meanwhile we can write codes in the
#' text editor to modify the categorical variables
#' @author Jiacheng He
#'
#' @param df The data frame
#' @param yVar The name of the target variable
#' @param ylim
#'
#' @importFrom dplyr select_if
#' @importFrom stringr str_sub
#' @export


automate_factor_plot <- function(df, yVar, facVars=NULL, ylim=NULL) {

  if (is.null(facVars)) {
    is_cat <- function(x) {is.factor(x) | is.character(x)}
    facVars <- names(df)[sapply(df, is_cat)]
  }

  control_flow <- function(input, k){
    if (input == "") { k <- k + 1 }
    else if (input == "z") { k <- k - 1 }
    else if (input == "x") { k <- k }
    else if (str_sub(input, 1, 3) == "var") {
      if (str_sub(input, 5, -1) %in% facVars) { k <- which(facVars==str_sub(input, 5, -1))}
      else {cat("Wrong variable name")}
    } else {
      print("Wrong command"); k <- k
    }
    return(k)
  }

  k <- 0

  while (k <= length(facVars)) {

    input <- readline(prompt = "Press Enter for next plot:  ")
    if (input == "q") { break }
    k <- control_flow(input, k)

    if (k < 1) {
      k <- 1
      cat("This is already the first plot.\n")
    }

    print(k)

    print(factor_plot(df, facVars[k], yVar, type = "box", ylim = ylim))
    cat(paste("Average", yVar, "for", facVars[k]))
    tmp <- readline(prompt = "")

    print(factor_plot(df, facVars[k], yVar, type = "count", ylim = ylim))
    cat(paste("Count of each", facVars[k]))
    tmp <- readline(prompt = "")
  }


  invisible(NULL)
}
