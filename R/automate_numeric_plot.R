#' @title Automate the Numeric_plot workflow when we have many numeric variables
#' @author Jiacheng He
#'
#'
#'
#'
#'
#' @export


automate_numeric_plot <- function(df, yVar, ylim=NULL) {

  y <- df[[yVar]]
  train_or_test <- df$train_or_test
  df <- select_if(df, is.numeric)
  df[[yVar]] <- y
  df$train_or_test <- train_or_test


  control_flow <- function(input, k){
    if (input == "") { k <- k + 1 }
    else if (input == "z") { k <- k - 1 }
    else if (input == "x") { k <- k }
    else if (str_sub(input, 1, 3) == "var") {
      if (str_sub(input, 5, -1) %in% names(df)) { k <- which(names(df)==str_sub(input, 5, -1))}
      else {cat("Wrong variable name")}
    } else {
      print("Wrong command")
    }
    return(k)
  }

  k <- 0

  while (k <= ncol(df)) {

    input <- readline(prompt = "Press Enter for next plot:  ")
    if (input == "q") { break }
    k <- control_flow(input, k)

    if (k < 1) {
      k <- 1
      cat("This is already the first plot.\n")
    }

    print(k)

    print(numeric_plot(df, names(df)[k], yVar, type = "scatter", ylim = ylim))
    cat(paste("Scatter plot of", yVar, "with", names(df)[k]))
    tmp <- readline(prompt = "")

    print(numeric_plot(df, names(df)[k], yVar, type = "density", ylim = ylim))
    cat(paste("Density of", names(df)[k]))
    tmp <- readline(prompt = "")

    print(numeric_plot(df, names(df)[k], yVar, type = "hist", ylim = ylim))
    cat(paste("Histogram of", names(df)[k]))
    tmp <- readline(prompt = "")
  }


  invisible(NULL)
}
