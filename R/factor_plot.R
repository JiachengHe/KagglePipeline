#' @title Plot the counts of each category or the average values of y for each category
#' @description Provide guidence of how to process the factor variable in the next steps (e.g. We might
#' want to get rid of the categories with rare frequencey, or combine the categories with similar average y)
#' @author Jiacheng He
#'
#' @param df The data frame which contains the variables
#' @param facVar The name of the factor variable
#' @param yVar The name of the left-hand-side target variable
#' @param stat stat="identity" to plot average y; stat="count" to plot the frequency
#'
#' @import rlang
#' @import ggplot2
#' @importFrom dplyr '%>%'
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
#' @export
#'


factor_plot <- function(df, facVar, yVar=NULL, stat="identity") {

  # facVar <- enquo(facVar)
  # yVar <- enquo(yVar)

  if (stat == "identity") {

    df %>%
      group_by_(facVar) %>%
      summarize_at(yVar, function(x){mean(x, na.rm = TRUE)}) %>%
      ggplot(aes_string(facVar, yVar)) +
      geom_bar(stat = "identity") +
      xlab(facVar) +
      ylab(yVar)

  } else if (stat == "count") {

    df %>%
      #select(fac = UQ(facVar)) %>%
      ggplot(aes_string(facVar)) +
      geom_bar() +
      xlab(facVar)

  }

}
