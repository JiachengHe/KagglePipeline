#' @title Plot the counts of each category or the average values of y for each category
#' @description Provide guidence of how to process the factor variable in the next steps (e.g. We might
#' want to get rid of the categories with rare frequencey, or combine the categories with similar average y)
#' @author Jiacheng He
#'
#' @param df The data frame which contains the variables
#' @param facVar The factor variable
#' @param yVar The left-hand-side target variable
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

  facVar <- enquo(facVar)
  yVar <- enquo(yVar)

  if (stat == "identity") {

    df %>%
      rename(fac = UQ(facVar), y = UQ(yVar)) %>%
      group_by(fac) %>%
      summarize(y = mean(y, na.rm = TRUE)) %>%
      ggplot(aes(fac, y)) +
      geom_bar(stat = "identity") +
      xlab(quo_text(facVar)) +
      ylab(quo_text(yVar))

  } else if (stat == "count") {

    df %>%
      select(fac = UQ(facVar)) %>%
      ggplot(aes(fac)) +
      geom_bar() +
      xlab(quo_text(facVar))

  }

}
