#' @title Plot the counts of each category or the average values of y for each category
#' @description Provide guidence of how to process the factor variable in the next steps (e.g. We might
#' want to get rid of the categories with rare frequencey, or combine the categories with similar average y)
#' @author Jiacheng He
#'
#' @param df The data frame which contains the variables
#' @param facVar The name of the factor variable
#' @param yVar The name of the left-hand-side target variable
#' @param type type="identity" to plot average y; type="count" to plot the frequency
#' @param ylim
#'
#' @import ggplot2
#' @importFrom dplyr '%>%'
#' @importFrom dplyr filter
#' @export
#'


factor_plot <- function(df, facVar, yVar=NULL, type="box", ylim=NULL) {

  if (type == "box") {

    if (is.numeric(df[[yVar]])) {

      df %>%
        filter(train_or_test == "train") %>%
        ggplot(aes_string(facVar, yVar)) +
        geom_boxplot() +
        xlab(facVar) +
        ylab(yVar) +
        coord_cartesian(ylim = ylim)

    } else if (is.factor(df[[yVar]])) {

      df[[yVar]] <- as.integer(as.character(df[[yVar]]))

      df %>%
        filter(train_or_test == "train") %>%
        group_by_(facVar) %>%
        summarize_at(yVar, mean, na.rm=TRUE) %>%
        ggplot(aes_string(facVar, yVar)) +
        geom_bar(stat = "identity") +
        xlab(facVar) +
        coord_cartesian(ylim = ylim)
    }



  } else if (type == "count") {

    df %>%
      ggplot(aes_string(facVar)) +
      geom_bar(aes(fill = train_or_test), position = "identity", alpha = 0.5) +
      xlab(facVar)
  }

}
