#' @title Plot the counts of each category or the average values of y for each category
#' @description
#' @author Jiacheng He
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
