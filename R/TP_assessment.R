#' Assess TP data
#'
#' Asssesses TP data against the appropriate criteria and produces columns for excursions
#' @param TP_data dataframe with TP data and criteria values
#' @return dataframe with the following columns added; 'excursion_cen'
#' @export
#' @examples
#' TP_assessment(TP_data = "your-TP-data")
#'

TP_assessment <- function(TP_data) {
  TP_summary <- TP_data %>%
    mutate(excursion_cen = if_else(!is.na(TP_crit),
                                   if_else(Result_cen > TP_crit, 1, 0),
                                   NaN)
    )

  return(TP_summary)
}
