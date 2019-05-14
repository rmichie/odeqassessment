#' Assess TP data
#' 
#' Asssesses TP data against the appropriate criteria and produces columns for excursions
#' @param TP_data
#' @return dataframe with the following columns added; 'excursion_cen'
#' @export
#' @example
#' TP_assessment(TP_data = "your-TP-data")
#' 

TP_assessment <- function(TP_data) {
  TP_summary <- TP_data %>%
    mutate(excursion_cen = ifelse(!is.na(TP_crit) & Result_cen > TP_crit, 1, 0)
    )
  
  return(TP_summary)
}
