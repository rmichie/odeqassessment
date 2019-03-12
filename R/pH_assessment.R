#' Assess pH data
#' 
#' Asssesses pH data against the appropriate criteria and produces columns for excursions, including whether they were high or low
#' @param pH_data
#' @return dataframe with the following columns added; 'pH_violation', 'pH_violation_high', 'pH_violation_low.'
#' @export
#' @example
#' pH_assessment(pH_data = "your-pH-data")
#' 

pH_assessment <- function(pH_data) {
  pH_summary <- pH_data %>%
    mutate(pH_excursion = ifelse(Result_cen < pH_Min | Result_cen > pH_Max, 1, 0 ),
           pH_excursion_high = ifelse(Result_cen > pH_Max, 1, 0 ),
           pH_excursion_low = ifelse(Result_cen < pH_Min, 1, 0 )
    )
  
  pH_summary$excursion_cen <- pH_summary$pH_excursion
  
  return(pH_summary)
}
