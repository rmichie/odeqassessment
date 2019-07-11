#' Bacteria Shell Harvest Analysis
#'
#' Assesses Ecoli data against the standard
#' @param df dataframe with Ecoli data
#' @param criteria POSIXCT column name containing criteria values
#' @return a dataframe with relevant Ecoli criteria and excursion variables added
#' @export
#' @examples function(df = your_ecoli_data, criteria = "bact-shell-crit")

Shell_Harvest <- function(df, criteria = "bact_crit_percent") {

  print("Begin shellfish harvesting analysis")

  Perc_Crit <- as.symbol(criteria)

  shell_harvest <- df %>%
    filter(BacteriaCode == 3,
           Char_Name == "Fecal Coliform") %>%
    mutate(perc_exceed = ifelse(Result_cen > !!Perc_Crit, 1, 0))

  if(nrow(shell_harvest) == 0) {
    stop("No available data")
  }

  shell_harvest_analysis <- shell_harvest %>%
    group_by(MLocID, OWRD_Basin) %>%
    summarise(num_samples = n(),
              median = ifelse(num_samples >= 5, median(Result_cen), NA ),
              num_exceed = sum(perc_exceed),
              Perc_Crit = first(!!Perc_Crit),
              SS_Crit = first(SS_Crit)
    )

  return(shell_harvest_analysis)

}
