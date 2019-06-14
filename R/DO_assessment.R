#' DO Analysis
#' 
#' Assesses dissolved oxygen data against the relevant standard and calculates DO sat where applicable
#' @param df dataframe with DO data, including temperature and elevation columns
#' @param datetime_column POSIXCT column name containing sample datetimes
#' @param spawn_start_column mm/dd spawn start date (as characters)
#' @param spawn_end_column mm/dd spawn end date (as characters)
#' @param result_column numeric results column name
#' @param temp_column column with numeric temperature values in degrees celsius (NAs for no data)
#' @param elev_column column with numeric elevation values in feet
#' @return a dataframe with relevant Ecoli criteria and excursion variables added
#' @export
#' @example function(df = your_ecoli_data, datetime_column = "sample_datetime")
#' 
DO_assessment <- function(df, datetime_column = "sample_datetime", spawn_start_column = "spawn_start", 
                          spawn_end_column = "spawn_end", result_column = "Result_cen",
                          temp_column = "temperature", elev_column = "ELEV_Ft"){
  
  library(lubridate)
  library(odbc)
  library(glue)
  library(DBI)
  library(zoo)
  # library(IRlibrary)
  
  
  print("Beginning year round analysis")
  
  print("Beginning continuous analysis")
  
  # Year round --------------------------------------------------------------
  
  sample_datetime <- as.symbol(datetime_column)
  spawn_start <- as.symbol(spawn_start_column)
  spawn_end <- as.symbol(spawn_end_column)
  result <- as.symbol(result_column)
  temperature <- as.symbol(temp_column)
  elevation <- as.symbol(elev_column)
  
  df$DO_Class <- LU_DOCode[match(df$DO_code, LU_DOCode$DO_code), "DO_Class"]
  
  # add spawn start and end dates as dates, include indicator if actdate is within spawn
  # add critical period start and end dates, include indicator is actdate is within critperiod
  data <- df %>% filter(Char_Name == "Dissolved oxygen (DO)") %>% 
    mutate(
      # Add columns for Critcal period start and end date
      critstart = mdy(paste0("7/1/",year(sample_datetime) )),
      critend = mdy(paste0("9/30/",year(sample_datetime) )),
      # Append spawn start and end dates with year
      Start_spawn = ifelse(!is.na(spawn_start), paste0(spawn_start,"/",year(sample_datetime)), NA ) ,
      End_spawn = ifelse(!is.na(spawn_end), paste0(spawn_end,"/",year(sample_datetime)), NA ),
      # Make spwnmn start and end date date format
      Start_spawn = mdy(Start_spawn),
      End_spawn = mdy(End_spawn),
      # If Spawn dates span a calendar year, account for year change in spawn end date
      End_spawn = if_else(End_spawn < Start_spawn & sample_datetime >= End_spawn, End_spawn + years(1), # add a year if in spawn period carrying to next year
                          End_spawn), # otherwise, keep End_spawn as current year
      Start_spawn = if_else(End_spawn < Start_spawn & sample_datetime <= End_spawn, Start_spawn - years(1), # subtract a year if in spawn period carrying from previous year
                            Start_spawn), # otherwise, keep Start_spawn as current year
      # Flag for results in spawning and/or critical period
      in_spawn = if_else(sample_datetime >= Start_spawn & sample_datetime <= End_spawn & !is.na(Start_spawn), 1, 0 ),
      is.crit = if_else(sample_datetime >= critstart & sample_datetime <= critend, 1, 0 ))
  
  data <- data %>% 
    filter(Statistical_Base %in% c("30DADMean", "7DADMin", "Minimum", NA)) %>%
    mutate(yr_excursion = if_else(is.na(Statistical_Base) & Result_cen < Do_crit_instant, 1,
                               if_else(Statistical_Base == "30DADMean" & Result_cen < Do_crit_30D, 1, 
                                       if_else(Statistical_Base == "7DADMin" & Result_cen < Do_crit_7Mi, 1, 
                                               if_else(Statistical_Base == "Minimum" & Result_cen < DO_crit_min, 1, 0)))))
  data <- data %>% 
    mutate(spawn_excursion = if_else(in_spawn == 1 & Statistical_Base %in% c("7DADMin", "Minimum", NA) & Result_cen < 11, 1, 0))
  
  # Subset data to DO saturation relevant data
  sat_data_yr_cont <- data %>% filter(Statistical_Base %in% c("30DADMean"), yr_excursion == 1, DO_Class == "Cold Water")
  sat_data_spawn_cont <- data %>% filter(Statistical_Base %in% c("30DADMean", "7DADMin", "Minimum"), spawn_excursion == 1)
  sat_data_yr_inst <- data %>% filter(is.na(Statistical_Base), yr_excursion == 1)
  sat_data_spawn_inst <- data %>% filter(spawn_excursion == 1)
  sat_data_inst <- bind_rows(sat_data_yr_inst, sat_data_spawn_inst)
  sat_data_inst$DO_sat <- mapply(DO_sat_calc, sat_data_inst$Result_cen, sat_data_inst$temperature, sat_data_inst$ELEV_Ft, USE.NAMES = FALSE)
  sat_data_inst$DO_sat <- if_else(sat_data_inst$DO_sat > 100, 100, sat_data_inst$DO_sat)
  sat_data_stns <- unique(c(sat_data_yr$MLocID, sat_data_spawn$MLocID))
  
  data$excursion_cen <- if_else(data$yr_excursion == 1 | data$spawn_excursion == 1, 1, 0)
  
  return(data)
}


#' DO saturation percent calculator
#' 
#' This function will calculate DO saturation percentage 
#' based on DO mg/L values, temperature in C, and elevcation in ft
#' This function is based on the equation found in The Dissolved
#' Ocygen Water Quality Standard Implementatiion Guidence.
#' This function differs from the oxySol function in the wql package
#' because it calcultaes the percentage dirrectly and incorporates elevation,
#' as opposed to pressure
#' 
#' @param DO DO value in mg/L
#' @param TempC Temperature value in C
#' @param elevft Elevation value in feet
#' @export
#' @examples 
#' DO_sat_calc()
#' 

DO_sat_calc <- function(DO, TempC, elevft) {
  DO / (exp(-139.34411 + ((1.575701*10^5)/(TempC+273.15)) - 
              ((6.642308 * 10^7)/((TempC+273.15)^2)) +  
              ((1.243800 * 10^10)/((TempC+273.15)^3)) -
              ((8.621949 * 10^11)/((TempC+273.15)^4))) * 
          (1 - (0.0001148 * elevft/3.281 ))) * 100
}
