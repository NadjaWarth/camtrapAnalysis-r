calc_capture_rates <- function(data, timeframes, years_to_analyze = None) {
  # 1. truncate_data_to_timeframes(data, timeframes, years_to_analyze)
  # foreach timeframe: (NOTE: Can use lapply to apply that without a for loop)
  # 2. calc unweighted capture_rate
  # 3. calc weighted capture_rate
}

#' Function to calculate the unweighted capture rate within specific timeframes in a year. 
#' Function works similar to estimate_effort
#' 
#' Unweighted Capture Rate: Number of observations per species in a given location divided by the number of observations 
#  capture rate / effort (overall, does not consider different lengths of different deployments)
#' 
#' @param data Input data: Dataframe with the following collumns expected: ...
#' @param years list of years to analyze within the data. If no years are specified, all occurring years in the data are taken
#' @param timeframes list of timeframes to analyze. A timeframe consists of a (start,end)-tupel in the format ('%m-%d', '%m-%d'). Timeframes should not overlap.
#' 
#' @return list of results for each year and timeframe
calc_unweighted_capture_rates_within_timeframes <- function(data, timeframes, years = NULL) {
  return()
}


#' Calculate the weighted capture rates based on the data created by estimate_effort_within_timeframes
#' 
#' @param effort_data expected columns: deploymentID, scientificName, effort, locationName
#' 
#' @return dataframe with cr_weighted collumn containing the weighted capture rate
calc_weighted_capture_rates_based_on_effort <- function(effort_data) {
  return()
}