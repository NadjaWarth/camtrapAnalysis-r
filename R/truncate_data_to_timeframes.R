#' Function to truncate data to specific timeframes in a year.
#' If a timeframe covers two years, important is the start of the timeframe.
#' (e.g timeframe: ('12-01', '01-15'), years: (2015, 2018, ...) -> timeframes to analyze: (2015-12-01, 2016-01-15), (2018-12-01, 2019-01-15) ...)
#' 
#' If the Start / End Date of a deployment lie outside the timeframe boundaries, the start / end will be adjusted to be the first / last day of the period. 
#' Sequences will be filtered as well (based on the startDate of the sequence) and the Timespan of a deployment will be estimated based on the adjusted dates. 
#' Deployments that lie entierly outside of the given period will be disregarded
#'
#' @param data Input data: Dataframe with the following collumns expected: StartDate, EndDate, SequDate
#' @param years list of years to analyze within the data. If no years are specified, all occurring years in the data are taken
#' @param timeframes list of timeframes to analyze. A timeframe consists of a (start,end)-tupel in the format ('%m-%d', '%m-%d'). Timeframes should not overlap.
#' 
#' @return list of data for each year (and each timeframe per year)
#'
#' @export
truncate_data_to_timeframes <- function(data, timeframes, years = NULL) {
  # TODO: Checks (optional)
  # Check data
  #check if data is in the right format (e.g if startDate and EndDate exists)
  
  # Check timeframes
  # check if list is not empty, (so at least one timeframe is given) -> error or default c("01-01","12-31")?
  # for all timeframes check:
  # is in right format? (eg. %m-%y) -> error
  # valid dates? (e.g c("40-12", "03-30")) is wrong -> error
  # overlapping timeframes? -> error
  
  # Check years
  # If no years are specified, take all occurring years in the data
  if (is.null(years)) {
    # Extract the year component from the startDate and endDate columns
    start_years <- as.integer(format(data$StartDate, "%Y"))
    end_years <- as.integer(format(data$EndDate, "%Y"))
    # Combine the years and remove duplicates
    years <- unique(c(start_years, end_years))
    # Sort the years in ascending order
    years <- sort(years)
  }
  
  results <- list()  # Create an empty list to store results for each year
  for (year in years) {
    results_per_timeframe <- list()
    for (timeframe in timeframes) {
      timeframe_start <- as.Date(paste(year, timeframe[1], sep = "-"))
      timeframe_end <- as.Date(paste(year, timeframe[2], sep = "-"))
      
      if (timeframe_start > timeframe_end) {
        timeframe_start <- as.Date(paste(year + 1, timeframe[1], sep = "-"))
      }
      
      truncate_deployments_to_timeframe <- data %>%
        #snap deployment start and end dates to the dates of the timeframe
        mutate(
          StartDate = ifelse(StartDate < timeframe_start & EndDate > timeframe_start, timeframe_start, StartDate),
          EndDate = ifelse(StartDate < timeframe_end & EndDate > timeframe_end, timeframe_end, EndDate)
        ) %>%
        mutate(
          StartDate = as.Date(StartDate),
          EndDate = as.Date(EndDate)
        ) %>%
        #filter out all deployments outside the timeframe
        filter(StartDate >= timeframe_start & StartDate <= timeframe_end & 
                 EndDate >= timeframe_start & EndDate <= timeframe_end) %>% 
        #keep only sequences inside the timeframe
        #mutate(SequDate = as.POSIXct(str_split(sequence_interval, " -- ", simplify = TRUE)[, 1])) %>% 
        filter(SequDate >= StartDate & SequDate <= EndDate)
      
      results_per_timeframe[[paste(timeframe[1], "-", timeframe[2])]] <- truncate_deployments_to_timeframe
    }
    results[[paste0(year)]] <- results_per_timeframe
  }
  return(results)
}