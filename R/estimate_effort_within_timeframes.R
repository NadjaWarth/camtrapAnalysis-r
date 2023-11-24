#' Function to estimate the effort within specific timeframes in a year.
#' If a timeframe covers two years, important is the start of the timeframe.
#' (e.g timeframe: ('12-01', '01-15'), years: (2015, 2018, ...) -> timeframes to analyze: (2015-12-01, 2016-01-15), (2018-12-01, 2019-01-15) ...)
#' 
#' If the Start / End Date of a deployment lie outside the timeframe boundaries, the start / end will be adjusted to be the first / last day of the period. 
#' Sequences will be filtered as well (based on the startDate of the sequence) and the Timespan of a deployment will be estimated based on the adjusted dates. 
#' Deployments that lie entierly outside of the given period will be disregarded
#'
#' Effort = detection distance (m) * timespan (days)
#'
#' @param data Input data: Dataframe with the following collumns expected: StartDate, EndDate, SequDate, Detection_Distance.
#' @param years list of years to analyze within the data. If no years are specified, all occurring years in the data are taken
#' @param timeframes list of timeframes to analyze. A timeframe consists of a (start,end)-tupel in the format ('%m-%d', '%m-%d'). Timeframes should not overlap.
#' 
#' @return list of results for each year and timeframe
#'
#' @examples
#' \dontrun{
#' # define test_data
#' test_data <- data.frame(
#'   deploymentID = c("06ee9610-93c3-4de5-ade7-0a507b94ad71","06ee9610-93c3-4de5-ade7-0a507b94ad71","044c34ac-7082-4240-9457-ac865aa55fcc", "044c34ac-7082-4240-9457-ac865aa55fcc", "0009d0dc-6337-47d8-8bb4-10da7bb786b4"),
#'   StartDate = as.Date(c("2015-01-15", "2015-03-05", "2016-03-10", "2019-08-20", "2020-02-05"), format = "%Y-%m-%d"),
#'   EndDate = as.Date(c("2015-02-28", "2015-05-07","2016-11-30", "2019-12-31", "2020-12-31"), format = "%Y-%m-%d"),
#'   SequDate = c(as.POSIXct(strptime("2015-01-17 01:30:00", "%Y-%m-%d %H:%M:%S")), as.POSIXct(strptime("2015-03-05 06:30:00", "%Y-%m-%d %H:%M:%S")), as.POSIXct(strptime("2016-04-12 01:30:00", "%Y-%m-%d %H:%M:%S")),as.POSIXct(strptime("2019-08-20 01:30:00", "%Y-%m-%d %H:%M:%S")),as.POSIXct(strptime("2020-02-27 01:30:00", "%Y-%m-%d %H:%M:%S"))),
#'   Detection_Distance = c(10, 5, 15, 10, 20)
#'   )
#' # define timeframes
#' timeframes <- list(c('02-01', '04-30'), c('05-01', '08-31'), c('09-01', '12-31'))
#' 
#' # (optional) define years
#' years <- c(2016,2017,2018)
#' 
#' results <- estimate_effort_within_timeframe(data, years, timeframes)
#' }
#' @export
estimate_effort_within_timeframes <- function(data, timeframes, years = NULL) {
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
      # Calculate effort per deployment based on first/last sequence and deployment duration
      
      effort_per_deployment <- mutate(truncate_deployments_to_timeframe,
                                      time_span_days = as.numeric(difftime(EndDate, StartDate, units = "days")),
                                      effort = time_span_days * Detection_Distance
      ) %>%
        # Remove all NA rows (false detections)----- still necessary?
        drop_na()
      #filter unneccessary columns?
      
      results_per_timeframe[[paste(timeframe[1], "-", timeframe[2])]] <- effort_per_deployment
    }
    results[[paste0(year)]] <- results_per_timeframe
  }
  return(results)
}