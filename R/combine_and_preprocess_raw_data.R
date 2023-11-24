combine_and_preprocess_raw_data <- function(ctdp_data, depl_hist_raw, data_WildID = None) {
  ### Preprocess the dataframes, TODO: More General, Check and Catch Errors in Data etc. 
  
  #Make depl_hist_raw compatible with camera trapping data and select relevant variables 
  depl_hist<- depl_hist_raw %>%
    rename(locationName = LocationID) %>%  # rename LocationID column to match with the camera trap data
    mutate(locationName = ifelse(locationName == "BCI-3-11.2", "BCI-3-11", locationName)) %>% 
    select(locationName, SetupDate, Detection_Distance) %>% 
    filter(!is.na(Detection_Distance) & !str_detect(Detection_Distance, "[^0-9.]")) %>%   # Keep rows where Detection_Distance is not NA & filter out rows where Detection_Distance contains non-numeric characters.
    mutate(Detection_Distance = as.numeric(Detection_Distance)) %>% 
    mutate(Detection_Distance = ifelse(Detection_Distance == 1050, 10.50, Detection_Distance)) %>%  # one value was at 1050 --> probably a missed comma
    mutate(StartDate = as.Date(SetupDate)) %>% 
    select(-c("SetupDate"))
  #Check for outliers (for example missed comma etc.)
  filter(depl_hist, depl_hist$Detection_Distance > 15)
  
  # Combine relevant data from Agouti 
  merged_Agouti <- 
    #Create a big data table with all the columns by merging the "subtibbles"
    merge_tibbles(Agouti, dropMedia = TRUE) %>% 
    #Select relevant Columns
    select(locationName, deploymentID, deployment_interval,
           sequence_interval, count, scientificName, vernacularNames.en, class, longitude, latitude) %>%
    #Make a new column for the Setup date (key to join with deployment history data) and End date of each deployment 
    separate(deployment_interval, into = c("StartDate", "EndDate"), sep = "--", convert = TRUE) %>%  
    mutate(StartDate = as.Date(StartDate), EndDate = as.Date(EndDate)) %>% 
    #Regenerate deployment Interval: 
    #mutate(deployment_interval = paste(StartDate, EndDate, sep = "--")) %>%
    #Create a column with a Sequence data instead of interval (needed later to filter out sequences)
    mutate(SequDate = as.POSIXct(str_split(sequence_interval, " -- ", simplify = TRUE)[, 1])) %>% 
    #Drop all rows that contain NA, could e.g. be observations that were categorized as "unkown"
    drop_na()
  
  # Combine detection distance from deployment history sheet with Agouti data
  data_cam_dd <- 
    #Add detection distance from deployment history data 
    merge(merged_Agouti, depl_hist, by = c("locationName", "StartDate"))
  
  return(data_cam_dd)
}