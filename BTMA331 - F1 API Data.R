


library(httr)
library(jsonlite)

# Function to fetch and process race results for a given season with pagination
get_all_race_results <- function(season) {
  # Initialize an empty data frame to store all results
  all_race_results <- data.frame()
  
  # Start with offset = 0
  offset <- 0
  limit <- 30  # Number of results per page
  
  repeat {
    # Construct the URL for the API request with pagination
    race_results_url <- paste0("https://ergast.com/api/f1/", season, "/results.json?limit=", limit, "&offset=", offset)
    
    # Fetch data from the API
    race_results_response <- GET(race_results_url)
    race_results_data <- fromJSON(content(race_results_response, "text"))
    
    # Extract the races and results from the API response
    races <- race_results_data$MRData$RaceTable$Races
    
    # If no races are returned, we are done fetching data
    if (length(races) == 0) {
      break
    }
    
    # Loop over each race and process the results
    for (i in 1:nrow(races)) {
      race <- races[i, ]  # Get the current race row
      
      race_info <- data.frame(
        Season = race$season,
        Round = race$round,
        RaceName = race$raceName,
        Date = race$date,
        Location = paste(race$Circuit$Location$locality, race$Circuit$Location$country),
        CircuitName = race$Circuit$circuitName,
        RaceURL = race$url
      )
      
      # Check if 'Results' exists for the current race
      if (!is.null(race$Results) && length(race$Results) > 0) {
        results_list <- race$Results
        
        # Iterate through each result in the results list (which is a data frame)
        for (j in 1:length(results_list)) {
          results <- results_list[[j]]  # Extract the data frame for the current race
          
          # Process the results for each driver in the race
          for (k in 1:nrow(results)) {
            result <- results[k, ]
            
            # Extract and clean the relevant details
            result_data <- cbind(race_info, 
                                 Position = result$positionText, 
                                 Driver = result$Driver$familyName, 
                                 Constructor = result$Constructor$name,
                                 Points = result$points)
            
            # Append the result to the cleaned data frame
            all_race_results <- rbind(all_race_results, result_data)
          }
        }
      } else {
        cat("No results for race:", race$raceName, "\n")
      }
    }
    
    # Increase the offset by the limit to fetch the next batch
    offset <- offset + limit
  }
  
  return(all_race_results)
}

# Fetch all race results for the 2023 season
race_results_2023 <- get_all_race_results(2023)

# Check if data was successfully fetched and save to CSV
if (nrow(race_results_2023) > 0) {
  write.csv(race_results_2023, "f1_race_results_2023.csv", row.names = FALSE)
} else {
  cat("No race results to save for 2023.\n")
}

# Optionally, view the first few rows of the data
head(race_results_2023)



# Fetch race results for 2024 season
race_results_2024 <- get_all_race_results(2024)

# Check if data was successfully fetched and save to CSV
if (nrow(race_results_2024) > 0) {
  write.csv(race_results_2024, "f1_race_results_2024.csv", row.names = FALSE)
} else {
  cat("No race results to save for 2024.\n")
}

# Optionally, view the first few rows of the data
head(race_results_2024)












# Function to fetch and process driver information for a given season
get_driver_info <- function(season) {
  # URL for the API to fetch driver information for a season
  driver_info_url <- paste0("https://ergast.com/api/f1/", season, "/drivers.json")
  
  # Fetch data from the API
  driver_info_response <- GET(driver_info_url)
  driver_info_data <- fromJSON(content(driver_info_response, "text"))
  
  # Extract the driver details from the API response
  drivers <- driver_info_data$MRData$DriverTable$Drivers  # This is now a data frame
  
  # Initialize an empty data frame to store driver details
  driver_details <- data.frame()
  
  # Loop over each driver and extract the necessary details
  for (i in 1:nrow(drivers)) {
    driver <- drivers[i, ]  # Get the current driver info
    
    driver_info <- data.frame(
      Season = season,
      DriverID = driver$driverId,
      DriverCode = driver$code,
      GivenName = driver$givenName,
      FamilyName = driver$familyName,
      DateOfBirth = driver$dateOfBirth,
      Nationality = driver$nationality,
      WikipediaURL = driver$url
    )
    
    # Get constructor (team) info associated with this driver (from a separate endpoint)
    constructor_url <- paste0("https://ergast.com/api/f1/", season, "/drivers/", driver$driverId, "/constructors.json")
    constructor_response <- GET(constructor_url)
    constructor_data <- fromJSON(content(constructor_response, "text"))
    
    # Check if constructor data is available and if it is in the expected format
    if (!is.null(constructor_data$MRData$ConstructorTable$Constructors) &&
        nrow(constructor_data$MRData$ConstructorTable$Constructors) > 0) {
      
      constructor <- constructor_data$MRData$ConstructorTable$Constructors[1, ]
      
      # Safely access the constructor data from the data frame
      driver_info$Constructor <- constructor$name
      driver_info$ConstructorURL <- constructor$url
    } else {
      # If constructor data is missing or in an unexpected format, assign NA
      driver_info$Constructor <- NA
      driver_info$ConstructorURL <- NA
    }
    
    # Append the driver info to the data frame
    driver_details <- rbind(driver_details, driver_info)
  }
  
  return(driver_details)
}

# Fetch driver info for 2023 season and 2024 season
driver_info_2023 <- get_driver_info(2023)
driver_info_2024 <- get_driver_info(2024)

# Combine the data for both seasons
all_driver_info <- rbind(driver_info_2023, driver_info_2024)

# Check if data was successfully fetched and save to CSV
if (nrow(all_driver_info) > 0) {
  write.csv(all_driver_info, "f1_driver_info_2023_2024.csv", row.names = FALSE)
} else {
  cat("No driver data to save.\n")
}

# Optionally, view the first few rows of the data
head(all_driver_info)


















