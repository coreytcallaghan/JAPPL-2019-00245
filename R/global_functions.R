## Script with various functions



## function for seasons in southern hemisphere
getSeason_southern <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Summer",
          ifelse (d >= SE & d < SS, "Autumn",
                  ifelse (d >= SS & d < FE, "Winter", "Spring")))
}

# function for seasons in northern hemisphere
getSeason_northern <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Autumn")))
}

get_season_gam_data <- function(data, season_int){
  season <- dplyr::filter(data, season_num == season_int) %>% .$season_num
  urban_index <- seq(min(data$urban_index), max(data$urban_index), 
                     length.out=nrow(dplyr::filter(data, season_num==season_int)))
  OBSERVER_ID <- sample(data$OBSERVER_ID, nrow(dplyr::filter(data, season_num==season_int)))
  LOCALITY_ID <- sample(data$LOCALITY_ID, nrow(dplyr::filter(data, season_num==season_int)))
  data.new <- data.frame(season = season,
                         DURATION_MINUTES = 60,
                         EFFORT_DISTANCE_KM = 2.5,
                         urban_index = urban_index,
                         OBSERVER_ID = OBSERVER_ID,
                         LOCALITY_ID = LOCALITY_ID)
  data.new
}

## function to get sequence of values for each greenspace
seq_function <- function(data){
  
  list <- data.frame(urban_index = seq(data$min_ui, data$max_ui, length.out=data$N),
                     LOCALITY_ID = data$LOCALITY_ID)
  
  return(list)
}