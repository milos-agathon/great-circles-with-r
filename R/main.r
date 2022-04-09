windowsFonts(georg = windowsFont('Georgia'))

# libraries we need
libs <- c("tidyverse", "sf", "giscoR", "classInt",
  "httr", "CoordinateCleaner")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = TRUE))

# 1. GET TRADE DATA
#---------
# query from Comtrade API
# grab text and return a dataframe

get_wheatDF <- function(res, data, wheat_df) {
  res <- GET("http://comtrade.un.org/api/get?type=C&freq=A&px=HS&ps=now&r=all&p=643&rg=1&cc=1001&fmt=csv")
  data <- content(res, 'text')
  wheat_df <- read.csv(text = data) %>%
    dplyr::select(c(10:11, 26)) %>%
    rename(iso3 = Reporter.ISO)
  return(wheat_df)
}

# 2. FETCH CAPITALS
#---------
get_capitals_coords <- function(cities, capitals) {
  data(countryref) 

  towns <- countryref

  capitals <- towns %>% 
    filter(!is.na(capital)) %>%
    group_by(iso3, capital) %>%
    summarise_at(vars(capital.lon, capital.lat), max) %>%
    rename(long = capital.lon, lat = capital.lat)

  return(capitals)
}

# 3. CREATE STARTING/ENDING POINTS
#---------
get_end_coords <- function(wheat_df, capitals, end, end_coords) {

  wheat_df <- get_wheatDF()
  capitals <- get_capitals_coords()

  end <- capitals %>% 
    filter(iso3!="RUS") %>%
    rename(end_lat = lat, end_long = long)

  end_coords <- end %>%
    sf::st_as_sf(coords = c("end_long","end_lat")) %>%
    sf::st_set_crs(4326) %>% 
    left_join(wheat_df, by = "iso3")  %>% 
    filter(!is.na(Qty))

  return(list(end, end_coords))
}


get_coords <- function(capitals, end, start, cap) {

  end <- get_end_coords(end)[[1]]
  capitals <- get_capitals_coords() 

  start <- capitals %>% 
    filter(iso3=="RUS") %>%
    rename(start_lat = lat, start_long = long) %>%
    group_by(iso3) %>% 
    slice(rep(1:max(nrow(end)), each=max(nrow(end)))) %>%
    ungroup() %>%
    dplyr::select(start_long, start_lat)

  cap <- as.data.frame(cbind(start, end))

  return(cap)
}


# 4. GENERATE GREAT CIRCLES
#---------

get_great_circles <- function(wheat_df, coords, lines) {

  wheat_df <- get_wheatDF()
  coords <- get_coords()

  lines <- coords %>%
    select(start_long,
      start_lat,
      end_long,
      end_lat) %>%
    transpose() %>%
    map(~ matrix(flatten_dbl(.), nrow = 2, byrow = TRUE)) %>%
    map(st_linestring) %>%
    st_sfc(crs = 4326) %>%
    st_sf(geometry = .) %>%
    bind_cols(coords) %>%
    select(iso3, capital, geometry) %>% 
    left_join(wheat_df, by = "iso3") %>% 
    filter(!is.na(Qty))

  return(lines)
}


# 5. RETURN WORLD SPATIAL OBJECT
#---------

get_world <- function(world_shp) {
  
  world_shp <- giscoR::gisco_get_countries(
    year = "2016",
    epsg = "4326",
    resolution = "10"
    ) %>%
    subset(NAME_ENGL!="Antarctica") # get rid of Antarctica 
  
  return(world_shp)
}

# 6. MAP
#---------
map_url <- "https://raw.githubusercontent.com/milos-agathon/great-circles-with-r/main/R/make_map.r"
source(map_url) # load script
map <- map_great_circles()

