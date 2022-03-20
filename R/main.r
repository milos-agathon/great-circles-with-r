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

  wheat_df <- wheatDF()
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
    resolution = "10",
    ) %>%
    subset(NAME_ENGL!="Antarctica") # get rid of Antarctica 
  
  return(world_shp)
}


map_great_circles <- function(world_shp, crsROBINSON, p) {
  
  world_shp <- get_world()

  crsROBINSON <- "+proj=robin +lon_0=0w"

  pnts <- get_end_coords(end)[[2]] %>%
    group_by(Reporter) %>%
    arrange(desc(Qty))

  lines <- get_great_circles()

p <- ggplot(pnts[1:10, ]) +
    geom_sf(data=world_shp, 
      fill="#063140", 
      color="#18BFF2", 
      size=0.2, 
      alpha=.35) +
    geom_sf(data=lines, 
      aes(size=Qty/1000000, alpha=Qty/1000000), 
      fill="#ff6103",
      #alpha=.35 
      color="#ff6103") +
    geom_sf(data=pnts, 
      aes(size=Qty/1000000), 
      fill="#ff6103", 
      color="#ff6103", 
      alpha = .85, 
      stroke=.25) +
    geom_sf_text(
    aes(
      label = paste0(Reporter, "\n", round(Qty/1000000, 0))),
      size=3,
      vjust=1,
      hjust=1, 
      family = "georg",
      color="#E65602",
      alpha=1,
      nudge_x = c(.5, .5, 1),
      nudge_y = c(-.5, -.5, 0)) +
    coord_sf(crs = crsROBINSON, expand = FALSE) +
    labs(
      y="", 
      subtitle="",
      x = "",
      title="Wheat imports from Russia in 2020",
      caption="©2022 Milos Popovic https://milospopovic.net\nSource: United Nations. 2022. UN comtrade 
      http://comtrade.un.org")+
    scale_size(
      name = "thousands of tonnes", 
      range = c(1, 10), 
      breaks = scales::pretty_breaks(6))+
    scale_alpha(range = c(.35, .85)) +
    guides(
      alpha="none",
      color="none", 
      fill="none", 
      size=guide_legend(
      override.aes = list(fill = NULL, alpha=.85, color="#ff6103"),
      direction = "horizontal",
      keyheight = unit(1.15, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = .5,
      nrow =1,
      byrow = T,
      reverse = F,
      label.position = "top"
        )
      ) +
    theme_minimal() +
    theme(text = element_text(family = "georg"),
      plot.background = element_rect(fill = "#052833", color = NA), 
      panel.background = element_rect(fill = "#052833", color = NA), 
      legend.background = element_rect(fill = "#052833", color = NA),
      legend.position = c(.55, -.05),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#052833", size = 0.1),
      plot.title = element_text(size=22, color="#ff6103", hjust=0.5, vjust=1),
      plot.subtitle = element_text(size=11, color="#7a2b41", hjust=0.5, vjust=0, face="bold"),
      plot.caption = element_text(size=8, color="grey80", hjust=0.15, vjust=0),
      axis.title.x = element_text(size=10, color="grey20", hjust=0.5, vjust=-6),
      legend.text = element_text(size=9, color="#ff6103"),
      legend.title = element_text(size=10, color="#ff6103"),
      strip.text = element_text(size=20, color="#ff6103", face="bold"),
      plot.margin = unit(c(t=1, r=-2, b=.5, l=-2), "lines"),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank())

  return(p)
}
