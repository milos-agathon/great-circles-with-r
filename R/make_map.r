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
