library(tidyverse)
library(rayshader)
library(ggridges)
library(showtext)
library(glue)
library(magick)
library(MetBrewer)

# Data source: https://www.ngdc.noaa.gov/mgg/greatlakes/michigan.html
dr <- terra::rast("data/michigan_lld/michigan_lld.tif")
tr_a <- terra::aggregate(dr, fact = 5)
tr_a_df <- as.data.frame(tr_a, xy = TRUE)
colnames(tr_a_df) <- c("long", "lat", "elevation")


blue <- "#085AED"
orange <- "#f2ceb2"
d_orange <- "#ed6808"
font_add_google("Berkshire Swash", "bs")
showtext_auto()


c <- met.brewer("Hokusai2", type = "continuous")

r <- tr_a_df %>%
  filter(elevation < 0) %>%
  mutate(elevation = abs(elevation)) %>%
  ggplot(aes(long, lat, fill = elevation)) +
  geom_raster() +
  scale_fill_gradientn(colors = met.brewer("Hokusai2")) +
  coord_sf() +
  labs(x = "", y = "") +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        axis.text = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        plot.title = element_text(family = "bs", color = c[6],
                                  size = 24, hjust = .5),
        plot.subtitle = element_text(family = "bs", color = c[6],
                                     size = 18, hjust = .5),
        legend.text = element_text(family = "bs", color = c[6]),
        legend.title = element_text(family = "bs", color = c[6]),
        plot.caption = element_text(family = "bs", color = c[6],
                                   size = 6, hjust = .5)) +
  labs(fill = "Depth (m)",
       title = "Mountains of Depth",
       subtitle = "Lake Michigan",
       caption = "Graphic by Spencer Schien (@MrPecners) | Data from NOAA")


plot_gg(r, multicore = TRUE, width=5,height=5, zoom = .4, soliddepth = 0,
        windowsize = c(1400, 866), phi = 30, theta = 30) 

render_snapshot(filename = "plots/day_14_3d.png")

y <- 1
render_movie(filename = glue("plots/gif/day_14_3d_{y}.mp4"))


