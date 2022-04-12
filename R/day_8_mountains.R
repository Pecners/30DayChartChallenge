library(tidyverse)
library(ggridges)
library(terra)
library(ggtext)
library(showtext)

# Data source: https://www.ngdc.noaa.gov/mgg/greatlakes/michigan.html
dr <- terra::rast("data/michigan_lld/michigan_lld.tif")
tr_a <- terra::aggregate(dr, fact = 5)
tr_a_df <- as.data.frame(tr_a, xy = TRUE)
colnames(tr_a_df) <- c("long", "lat", "elevation")

f <- tr_a_df %>%
  mutate(long_round = round(long, 3),
         lat_round = round(lat, 2)) %>%
  group_by(long_round, lat_round) %>%
  mutate(avg_elevation = mean(elevation, na.rm = TRUE),
         rev_elev = avg_elevation * -1) %>%
  ungroup()

ff <- tr_a_df %>%
  mutate(long_round = round(long, 3),
         lat_round = round(lat, 1)) %>%
  group_by(long_round, lat_round) %>%
  mutate(avg_elevation = mean(elevation, na.rm = TRUE),
         rev_elev = avg_elevation * -1) %>%
  ungroup()


blue <- "#085AED"
orange <- "#f2ceb2"
d_orange <- "#ed6808"
font_add_google("Berkshire Swash", "bs")
showtext_auto()


small <- f %>%
  ggplot(aes(x = long, y = lat_round,
             height = rev_elev, group = lat_round)) +
  geom_density_ridges(stat = "identity",
                      fill = blue,
                      alpha = .75,
                      color = NA,
                      scale = 2) +
  annotate(geom = "text", x = -85.5, y = 44, label = "Lake Michigan\nMountains of Depth",
           family = "bs", size = 8, color = "#0846b8") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(.01, .01)) +
  coord_sf() +
  theme_void() +
  theme(plot.caption = element_text(color = alpha(blue, .5),
                                    margin = margin(t = -20, r = 10, b = 10),
                                    family = "bs")) +
  labs(caption = "Graphic by Spencer Schien (@MrPecners) | Data from NOAA")


big <- ff %>%
  ggplot(aes(x = long, y = lat_round,
             height = rev_elev, group = lat_round)) +
  geom_density_ridges(stat = "identity",
                      fill = blue,
                      alpha = .75,
                      color = NA,
                      scale = 2) +
  annotate(geom = "text", x = -85.5, y = 44, label = "Lake Michigan\nMountains of Depth",
           family = "bs", size = 8, color = "#0846b8") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(.01, .01)) +
  coord_sf() +
  theme_void() +
  theme(plot.caption = element_text(color = alpha(blue, .5),
                                    margin = margin(t = -20, r = 10, b = 10),
                                    family = "bs")) +
  labs(caption = "Graphic by Spencer Schien (@MrPecners) | Data from NOAA")

ggsave(small, filename = "plots/day_8_mountains_small.png", bg = orange,
       h = 9, w = 7)

ggsave(big, filename = "plots/day_8_mountains_big.png", bg = orange,
       h = 9, w = 7)
