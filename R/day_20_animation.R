# Ordinarily I would break this up into separate scripts,
# but I'm keeping it consolidated here for the 
# 30 Day Chart Challenge.

library(tidyverse)
library(osmdata)
library(sf)
library(MetBrewer)
library(magick)
library(glue)
library(showtext)

# The `osmdata` package facilitates OSM overpass queries.
# This is a vector of strings that will be passed as the
# value in a key value pair, with the key always being 
# "highway". Additionally, links are queried appending
# "_link" to the value. More info on these features can be found
# here: https://wiki.openstreetmap.org/wiki/Map_features.

queries <- c("motorway",
             "trunk",
             "primary",
             "secondary",
             "tertiary",
             "residential",
             "service",
             "unclassified")

q <- c(queries, paste(queries, "_link", sep = ""))

# This code chunk is where data is queried from OSM.
# I am saving the different queries because it kept failing
# when I tried to do them all at once.

walk(q, function(x) {
  cat(crayon::cyan(paste("Starting", x, "\n")))
  t <- opq("Milwaukee, Wisconsin") %>%
    add_osm_feature(key = "highway", value = x) %>%
    osmdata_sf()
  
  saveRDS(t, file = paste("data/roads/", x, ".rda", sep = ""))
  cat(crayon::red(paste("Finished", x, "\n")))
})

d_files <- list.files("data/roads")

# I separate the service roads because I want to be able to isolate them
# mainly for the plotting process, i.e. making them different size.

d <- d_files[!str_detect(d_files, "service")]

not_service <- map_df(d, function(x) {
  cat(crayon::cyan(paste("Starting", x, "\n")))
  t <- readRDS(paste("../wi_roads/data/", x, sep = ""))
  cat(crayon::red(paste("Finished", x, "\n")))
  
  if (!is.null(t$osm_lines)) {
    t$osm_lines %>%
      select(geometry)
  } 
})

t <- readRDS("data/roads/service.rda")

s <- t$osm_lines %>%
  select(geometry)

roads <- list(not_service = not_service,
              service = s)

# You'll need city limits, Milwaukee's can be found here:
# https://github.com/Pecners/shapefiles

mke <- st_read("../milwaukee_citylimit/citylimit.shp")

# Clip off roads outside city limits

mke_roads <- map(roads, function(x) {
  r <- st_transform(x, st_crs(mke))
  st_intersection(r, mke)
})

# Set fiserv coords, which will be center of heart

fiserv <- tibble(lat = 43.04561337538446, long = -87.91734328428254) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(., crs =st_crs(mke))

# This is code that will produce a heart polygon

# Font for plot
font_add_google("Teko", "t")
showtext_auto()

walk2(c(5000, 10000, 20000, 30000), c("a", "b", "c", "d"), function(rad, lab) {
  r <- rad
  t <- seq(0, 2 * pi, length.out = 100)
  x <- r * sin(t) ^ 3 + 2556683 # constant for center of heart, set to fiserv
  y <- (13 * r/16) * cos(t) - (5 * r/16) * cos(2 * t) - (2 * r/16) * cos(3 * t) -(r/16) * cos(4 * t) + 388039 # constant
  
  
  # Create heart polygon
  
  heart <- tibble(x = x, y = y) %>%
    st_as_sf(., coords = c("x", "y"), crs = st_crs(mke)) %>%
    summarise(do_union = FALSE) %>%
    st_cast(., to = "POLYGON")
  
  # Set the heart as roads in heart polygon, need to 
  # do it for both service and not service. Everything 
  # oustide heart is white. Colors of course can be changed.
  
  red_roads <- st_intersection(mke_roads$not_service, heart)
  red_service <- st_intersection(mke_roads$service, heart)
  white_roads <- st_difference(mke_roads$not_service, heart)
  white_service <- st_difference(mke_roads$service, heart)

  bigs <- .25 # size of non-service roads
  smalls <- .1 # size of service roads
  
  # set colors
  green <- "#03481C" # bucks green
  cream <- "#F2EDD3" # bucks cream
  blue <- "#027DC4" # bucks blue
  red <- "#C51F31"
  
  p <- red_roads %>%
    ggplot() +
    geom_sf(size = bigs, color = green) +
    geom_sf(data = red_service, size = smalls, color = green) +
    geom_sf(data = white_roads, size = bigs, color = cream) +
    geom_sf(data = white_service, size = smalls, color = cream) +
    coord_sf(xlim = c(2512889.3, 2572866.7)) +
    theme_void() +
    theme(plot.title = element_text(family = "t", color = cream,
                                    hjust = .85, size = 30,
                                    margin = margin(t = 40, b = -60)),
          plot.caption = element_text(family = "t", hjust = .15,
                                      color = alpha(cream, .5), size = 12,
                                      margin = margin(t = -40, b = 40))) +
    labs(title = "#BUCKSINSIX",
         caption = "Graphic by Spencer Schien (@MrPecners)")
    
  
  ggsave(p, filename = glue("plots/h/mke_heart_{lab}.png"), bg = "black", 
         w = 7, h = 9)
})

# Make gif

imgs <- list.files("plots/h", full.names = TRUE)
imgs_order <- imgs[c(1, 1, 2, 3, 4, 1, 1)]
img_list <- lapply(imgs_order, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 5, optimize = TRUE)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "plots/day_22_animation.gif")
