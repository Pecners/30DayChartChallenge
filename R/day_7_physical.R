library(tidyverse)
library(osmdata)
library(tigris)
library(sf)
library(glue)
library(showtext)

counties <- counties(state = "WI")

states <- states()

c_names <- counties %>%
  as_tibble() %>%
  .[["NAMELSAD"]]


done <- str_remove(list.files("data/water"), ".rds$")

to_do <- c_names[which(!c_names %in% done)]

walk(to_do, function(x) {
  
  cat(crayon::cyan(paste("Starting", x, "\n")))
  d <- opq(glue("{x}, Wisconsin")) %>%
    add_osm_feature(key = "waterway") %>%
    osmdata_sf()
  
  saveRDS(d$osm_lines, glue("data/water/{x}.rds"))
})

# Load data ---------------------------------------------------

water <- map_df(list.files("data/water"), function(x) {
  cat(crayon::cyan(paste("Starting", x, "\n")))
  d <- read_rds(glue("data/water/{x}"))
})

wi <- states %>%
  filter(NAME == "Wisconsin") %>%
  st_transform(., crs = st_crs(water))

l <- rnaturalearth::ne_download(type = "lakes", category = "physical", scale = "large")  %>%
  st_as_sf(., crs = st_crs(4326))

gl <- l %>% 
  filter(name %in% c("Lake Michigan", "Lake Superior")) %>%
  st_union()

wi_trim <- st_difference(wi, gl)

wi_water <- st_intersection(water, wi)

sam <- sample(1:nrow(wi_water), 1000, replace = FALSE)

wi_rivers <- wi_water %>%
  filter(waterway == "river" & 
           st_geometry_type(geometry) %in% c("LINESTRING",
                                            "MULTILINESTRING",
                                            "GEOMETRYCOLLECTION")) %>%
  select(geometry)

only_rivers <- map_df(1:nrow(wi_rivers), function(x) {
  d <- wi_rivers[x,]
  t <- st_geometry_type(d)
  
  if (!str_detect(t, "LINESTRING")) {
    d[1,"geometry"] <- st_collection_extract(d$geometry)
  }
  
  return(d)
})

blue <- "#9CC0F9"
beige <- "#FEF8DF"
d_beige <- "#f0e8ce"
d_blue <- "#8eaee0"

font_add_google("Tangerine", "t")
showtext_auto()


r_plot <- only_rivers %>%
  ggplot() +
  geom_sf(data = wi_trim, fill = d_beige, color = NA) +
  geom_sf(size = .3, color = blue) +
  theme_void() +
  theme(plot.title = element_text(size = 50, hjust = 1,
                                  margin = margin(t = 10, b = -70),
                                  family = "t",
                                  color = d_blue, face = "bold"),
        plot.caption = element_text(family = "t", hjust = 0, 
                                    color = alpha(d_blue, .75),
                                    margin = margin(t = -50, b = 10),
                                    size = 12)) +
  labs(title = "Wisconsin Rivers",
       caption = "Data from OpenStreetMap\nGraphic by Spencer Schien(@MrPecners)")

ggsave(r_plot, filename = "plots/day_7_physical.png", bg = beige)
