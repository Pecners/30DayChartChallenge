library(tidyverse)
library(glue)
library(scales)
library(showtext)
library(ggtext)

# Update IMDb data

update_all <- function(exclude_table = "none") {
  n <- c(
   #"name.basics",
   #"title.akas",
   "title.basics",
   #"title.crew",
   #"title.episode",
   #"title.principals",
   #"title.ratings"
    )
  
  n_only <- n[!stringr::str_detect(n, exclude_table)]
  
  links <- glue::glue("https://datasets.imdbws.com/{n}.tsv.gz")
  
  walk2(links, n_only, function(x, y) {
    temp <- tempfile()
    download.file(x, temp)
    data <- read_tsv(gzfile(temp))
    saveRDS(data, glue("data/imdb/{y}.rda"))
  })
}

update_all()

# Load data

title_basics <- read_rds("data/imdb/title.basics.rda")

year_totals <- title_basics %>%
  filter(titleType %in% "movie") %>%
  group_by(startYear, titleType) %>%
  tally() %>%
  ungroup() %>%
  mutate(year = as.numeric(startYear)) %>%
  filter(year < 2022)

gold <- "#fab348"
red <- "#5c1510"

font_add_google("Frank Ruhl Libre", "frl")
showtext_auto() 

top <- 2250

year_totals %>%
  filter(titleType == "movie") %>%
  ggplot(aes(year, n)) +
  # left vertical segment
  geom_segment(x = 1938.6, xend = 1938.6,
               y = 1876 + 100, yend = top, 
               color = "white", size = .1) +
  # right vertical segment
  geom_segment(x = 1945.4, xend = 1945.4,
               y = 1119 + 100, yend = top, 
               color = "white", size = .1) +
  # horizontal connecting segment 
  geom_segment(x = 1938.6, xend = 1945.4,
               y = top, yend = top, 
               color = "white", size = .1) +
  # vertical connecting segment
  geom_segment(x = 1942, xend = 1942,
               y = top, yend = top + 500, color = "white", size = .1) +
  # WWII annotation
  annotate(geom = "text", x = 1942, y = top + 1000,
           label = "WWII", color = "white", family = "frl") +
  geom_col(fill = gold, width = .75) +
  # Netflix segment
  geom_segment(x = 1997, xend = 1997, yend = 4700, y = 7000,
               color = "white", size = .1,
               arrow = arrow(length = unit(.1, "cm"), type = "closed")) +
  annotate(geom = "text", x = 1997, y = 8000,
           label = "Netflix\nlaunches", color = "white",
           lineheight = .8, family = "frl") +
  # 2020 segment
  annotate(geom = "curve", x = 2010, xend = 2020,
           y = 16250, yend = 15400, color = "white", size = .2,
           curvature = -.5, angle = 30,
           arrow = arrow(length = unit(.1, "cm"), type = "closed"), family = "frl") +
  annotate(geom = "text", x = 2000, y = 16250, label = "Global pandemic\n(2020)",
           color = "white", family = "frl", lineheight = .8) +
  scale_y_continuous(breaks = c(5000, 10000, 15000),
                     labels = comma) +
  scale_x_continuous(breaks = c(1896, 1925, 1950, 1975, 2000, 2021),
                     expand = c(0, 0)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = alpha("white", .25),
                                          size = .5),
        axis.text = element_text(color = alpha("white", .5), 
                                 hjust = c(0, rep(.5, 4), 1), family = "frl"),
        axis.text.y = element_text(color = alpha("white", .5), family = "frl"),
        plot.title.position = "plot",
        plot.title = element_textbox(color = red, family = "frl", size = 24,
                                     fill = gold, padding = margin(5,2,2,2),
                                     r = unit(3, "pt"), box.color = gold),
        plot.subtitle = element_textbox(color = gold, fill = NA, size = 14,
                                        family = "frl"),
        plot.caption.position = "plot",
        plot.caption = element_text(color = alpha("white", .25), family = "frl")) +
  labs(x = "", y = "",
       title = "**Growing the Big Screen**",
       subtitle = "Since 2000, the annual count of movies produced worldwide has increased by over 300%.",
       caption = glue("Analysis and graphic by Spencer Schien (@MrPecners) | Data from IMDb"))

ggsave(filename = "plots/day_3_historical.png", bg = red,
       width = 9, h = 5)
