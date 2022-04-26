###############################################################################
# This code benefited greatly from a blog post                                #
# by Michael Lee (@mkeleeco on Twitter), found                                #
# here: https://www.mikelee.co/posts/2017-06-28-wsj-measles-vaccination-chart/#
#                                                                             #
# Project Tycho is the data source: https://www.tycho.pitt.edu/data/          #
###############################################################################

library(tidyverse)
library(lubridate)
library(glue)
library(ggtext)

d <- read_csv("data/tycho/level1.csv")

measles <- d %>%
  filter(disease == "MEASLES")

annual <- measles %>% 
  mutate(year = str_sub(epi_week, start = 1, end = 4)) %>% 
  group_by(year, state) %>% 
  summarise(total = sum(incidence_per_100000))

cols<- c(
  "#e7f0fa",
  "#c9e2f6",
  "#95cbee",
  "#0099dc",
  "#4ab04a", 
  "#ffd73e", 
  "#eec73a", 
  "#e29421", 
  "#f05336"
)

o <- annual %>%
  ungroup() %>%
  select(state) %>%
  unique() %>%
  arrange(state) %>%
  .[[1]]

annual %>%
  mutate(state = factor(state, levels = rev(o))) %>%
  ggplot(aes(x = year, y = state, fill = total)) +
  geom_tile(width = .9, height = .9) +
  # March 21, 1963 measles vaccine licensed
  geom_segment(x = "1963", xend = "1963",
               y = 0, yend = 51.5) +
  annotate(geom = "text", x = "1963", y = 52.5, label = "Vaccine introduced",
           hjust = 0)+
  theme_minimal() +
  scale_x_discrete(breaks = as.character(seq(from = 1930, to = 2000, by = 10))) +
  scale_fill_gradientn(colours=cols, limits=c(0, 4000),
                       values=c(0, 0.01, 0.02, 0.03, 0.09, 0.1, .15, .25, .4, .5, 1), 
                       na.value=rgb(246, 246, 246, max=255),
                       labels=c("0k", "1k", "2k", "3k", "4k"),
                       guide = guide_colourbar(ticks=T, nbin=50,
                                             title = "Incidence per\n100,000 people",
                                             title.theme = element_text(size = 8, lineheight = .9,
                                                                        margin = margin(r = 10, b = 10),
                                                                        hjust = 1),
                                             barheight=.5, label=T, 
                                             barwidth=10)) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 6),
        plot.title.position = "plot",
        plot.title = element_text(size = 16, face = "bold"),
        plot.caption = element_textbox(hjust = 0, color = "grey60",
                                       width = unit(8, "in"),
                                       lineheight = 1.1),
        plot.caption.position = "plot") +
  labs(title = "Measles incidence in the United States",
       y = "", x = "",
       caption = glue("Data from Project Tycho. ",
                      "Graphic by Spencer Schien (@MrPecners), modeled after a WSJ graphic and ",
                      "aided by a walk-through written by Michael Lee (@mikeleeco)."))

ggsave("plots/day_23_tiles.png", w = 9, h = 6, bg = "white")

