library(tidyverse)
library(glue)
library(ggraph)
library(ggtext)
library(tidygraph)
library(MetBrewer)
library(showtext)

# See my day 3 code for how to access this data from IMDb
# Load IMDb data

principals <- read_rds("data/imdb/title.principals.rda")
names <- read_rds("data/imdb/name.basics.rda")
#ratings <- read_rds("data/imdb/title.ratings.rda")
title_basics <- read_rds("data/imdb/title.basics.rda")

# Set Kevin Bacon's id

kevin_bacon <- "nm0000102"

# Filter for only movies

movies <- title_basics %>%
  filter(titleType == "movie") %>%
  .[["tconst"]]

# Get list of all principal cast for all movies

edges <- principals %>%
  filter(category %in% c("actor", "actress") & tconst %in% movies) %>%
  select(to = tconst, 
         from = nconst)

# Get list of Kevin Bacon movies

kb <- edges %>%
  filter(from == kevin_bacon) %>%
  .[["to"]]

# Get list of all principal cast in KB movies

one_degree <- edges %>%
  filter(to %in% kb) %>% 
  .[["from"]] %>%
  unique()

# Get all movies for all cast in KB movies
# i.e. two degrees
m <- edges %>%
  filter(from %in% one_degree) %>%
  .[["to"]] %>%
  unique()

# Get full cast list for two degrees

two <- edges %>%
  filter(to %in% m) %>%
  mutate(is_kb = ifelse(from == kevin_bacon, TRUE, FALSE)) %>%
  arrange(is_kb) %>%
  select(-is_kb)

# Check max and min connections
two %>%
  group_by(from) %>%
  tally() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  filter(n == 1)

# Set up graphs, one smaller for building quickly

e <- as_tbl_graph(tail(two, 1000))
ee <- as_tbl_graph(two)

# Set font and colors
font_add_google("Vollkorn", "v")
showtext_auto()
c <- met.brewer("Hiroshige")


ggraph(ee, layout = "stress") + 
  geom_edge_link(edge_alpha = .25, edge_width = .1, color = c[7]) +
  geom_node_point(aes(color = ifelse(name == kevin_bacon, TRUE, FALSE),
                      size = centrality_pagerank(directed = FALSE)),
                  alpha = .8) +
  scale_size_continuous(range = c(.1, 3),
                        breaks = c(.0005, .0025),
                        labels = c("Fewer connections\n(3,574 have 1)", "More connections\n(Robert de Niro has the most, 92)")) +
  scale_color_manual(values = c(c[10], c[1])) +
  theme(legend.position = "bottom",
        legend.margin = margin(t = -20, b = 10),
        legend.background = element_rect(fill = c[5]),
        legend.key = element_blank(),
        legend.text = element_text(family = "v", color = c[10], size = 12),
        panel.background = element_rect(fill = c[5], color = NA),
        plot.backgroun = element_rect(fill = c[5], color = NA),
        plot.title = element_textbox(hjust = .5, family = "v", size = 30,
                                     color = c[10]),
        plot.subtitle = element_textbox(family = "v", size = 16, lineheight = 1.1,
                                        color = c[10], width = unit(8.75, "in")),
        plot.caption = element_textbox(family = "v", size = 12,
                                       color = alpha(c[10], .5), hjust= .5)) +
  guides(color = "none",
         size = guide_legend(override.aes = list(fill = c[10], shape = 21,
                                                 color = c[10]))) +
  labs(title = glue("Two Degrees of <span style='color:{c[1]}'>**Kevin Bacon**</span>"),
       subtitle = glue("This graphic shows the network of 5,188 actors and actresses ",
                       "who have starred in a movie either with Kevin Bacon or ",
                       "with someone who starred with him."),
       caption = "Graphic by Spencer Schien (@MrPecners) | Data from IMDb",
       size = "")

ggsave("plots/day_17_connections.png", bg = c[5], w = 9, h = 9)
