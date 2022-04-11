library(tidyverse)
library(emojifont)
library(ggbeeswarm)
library(geomtextpath)
library(glue)
library(showtext)
library(ggtext)

# Data source: https://en.wikipedia.org/wiki/List_of_Masters_Tournament_champions#cite_note-scoring-11

d <- read_csv("data/masters_winners.csv")

winners <- d %>%
  mutate(s = ifelse(str_detect(to_par, "\\D"), -1, 1),
         d = as.numeric(str_extract(to_par, "\\d+")),
         good_score = s * d,
         decade = str_sub(year, start = 1, end = 3)) %>%
  select(year,
         decade,
         country,
         champion,
         total_score,
         good_score) %>%
  filter(!is.na(year))

green <- "#067756"
yellow <- "#FCE700"

font_add_google("Libre Caslon Text", "lc")
showtext_auto()

fairs <- winners %>%
  group_by(decade) %>%
  summarise(ymin = min(good_score, na.rm = TRUE) - .2,
            ymax = max(good_score, na.rm = TRUE) + .2,
            avg = mean(good_score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ind = row_number(),
         xmin = ind - .75 / 2,
         xmax = ind + .75 / 2)

winners %>%
  left_join(., fairs %>%
              select(decade, ind)) %>%
  ggplot(aes(ind, good_score)) +
  geom_texthline(yintercept = 0, label = "Even Par",
                 linetype = 2, color = green) +
  geom_rect(data = fairs,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax), 
            inherit.aes = FALSE, fill = alpha(green, .9),
            color = green) +
  geom_segment(data = fairs,
               aes(x = xmin, xend = xmax,
                   y = avg, yend = avg),
               color = yellow, linetype = 3) +
  geom_fontawesome(alias = "fa-flag", size = 4,
             color = yellow, x= fairs$ind, y = fairs$avg,
             vjust = 0) +
  geom_beeswarm(fill = "white", color = green, shape = 21, 
                cex = 1.5) +
  geom_text(data = fairs, aes(label = glue("{decade}0's"),
                              x = xmin + .4, y = ymin, vjust = 1.25),
            inherit.aes = FALSE, color = green, family = "lc", size = 4) +
  annotate(geom = "text", x = 10, y = -13.33, label = "Decade\navg.", 
           color = yellow, vjust = 1.25, family = "lc", size = 3,
           lineheight = .9) +
  theme_minimal() +
  theme(text = element_text(family = "lc"),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = green, 
                                   vjust = c(0, rep(.5, 5))),
        plot.title.position = "plot",
        plot.title = element_textbox(family = "lc", size = 24,
                                     face = "bold", color = yellow,
                                     fill = green, r = unit(2, "pt"),
                                     padding = margin(5,5,2,5),
                                     hjust = .5),
        plot.subtitle = element_textbox(color = green, size = 16, hjust = .5,
                                        halign = .5, width = unit(6.75, "in"),
                                        margin = margin(t = 10, b = 10),
                                        lineheight = 1.2),
        plot.caption.position = "plot",
        plot.caption = element_text(color = alpha(green, .6),
                                    hjust = .5, family = "lc")) +
  scale_y_continuous(breaks = c(1, seq(from = 0, to = -20, by = -5)),
                     labels = c("Strokes\nto par", seq(from = 0, to = -20, by = -5))) +
  labs(x = "", y = "",
       title = "PLAYING AGAINST PAR",
       subtitle = glue("Winning scores for the **Masters Tournament** ",
                       "have ranged from 1 over to 20 under par, with the average close to 9 under."),
       caption = glue("Graphic by Spencer Schien (@MrPecners) | Data from Wikipedia"))

ggsave("plots/day_10_experimental.png", bg = "white",
       w = 9, h = 7.5)
