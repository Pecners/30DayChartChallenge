library(tidyverse)
library(MetBrewer)
library(ggtext)
library(glue)
library(showtext)

data <- read_csv("data/pulitzer_winners_fiction.csv")

year <- c(1918:2021)
author_sex <- c("m", "f")
full <- expand.grid(year = year, author_sex = author_sex)

roll_sex <- data %>%
  filter(!is.na(winner)) %>%
  right_join(., full) %>%
  mutate(dummie = ifelse(is.na(winner), 0, 1)) %>%
  arrange(year) %>%
  group_by(author_sex) %>%
  mutate(roll = cumsum(dummie),
         author_sex = ifelse(author_sex == "m", "male", "female"))



labs <- roll_sex %>%
  filter(year == max(year)) %>%
  mutate(lab = glue("{roll} {author_sex} wins"))

max_gap <- map_df(unique(roll_sex$author_sex), function(x) {
  t <- roll_sex %>%
    filter(author_sex == x)
  
  m <- map_dbl(2:nrow(t), function(y) {
    t[[y, "year"]] - t[[(y-1), "year"]]
  })
  
  tibble(
    author_sex = x,
    max = max(m)
  )
})

colors <- met.brewer("Moreau")

recent <- roll_sex %>%
  filter(year > 1960) %>%
  group_by(author_sex) %>%
  filter(year == min(year) | year == max(year))


ar <- "grey80"

font_add_google("Spectral", "spec")
showtext_auto()
showtext_opts(dpi = 200)

p <- roll_sex %>%
  ggplot(aes(year, roll, color = author_sex)) +
  annotate(geom = "rect", xmin = 1942, xmax = 1961,
            ymin = -Inf, ymax = Inf, color = NA, 
           fill = "grey90") +
  geom_line() +
  geom_line(data = recent, linetype = 3) +
  geom_text(data = labs, aes(x = year + 1, label = lab),
            hjust = 0, vjust = .25, family = "spec") +
  geom_textbox(x = 1951.5, y = 50, fill = "grey90", halign = .5,
               label = "No female winner<br>1942-1961", box.color = NA,
               color = "grey60", family = "spec", size = 2.75,
               width = unit(1, "in"), box.padding = margin(0,0,0,0)) +
  # Female arrow
  annotate(geom = "curve", x = 1990, xend = 1983,
           y = 37, yend = 20, angle = 66,
           arrow = arrow(length = unit(.1, "cm"), type = "closed"),
           color = ar) +
  geom_textbox(x = 1990, y = 37, label = glue("Since 1961, men have won twice ",
                                              "for every one win by a woman."),
               color = "grey50", width = unit(1.75, "in"), box.color = NA,
               hjust = 0, box.margin = margin(0,-2,0,0), 
               box.padding = margin(0, 0, 0, 2),
               family = "spec", size = 2.75) +
  # Male arrow
  annotate(geom = "curve", x = 2017, xend = 2015,
           y = 38, yend = 59, angle = 66,
           arrow = arrow(length = unit(.1, "cm"), type = "closed"),
           color = ar) +
  scale_x_continuous(limits = c(NA, 2035),
                     breaks = c(seq(1920, 2020, by = 20))) +
  scale_color_manual(values = colors[c(5,3)]) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_textbox(family = "spec", size = 24, hjust = .5,
                                     margin = margin(0,0,0,0)),
        plot.subtitle = element_textbox(family = "spec", width = unit(8.5, "in"),
                                        lineheight = 1.1, size = 13,
                                        margin = margin(10, 0, 10, 0)),
        plot.caption = element_textbox(color = "grey60", hjust = 0, family = "spec",
                                       width = unit(8.5, "in"), lineheight = 1.1),
        axis.text.y = element_blank()) +
  labs(title = "**Pulitzer's Unbalanced Trend**",
       subtitle = glue("The Pulitzer Prize for Fiction ",
                    "has lacked gender parity since World War II. Following a ",
                    "dirth of female winners that spanned 19 years, ",
                    "men have won the prize at a rate of 2 to 1 compared to women since 1961."),
       x = "", y = "",
       caption = glue("The Fiction category was created in 1948; data from prior years ",
                      "shown here are from the Novel category. There were ten years during the span shown ",
                      "where no award was given. Analysis and graphic by Spencer Schien (@MrPecners), ",
                      "data from The Pulitzer Prizes and Wikipedia."))


ggsave(p, filename = "plots/day_25_trend.png",
       w = 9, h = 5, bg = "white")
