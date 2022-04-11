library(tidyverse)
library(lubridate)
library(geomtextpath)
library(MetBrewer)
library(glue)
library(ggtext)
library(showtext)

d <- read_csv("https://data.milwaukee.gov/dataset/5fafe01d-dc55-4a41-8760-8ae52f7855f1/resource/8fffaa3a-b500-4561-8898-78a424bdacee/download/trafficaccident.csv")


ty <- d %>%
  mutate(m = format(CASEDATE, "%b"),
         y = year(CASEDATE))

ym <- ty %>% 
  group_by(y, m) %>%
  tally() %>%
  filter(y > 2011 & y < 2022) %>%
  mutate(m = factor(m, levels = month.abb))

c <- met.brewer("Hiroshige", n = 12)

font_add_google("Alegreya Sans SC", "as")
showtext_auto()

labs <- ym %>%
  ungroup() %>%
  group_by(y) %>%
  summarise(mn = max(n))

ym %>%
  ggplot(aes(y, n, fill = m)) +
  geom_col(position = "dodge", width = 1) +
  geom_labelhline(yintercept = 500, label = "500 Accidents", linetype = 2,
                  linecolor = "white", family = "as") +
  geom_textpath(data = labs, inherit.aes = FALSE,
                aes(x = y, y = mn, label = y),
                vjust = 1.5, family = "as") +
  coord_curvedpolar(start = 0) +
  scale_y_continuous(limits = c(-500, NA)) +
  scale_fill_manual(values = c) +
  theme_void() +
  theme(legend.position = c(1-.75, 1-.925),
        legend.key.size = unit(.75, "cm"),
        legend.text.align = 0,
        legend.text = element_text(family = "as", face = "bold"),
        plot.margin = margin(0,0,-50,0),
        plot.title = element_textbox(halign = 1, hjust = 1, 
                                     margin = margin(t = -55, b = -75, r = 10),
                                     maxwidth = unit(5, "in"), fill = c[6],
                                     color = c[12], padding = margin(5,5,2,5),
                                     r = unit(2, "pt"), linetype = 1,
                                     family = "as", face = "bold", size = 20),
        plot.caption = element_textbox(halign = 1, hjust = 1,
                                       margin = margin(t = -30, r = 10),
                                       maxwidth = unit(5, "in"), fill = c[6],
                                       color = c[12], padding = margin(5,5,2,5),
                                       r = unit(2, "pt"), linetype = 1,
                                       family = "as", size = 8)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, label.position = "left")) +
  labs(fill = "",
       title = glue("Monthly count of traffic accidents<br>Milwaukee, WI"),
       caption = glue("Graphic by Spencer Schien (@MrPecners) | ",
                      "Data from Milwaukee Police Department"))

ggsave(filename = "plots/day_11_circular.png", bg = "white",
       w = 9, h = 9)
