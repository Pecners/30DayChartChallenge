library(tidyverse)
library(lubridate)
library(ggstream)
library(glue)
library(scales)
library(showtext)
library(ggtext)
library(geomtextpath)

# You can download the data here: https://www.drought.gov/states/Wisconsin

data <- read_csv("data/SPI-wisconsin.csv", 
                 name_repair = function(x) {
                   str_to_lower(x)
                 })

# Clean data to make it long, get usable dates

long <- data %>%
  filter(`-9` != 100) %>%
  mutate(good_date = ymd(str_sub(date, 3, 10))) %>%
  pivot_longer(cols = c(3:13), names_to = "cat") %>%
  select(-c(1:2)) %>%
  filter(cat != "-9") %>%
  mutate(cat = factor(cat, levels = c(glue("d{0:4}"),
                                       glue("w{0:4}"))))

# Investigate wet years

long %>%
  filter(cat == "w0" & good_date > ymd("2015-12-01") & good_date < ymd("2021-01-01")) %>%
  group_by(plus_90 = value >= 90.0) %>% 
  tally()

# Set colors, sourced from the website where I got the data

c <- c(
  "d0" = "#FCFF00",
  "d1" = "#FFCC99",
  "d2" = "#FF6600",
  "d3" = "#FF0000",
  "d4" = "#660000",
  "w0" = "#AAFF55",
  "w1" = "#01FFFF",
  "w2" = "#04A9FF",
  "w3" = "#0000FF",
  "w4" = "#0000AA"
)

# Invert colors because I want to use a black background

c_neg <- c(rev(c[1:5]), rev(c[6:10]))
names(c_neg) <- NULL

# Create df for custom axis labels

ylabs <- tibble(
  x = c("1895-09-01",
        glue("{seq(from = 1920, to = 2000, by = 20)}-01-01"),
        "2022-03-01")
) %>%
  mutate(x = ymd(x),
         y = 125,
         yend = -125,
         lab = year(x),
         hjust = c(0,0,0,.5,1,1,1),
         vjust = c(-.5, .5,.5, 1, 1, .5, -.5))

# Load fonts

font_add_google("Atkinson Hyperlegible", "fo")
font_add_google("K2D", "fo")
showtext_auto()

long %>%
  #filter(good_date < ymd("1900-01-01")) %>%
  mutate(value = ifelse(str_detect(cat, "^w"), value * -1, value)) %>%
  ggplot(aes(good_date, value, fill = cat)) +
  annotate(geom = "rect", xmin = ymd("2016-01-01"), xmax = ymd("2021-01-01"),
           ymin = -110, ymax = 100, fill = "grey40") +
  geom_segment(data = ylabs,
               aes(x = x, xend = x, y = y-5, yend = yend),
               color = "grey50", inherit.aes = FALSE) +
  geom_text(data = ylabs, family = "fo",
            aes(x = x, y = y, label = lab, 
                hjust = hjust, vjust = vjust),
            color = "grey50", inherit.aes = FALSE) +
  geom_area(position = "identity") +
  scale_y_continuous(labels = function(x) glue("{abs(x)}%"),
                     limits = c(-1000, NA)) +
  scale_x_date(expand = c(.05, 0)) +
  scale_fill_manual(values = c_neg,
                    labels = c(rep("", 4), "Driest",
                               rep("", 4), "Wettest")) +
  coord_curvedpolar(clip = "off") +
  theme_void() +
  theme(text = element_text(family = "fo"),
        legend.position = c(.5,.3),
        legend.key.size = unit(1, "lines"),
        plot.title = element_textbox(margin = margin(b = -250,
                                                  t = 100),
                                  color = "white", hjust = .5,
                                  family = "fo", size = 20, face = "bold"),
        plot.subtitle = element_textbox(color = "white", family = "fo", size = 16,
                                        margin = margin(b = -350,
                                                        t = 300),
                                        width = unit(4.5, "in"), hjust = .5,
                                        vjust = 0, lineheight= 1.1),
        plot.caption = element_text(color = "grey40", hjust = .5,
                                    margin = margin(t = -25, b = -30))) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, 
                             label.theme = element_text(color = "white",
                                                        size = 8))) +
  labs(fill = "",
       title = "Wisconsin Drought Conditions",
       subtitle = glue("The period from 2016 to 2021 (highlighted above) was a wet one liin Wisconsin, ",
                       "with over 90% of the state wetter than average for 41 out of 60 months. ",
                       "Since 2020, there has not been a single month where that was the case."),
       caption = glue("Graphic by Spencer Schien (@MrPecners) | ",
                      "Data from the National Integrated Drought Information System"))

ggsave("plots/day_16_environment.png", bg = "black",
       w = 9, h = 9)
