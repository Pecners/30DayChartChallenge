library(tidyverse)
library(lubridate)
library(glue)
library(ggridges)
library(showtext)
library(MetBrewer)
library(ggtext)

# Data sourced from NOAA here:
# https://gml.noaa.gov/grad/solcalc/

sunrise <- read_csv("data/sunrise.csv")
sunset <- read_csv("data/sunset.csv")

# Clean and put in tidy format
d <- sunrise %>%
  pivot_longer(cols = -Day, names_to = "month", values_to = "time") %>%
  mutate(yd = ymd(glue("2022-{month}-{Day}")),
         group = "sunrise") 

dd <- sunset %>%
  pivot_longer(cols = -Day, names_to = "month", values_to = "time") %>%
  mutate(yd = ymd(glue("2022-{month}-{Day}")),
         group = "sunset")

both <- bind_rows(d, dd) 

# Calculate daylight hours

wide <- both %>%
  pivot_wider(names_from = group, values_from = time) %>%
  mutate(daylight = sunset - sunrise,
         darkness = 24*60*60 - daylight,
         month = month(yd)) %>%
  pivot_longer(cols = c(daylight, darkness), names_to = "group", values_to = "length")

# Set up dataframe for labels

labs <- wide %>%
  filter(group == "daylight") %>%
  mutate(monthl = format(yd, "%B")) %>%
  group_by(monthl, month) %>%
  summarise(avg = mean(length),
            m = max(length)) %>%
  filter(!is.na(month))

# Set up breaks

low <- 9*60*60
half <- 12*60*60
high <- 15*60*60

font_add_google("Lemonada", "l")
font_add_google("Indie Flower", "if")
showtext_auto()

c <- met.brewer("Cross")
bg <- "white"

wide %>%
  filter(group == "daylight") %>%
  ggplot(aes(x = length, y = month, fill = stat(x), group = month)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  geom_text(data = labs, aes(x = ifelse(month == 6, m + 500, avg),
                              hjust = ifelse(month == 6, 0, .5),
                              y = month, label = monthl),
            family = "l", size = 3,
            inherit.aes = FALSE, vjust = 1.5) +
  scale_fill_gradient(low = c[9], high = c[5]) +
  scale_y_reverse() +
  scale_x_continuous(breaks = c(low, half, high),
                     labels = c("9 hrs of daylight", "12 hrs", "15 hrs")) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = bg),
        plot.background = element_rect(fill = bg),
        panel.grid.major.x = element_line(color = "grey60", linetype = 3),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "l"),
        axis.ticks = element_blank(),
        plot.title = element_textbox(family = "l", size = 28),
        plot.subtitle = element_text(family = "l", size = 16),
        plot.caption = element_text(family = "l", hjust = 0, color = "grey70")) +
  labs(title = glue("What goes <span style='color:{c[5]}'>**light**</span> must go ",
                    "<span style='color:{c[9]}'>**dark**</span>"),
       subtitle = "Monthly distribution of daylight hours in Milwaukee, Wisconsin",
       x = "", y = "",
       caption = "Graphic by Spencer Schien (@MrPecners) | Data from NOAA")

ggsave("plots/day_21_updown.png", bg = c[6], width = 9, h = 6, dpi = 300)
