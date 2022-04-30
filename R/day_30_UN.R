library(tidyverse)
library(glue)
library(MetBrewer)
library(scales)
library(geomtextpath)

f <- list.files("data/un")
data <- read_csv(glue("data/un/{f}"))

perm_mems <- c("United States of America",
               "France",
               "United Kingdom",
               "China",
               "Russian Federation")

pm <- data %>%
  filter(Variant %in% c("Medium", "High", "Low") &
           Location %in% perm_mems) %>%
  mutate(Location = case_when(Location == "United States of America" ~ "United States",
                              Location == "Russian Federation" ~ "Russia",
                              TRUE ~ Location),
         Location = factor(Location,
                           levels = c("China",
                                      "United States",
                                      "Russia",
                                      "United Kingdom",
                                      "France")))

mid <- pm %>%
  filter(Variant == "Medium") %>%
  arrange(Location)

hl <- pm %>%
  filter(Variant != "Medium") %>%
  select(Time, Location, Variant, PopTotal) %>%
  pivot_wider(names_from = Variant, values_from = PopTotal) %>%
  arrange(Location)

c <- met.brewer("Redon")

mid %>%
  filter(Time <= 2020) %>%
  ggplot(aes(Time, PopTotal, group = Location, color = Location)) +
  geom_line() +
  geom_line(data = mid %>%
              filter(Time >= 2020 & Location != "China"),
            linetype = 2) +
  geom_ribbon(data = hl, aes(x = Time, ymin = Low, ymax = High,
                             group = Location,
                             fill = Location),
              inherit.aes = FALSE,
              alpha = .5) +
  geom_text(data = mid %>%
              filter(Time == max(Time)),
            aes(label = Location, x = Time + 1,
                vjust = ifelse(Location == "France", 1.5, .5)),
            hjust = 0, size = 3) +
  geom_textpath(data = hl %>%
                  filter(Location == "China"),
                aes(label = "Upper bound assumes high fertility", x = Time, y = High),
                linewidth = 0, offset = unit(.25, "lines"), size = 3) +
  geom_textpath(data = hl %>%
                  filter(Location == "China"),
                aes(label = "Low fertility", x = Time, y = Low),
                linewidth = 0, offset = unit(-.5, "lines"),
                size = 3) +
  geom_textpath(data = mid %>%
                  filter(Location == "China" & Variant == "Medium" & 
                           Time >= 2020),
                aes(label = "Medium fertility", x = Time),
                linetype = 2, size = 3) +
  scale_color_manual(values = c[c(2,4,6,10,12)]) +
  scale_fill_manual(values = c[c(1,3,5,9,11)]) +
  scale_x_continuous(limits = c(1950, 2120), expand = c(0,0),
                     breaks = seq(from = 1950, to = 2100, by = 25)) +
  scale_y_continuous(breaks = c(0, 500000, 1000000, 1500000),
                     labels = c("0", "500M", "1,000M", "1,500M\nTotal Pop.")) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, color = "grey70", hjust = 0)) +
  labs(title = "Population Projections for UN Security Council Permanent Members",
       subtitle = glue("The future of China's population is the most uncertain among the five ",
                       "permanent member of the UN Security Council."),
       caption = glue("Graphic by Spencer Schien (@MrPecners) | ",
                      "Data from the United Nations Department of Economic and Social Affairs"),
       y = "", x = "")

ggsave("plots/day_30_UN.png", bg = "white",
       w = 9, h = 6)
