library(tidyverse)
library(ggtext)
library(glue)
library(showtext)

d <- read_csv("data/malaria-death-rates.csv")

names(d)[4] <- "rate"

clean <- d %>%
  filter(!is.na(Code) & Year %in% c(1990, 2019)) %>%
  pivot_wider(names_from = Year, values_from = rate) %>%
  mutate(diff = `1990` - `2019`,
         perc = diff / `1990`) %>%
  filter(diff != 0)

orange <- "#e65b3c"
gold <- "#d9b047"
bg <- "#18273D"

font_add_google("Vollkorn", "v")
showtext_auto()

clean %>%
  filter(diff == max(diff) | diff == min(diff)) %>%
  ggplot(aes(x = 0, xend = diff)) +
  annotate(geom = "segment", x = max(clean$diff), xend = max(clean$diff),
               y = 0, yend = -.1, color = alpha("white", .25), linetype = 2) +
  geom_textbox(x = max(clean$diff), y = -.1, vjust = 1, hjust = .9, halign = 1,
               label = "**Uganda**<br>Reduction of 125 deaths<br>per 100k",
               color = gold, fill = bg, box.size = 0, family = "v") +
  annotate(geom = "segment", x = min(clean$diff), xend = min(clean$diff),
           y = 0, yend = .1, color = alpha("white", .25), linetype = 2) +
  geom_textbox(x = min(clean$diff), y = .1, vjust = 0, hjust = .1,
               label = "**Mauritania**<br>Increase of 59 deaths<br>per 100k",
               color = orange, fill = bg, box.size = 0, family = "v") +
  geom_curve(y = 0, yend = 0, curvature = -1.5, angle = 120, ncp = 200,
             size = .5, aes(color = ifelse(diff < 0, orange, gold),
                            alpha = ifelse(diff == max(diff) | diff == min(diff), 1, .2))) +
  annotate(geom = "text", x = 20, y = -.05, label = "1990 baseline",
           family = "v", color = "grey50", hjust = 0) +
  annotate(geom = "segment", x = 20, xend = 0, y = -.05, yend = -.001,
           color = "grey50") +
  geom_hline(yintercept = 0, color = "white") +
  # caption
  geom_textbox(x = max(clean$diff), y = -.4, vjust = 1, hjust = .95, halign = 1,
               label = "Graphic by Spencer Schien (@MrPecners) | Data from Our World in Data",
               color = alpha("white", .25), fill = bg, box.size = 0, family = "v",
               width = unit(5.5, "in")) +
  scale_y_continuous(limits = c(-1, 1), expand = c(0,0)) +
  scale_color_identity() +
  scale_alpha_identity() +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(b = -150),
        legend.position = "none",
        plot.title = element_textbox(color = "white", family = "v",
                                     margin = margin(t = 5, l = 5),
                                     size = 24),
        plot.subtitle = element_textbox(width = unit(8.5, "in"),
                                        color = "white", family = "v",
                                        margin = margin(t = 5, l = 5, b = 10),
                                        size = 14, fill = bg)) + 
  labs(title = "**Moving in the Right Direction**",
       subtitle = glue("Most countries have <span style='color:{gold}'>**reduced**</span> ",
                       "their death rates from Malaria ",
                       "between 1990 and 2019, though a limited number have have seen an ",
                       "<span style='color:{orange}'>**increase**</span>."))

  

ggsave(filename = "plots/ggraph_mistake.png", bg = bg,
       w = 9, h = 7)

