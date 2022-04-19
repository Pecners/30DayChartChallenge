library(tidyverse)
library(ggtext)
library(showtext)
library(glue)

data <- read_csv("data/meat_consumption.csv")

names(data) <- str_to_lower(names(data))

d <- data %>%
  filter(subject == "BEEF" & measure == "KG_CAP" & time %in% c(2000, 2020)) %>%
  pivot_wider(names_from = time, values_from = value) %>%
  mutate(delta = `2020` - `2000`) %>%
  arrange(delta)


orange <- "#e65b3c"
gold <- "#d9b047"
bg <- "#18273D"

font_add_google("Vollkorn", "v")
showtext_auto()

d %>%
  ggplot(aes(x = 0, xend = delta)) +
  geom_curve(y = 0, yend = 0, curvature = -1.5, angle = 120, ncp = 200,
             size = .5, aes(color = ifelse(delta < 0, orange, gold),
                            alpha = ifelse(delta == max(delta) | delta == min(delta), 1, .2))) +
  annotate(geom = "segment", x = min(d$delta) - .5, xend = max(d$delta) + .5, hjust = .9,
               vjust = 1.25, y = 0, yend = 0, color = "grey30", label = "test",
               arrow = arrow(length = unit(.2, "cm"), type = "closed", ends = "both")) +
  geom_segment(data = tibble(y = rep(-.025, 3), yend = rep(.025, 3),
                                 x = c(-10, -5, 5), xend = c(-10, -5, 5)),
                   aes(x = x, xend = xend,
                       y = y, yend = yend), color = "grey30", size = .25,
                   inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(-10, -5, 5), y = c(.04, .04, -.04),
                          l = c("-10 kg/capita", "-5", "+5"), vjust = c(0, 0, 1)),
            aes(x = x, y = y, label = l, vjust = vjust), size = 3,
            inherit.aes = FALSE, color = "grey30") +
  annotate(geom = "text", x = 1.5, y = -.1, label = "2000 baseline",
           family = "v", color = "grey50", hjust = 0) +
  annotate(geom = "segment", x = 1.5, xend = 0, y = -.1, yend = -.001,
           color = "grey50") +
  annotate(geom = "text", x = max(d$delta) + .75, y = 0, 
           label = "More beef\nin 2020", hjust = 0, lineheight = .9,
           color = "grey30", size = 3) +
  annotate(geom = "text", x = min(d$delta) - .75, y = 0, 
           label = "Less beef\nin 2020", hjust = 1, lineheight = .9,
           color = "grey30", size = 3) +
  annotate(geom = "segment", x = min(d$delta), xend = min(d$delta),
           y = 0, yend = .2, color = alpha("white", .25), linetype = 2) +
  geom_textbox(x = min(d$delta), y = .2, vjust = 0, hjust = .1,
               label = "**Paraguay**<br>-11.9 kg/capita", width = unit(1.5, "in"),
               color = orange, fill = bg, box.size = 0, family = "v") +
  annotate(geom = "segment", x = max(d$delta), xend = max(d$delta),
           y = 0, yend = -.2, color = alpha("white", .25), linetype = 2) +
  geom_textbox(x = max(d$delta), y = -.2, vjust = 1, hjust = .9, halign = 1,
               label = "**Israel**<br>+6.7 kg/capita", width = unit(1.5, "in"),
               color = gold, fill = bg, box.size = 0, family = "v") +
  geom_textbox(x = max(d$delta) + 2, y = -.9, vjust = 1, hjust = .95, halign = 1,
               label = "Graphic by Spencer Schien (@MrPecners) | Data from OECD",
               color = "grey30", fill = bg, box.size = 0, family = "v",
               width = unit(5.5, "in")) +
  scale_y_continuous(limits = c(-1, .75), expand = c(0,0)) +
  scale_x_continuous(expand = c(.1, .1)) +
  scale_color_identity() +
  scale_alpha_identity() +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(b = 0),
        legend.position = "none",
        plot.title = element_textbox(color = "white", family = "v",
                                     margin = margin(t = 5, l = 5, b = 10),
                                     size = 28),
        plot.subtitle = element_textbox(width = unit(8.75, "in"), lineheight = 1.1,
                                        color = "white", family = "v",
                                        margin = margin(t = 5, l = 5, b = -10),
                                        size = 18, fill = bg)) + 
  labs(title = "**Where's the beef?**",
       subtitle = glue("Among the 38 OECD member countries, <span style='color:{orange}'>**21 have reduced**</span> ",
                       "their beef consumption ",
                       "between 2000 and 2020, while ",
                       "<span style='color:{gold}'>**17 have increased**</span> consumption."))

ggsave(filename = "plots/day_18_oecd.png", bg = bg,
       w = 9, h = 7)
