library(tidyverse)
library(ggtext)
library(glue)
library(showtext)
library(patchwork)
library(ggthemes)

# Data sourced from Wikipedia: https://en.wikipedia.org/wiki/Pulitzer_Prize_for_Fiction
# and cleaned by hand.

data <- read_csv("data/pulitzer_winners_fiction.csv")

d <- data %>% 
  mutate(year = as.character(year)) %>%
  select(year,
         winner,
         author_sex) %>%
  mutate(x = as.numeric(str_extract(year, ".$")),
         y = as.numeric(str_sub(year, start = 1, end = 3)))

# Set theme 

bg <- "#18273D"
m_color <- "#e65b3c"
f_color <- "#d9b047"
shade <- alpha("white", .1)

font_add_google("Vollkorn", "v")
showtext_auto()

# Main plot 

main <- d %>%
  ggplot(aes(x, y, color = author_sex, fill = author_sex)) +
  annotate(geom = "rect", xmin = 2.5, xmax = 9.5,
            ymin = 193.4, ymax = 194.4, fill = shade, color = NA) +
  annotate(geom = "rect", xmin = -.5, xmax = 9.5,
            ymin = 194.4, ymax = 195.4, fill = shade, color = NA) +
  annotate(geom = "rect", xmin = -.5, xmax = .5,
            ymin = 195.4, ymax = 196.4, fill = shade, color = NA) +
  geom_point(data = d %>% filter(!is.na(author_sex)), shape = 22, size = 7) +
  annotate(geom = "curve", x = 11, xend = 9.6,
           y = 196, yend = 194.2, curvature = -.5,
           arrow = arrow(length = unit(.2, "cm"), type = "closed"),
           color = "grey50") +
  geom_textbox(x = 11, y = 196, family = "v",
               label = glue("No female winner for 18 years"), 
               width = unit(1.5, "in"), color = "grey60",
               fill = bg, box.color = bg) +
  geom_text(aes(y = y - .4, label = year), family = "v",
            size = 3, color = "white") +
  geom_text(x = 3, y = 202.5, label = "No prize awarded", family = "v",
            color = "grey60", hjust = 0) +
  annotate(geom = "curve", x = 2.9, xend = 2,
           y = 202.5, yend = 201.25, 
           arrow = arrow(length = unit(.2, "cm"), type = "closed"),
           color = "grey50") +
  annotate(geom = "text", x = 5.5, y = 190, family = "v",
           color = alpha("white", .25), size = 4, lineheight = 1.1,
           label = glue("Data from The Pulitzer Prizes and Wikipedia | ",
                        "Analysis and graphic by Spencer Schien (@MrPecners)")) +
  scale_fill_manual(values = c(f_color, m_color)) +
  scale_color_manual(values = c(f_color, m_color)) +
  scale_x_continuous(limits = c(-.5, 12), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(b = 10))

# Overall donut chart prep

perc <- d %>%
  filter(!is.na(author_sex)) %>%
  group_by(author_sex) %>%
  tally() %>%
  arrange(desc(author_sex)) %>%
  mutate(ypos = cumsum(n) - n * .66,
         perc = n / sum(n),
         lab = case_when(author_sex == "m" ~ glue("{n} male\nwinners"),
                         TRUE ~ glue("{n} female\nwinners")),
         c = c(m_color, f_color))

# Overall donut chart

pie <- perc %>%
  ggplot(aes(x = .5, y = n, fill = author_sex, label = lab)) +
  geom_bar(stat = "identity", width = .5) +
  geom_text(aes(x = .9, y = ypos, color = c), family = "v",
            lineheight = 1, size = 4, hjust = c(0,1)) +
  scale_fill_manual(values = c(f_color, m_color)) +
  scale_color_identity() +
  coord_polar(theta = "y", clip = "off") +
  scale_y_continuous(limits = c(0, 94)) +
  scale_x_continuous(limits = c(0, 1)) +
  theme_void() +
  theme(legend.position = "none")

# Set layout for patchwork

layout <- c(
  area(1,1,2,2), # title
  area(1,3,2,4), # donut
  area(3,1,6,4) # boxes
)

# Title for patchwork

title <- ggplot() +
  theme_map() +
  geom_richtext(aes(x = 0, y = 25),  family = "v",
                label = glue("Pulitzer's Fiction"),
                size = 15, fill = NA, label.color = NA,
                hjust = 0, color = "white") +
  geom_textbox(aes(x = 0, y = 12), color = "white", family = "v",
                label = glue("The Pulitzer Prize for fiction ",
                             "has been awarded over twice as often to ",
                             "<span style='color:{m_color}'>**men**</span> ",
                             "as it has been to ",
                             "<span style='color:{f_color}'>**women**</span>."),
                fill = NA, box.size = 0, width = unit(4, "in"), hjust = 0,
               size = 6) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(limits = c(0, 30), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 40), expand = c(0, 0))

# Create patchwork

wrap_plots(
  title,
  pie,
  main,
  design = layout
) &
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = bg, color = NA)
  ))

# Save plot

ggsave(filename = "plots/day_1_part_of_whole.png", bg = bg,
       w = 8, h = 9)
        