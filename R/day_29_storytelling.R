library(tidyverse)
library(showtext)
library(ggtext)
library(glue)
library(patchwork)
library(ggthemes)

# Data sourced from here: https://en.wikipedia.org/wiki/List_of_Nobel_laureates_in_Literature

data <- read_csv("data/nobel_laureates_lit.csv")
names(data) <- str_to_lower(names(data))

d <- data %>% 
  mutate(year = as.character(year)) %>%
  select(year,
         laureate,
         author_sex) %>%
  mutate(x = as.numeric(str_extract(year, ".$")),
         y = as.numeric(str_sub(year, start = 1, end = 3)),
         yind = max(y) - y) %>%
  group_by(year) %>%
  mutate(year_ind = row_number(),
         year_n = n())

# Set color scheme and font

bg <- "#18273D"
m_color <- "#B4C6D4"
f_color <- "#EEAF35"
shade <- alpha("white", .1)

font_add_google("Vollkorn", "v")
showtext_auto()

# Main plot 
main <- d %>%
  ggplot(aes(x, yind, color = author_sex, fill = author_sex)) +
  # WWII blank spot annotation
  annotate(geom = "text", x = 1.5, y = 8, label = "Blank space means no Prize awarded",
           color = "grey60", size = 3, family = "v") +
  annotate(geom = "text", x = 4, y = max(d$yind) + .5, label = "Two winners",
           color = "grey60", size = 2.5, family = "v") +
  # Longest stretch annotation
  geom_textbox(x = 10.75, y = 5, family = "v",
               label = glue("No female winner for 24 years"), 
               width = unit(1.5, "in"), color = "grey60",
               fill = bg, box.color = bg) +
  # Longest stretch highlighting
  annotate(geom = "rect", xmin = 6.5, xmax = 9.5,
           ymin = 5.4, ymax = 6.4, fill = shade, color = NA) +
  annotate(geom = "rect", xmin = -.5, xmax = 9.5,
           ymin = 4.4, ymax = 5.4, fill = shade, color = NA) +
  annotate(geom = "rect", xmin = -.5, xmax = 9.5,
           ymin = 3.4, ymax = 4.4, fill = shade, color = NA) +
  annotate(geom = "rect", xmin = -.5, xmax = .5,
           ymin = 2.4, ymax = 3.4, fill = shade, color = NA) +
  # Add the boxes now
  geom_point(data = d %>% filter(!is.na(author_sex)), 
             aes(x = case_when(year_n == 2 & year_ind == 1 ~ x - .2,
                               year_n == 2 & year_ind == 2 ~ x + .2,
                               TRUE ~ x)), shape = 22, size = 7) +
  # Add the year labels
  geom_text(aes(y = yind - .4, label = year), family = "v",
            size = 3, color = "white") +
  # Caption
  annotate(geom = "text", x = 5.5, y = -1, family = "v",
           color = alpha("white", .25), size = 4, lineheight = 1.1,
           label = glue("Data from The Nobel Prize and Wikipedia | ",
                        "Graphic by Spencer Schien (@MrPecners)")) +
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
  mutate(ypos = cumsum(n) - n * .5,
         perc = n / sum(n),
         lab = case_when(author_sex == "m" ~ glue("{n} male\nwinners"),
                         TRUE ~ glue("{n} female\nwinners")),
         c = c(m_color, f_color))

# Overall donut chart

pie <- perc %>%
  ggplot(aes(x = .5, y = n, fill = author_sex, label = lab)) +
  geom_bar(stat = "identity", width = .5) +
  geom_text(aes(x = 1.1, y = ypos, color = c), family = "v",
            lineheight = .9, size = 4, hjust = c(0,1)) +
  scale_fill_manual(values = c(f_color, m_color)) +
  scale_color_identity() +
  coord_polar(theta = "y", clip = "off", start = 1) +
  scale_x_continuous(limits = c(0, 1.1)) +
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
                label = glue("Nobel Literature"),
                size = 15, fill = NA, label.color = NA,
                hjust = 0, color = "white") +
  geom_textbox(aes(x = 0, y = 12), color = "white", family = "v",
               label = glue("In 121 years, the Nobel Prize in Literature has been awarded to 16 ",
                            "<span style='color:{f_color}'>**women**</span> ",
                            "and 102 <span style='color:{m_color}'>**men**</span>. ",
                            "Large as this imbalance is, it represents the second most ",
                            "female laureates among the Nobel Prizes, behind only the Nobel Peace Prize, ",
                            "which has been awarded to 18 women during the same span."),
               fill = NA, box.size = 0, width = unit(6.5, "in"), hjust = 0,
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

ggsave(filename = "plots/day_29_storytelling.png", bg = bg,
       w = 10, h = 9)
