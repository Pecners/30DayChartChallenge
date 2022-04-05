library(tidyverse)
library(patchwork)
library(ggthemes)
library(ggtext)
library(showtext)
library(glue)

# source: https://dnr.wisconsin.gov/topic/forests/championtrees

data <- read_csv("data/wi_champion_trees.csv")

d <- data %>%
  select(`Common Name`:`Species Verified`) %>%
  mutate(avg_spread = (`Crown Spread Diameter 1 verified` + `Crown Spread Diameter 2 verified`) / 2,
         diameter = (`Trunk Circumference verified` / pi) / 12) %>%
  arrange(desc(`Total Points`))

green <- "#25591f"
brown <- "#432702"

font_add_google("Luckiest Guy", "lg")
font_add_google("Architects Daughter", "ad")
showtext_auto()
showtext_opts(dpi = 300)

plots <- map(1:nrow(d), function(i) {
  dd <- d[i,]
  
  
  ymax <- dd$`Vertical Height verified`
  ymin <- ymax * .33
  xmin <- 0 - dd$avg_spread / 2
  xmax <- 0 + dd$avg_spread / 2
  
  can <- tibble(
    x = c(xmin, 0, xmax),
    y = c(ymin, ymax, ymin)
  )
  
  p <- dd %>%
    ggplot() +
    geom_rect(aes(xmin = 0 - diameter / 2, xmax = 0 + diameter / 2,
                  ymin = 0, ymax = `Vertical Height verified` * .75),
              fill = brown) +
    geom_polygon(data = can, aes(x = x, y = y), fill = green) +
    geom_text(x = 0, y = -10, label = str_wrap(dd$`Common Name`, 15), size = 2.5,
              family = "ad", color = brown, lineheight = .9, vjust = 1) +
    coord_cartesian(clip = "off") +
    theme_void() +
    scale_y_continuous(limits = c(-15, max(d$`Vertical Height verified`))) +
    scale_x_continuous(limits = c(0 - (max(d$avg_spread) / 2), 0 + max(d$avg_spread) / 2))
  
  return(p)
})


# Set up the plot title

title <- ggplot() +
  theme_void() +
  geom_richtext(aes(x = 0, y = 25), 
                label = glue("Wisconsin Champion Trees"),size = 10, fill = NA, label.color = NA,
                hjust = 0, family = "lg", color = "#214d1b") +
  geom_textbox(aes(x = 0, y = 7),
                label = glue("A Champion Tree is the largest of its species in terms of height, trunk ",
                             "circumference, and crown spread (all shown below to scale). ",
                             "In Wisconsin, the tallest Champion ",
                             "on record is an Eastern White Pine, but the overall Champion is a Black Willow."),
                hjust = 0, fill = NA, box.size = 0, width = unit(7.5, "in"),
               family = "ad", color = brown) +
  scale_y_continuous(limits = c(0, 30)) +
  scale_x_continuous(limits = c(0, 10)) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(c(0,0,0,0)))


scale <- dd %>%
  ggplot() +
  geom_segment(x = -25, xend = 25,
               y = 10, yend = 10,
               color = brown, size = .5, lineend = "square") +
  geom_segment(x = 25, xend = 25,
               y = 10, yend = 5, lineend = "square") +
  geom_segment(x = -25, xend = -25,
               y = 10, yend = 5, lineend = "square") +
  geom_text(x = 0, y = 25, label = "50 ft.", family = "ad",
            color = brown, size = 2.5) +
  scale_y_continuous(limits = c(-15, max(d$`Vertical Height verified`))) +
  scale_x_continuous(limits = c(0 - (max(d$avg_spread) / 2), 0 + max(d$avg_spread) / 2)) +
  theme_void()

# Set up arrows and caption

va <- tibble(
  x = 0,
  xend = 0,
  y = 0, 
  yend = .9
)

ha <- tibble(
  x = 1,
  xend = .075,
  y = 0, 
  yend = 0
)

vert_arrow <- va %>%
  ggplot() +
  geom_segment(aes(x = x, xend = xend,
                   y = y, yend = yend),
               arrow = arrow(length = unit(.1, "cm"), type = "closed"),
               color = brown, size = .4) +
  geom_text(aes(x = x, y = .93), label = "Bigger", color = brown,
            family = "ad", size = 2.5) +
  theme_void()

hor_arrow <- ha %>%
  ggplot() +
  geom_segment(aes(x = x, xend = xend,
                   y = y, yend = yend),
               arrow = arrow(length = unit(.1, "cm"), type = "closed"),
               color = brown, size = .4) +
  geom_text(aes(x = 0, y = y), label = "Bigger", color = brown, hjust = 0,
            family = "ad", size = 2.5) +
  geom_textbox(x = 0, y = -.02, hjust = 0, vjust = 1,
               color = alpha(brown, .5), family = "ad",
               width = unit(7.5, "in"), box.size = 0, size = 3, fill = bg,
               label = glue("Analysis and graphic by Spencer Schien (@MrPecners) | ",
                            "Data from Wisconsin Department of Natural Resources"),
               box.padding = margin(0,0,0,0)) +
  scale_x_continuous(limits = c(0, 1)) +
  theme_void() +
  coord_cartesian(clip = "off")

# Set up layout for patchwork

s <- 1
bg <- "#d0f1cc"



layout <- c(
  area(1,1,s,3), 
  #area(1,4,2,5),
  area(s+1,7,s+5,7),
  area(s+1, 1), area(s+1, 2), area(s+1,3), area(s+1,4),
  area(s+1,5), area(s+1,6), area(s+2,1), area(s+2,2), area(s+2,3), area(s+2,4),
  area(s+2,5), area(s+2,6), area(s+3,1), area(s+3,2), area(s+3,3), area(s+3,4),
  area(s+3,5), area(s+3,6), area(s+4,1), area(s+4,2), area(s+4,3), area(s+4,4),
  area(s+4,5), area(s+4,6), area(s+5,1), area(s+5,2),
  area(s+5, 6),
  area(s+6,1,s+6,6)
)

# Create pat
wrap_plots(title,
           vert_arrow,
           plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]],
           plots[[7]], plots[[8]], plots[[9]], plots[[10]], plots[[11]], plots[[12]],
           plots[[13]], plots[[14]], plots[[15]], plots[[16]], plots[[17]], plots[[18]],
           plots[[19]], plots[[20]], plots[[21]], plots[[22]], plots[[23]], plots[[24]],
           plots[[25]], plots[[26]],
           scale,
           hor_arrow,
           design = layout) &
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = alpha(bg, .5), color = NA)
  ))


ggsave(filename = "plots/day_4_flora.png", device = "png", bg = bg,
       w = 8, h = 7)
