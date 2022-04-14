library(tidyverse)
library(corrr)
library(RColorBrewer)
library(showtext)
library(glue)
library(ggtext)

# Get fonts
font_add_google("Rock Salt", "rs")
font_add_google("Caveat", "c")
showtext_auto()

# Load data
data <- tidytuesdayR::tt_load("2021-12-14")

# Select numeric variables
c <- data$studio_album_tracks %>%
  select(danceability,
         energy,
         speechiness,
         acousticness,
         instrumentalness,
         liveness,
         valence,
         tempo)

# Calculate correlation, put in df
df_c <- c %>%
  correlate()

# Transform for plotting, then plot
f <- df_c %>%
  pivot_longer(-term, names_to = "group", values_to = "corr") %>%
  filter(term == "danceability" & !is.na(corr)) %>%
  mutate(group = str_to_title(group),
         budge = case_when(corr > 0 ~ -.025,
                           corr < 0 ~ .025, 
                           TRUE ~ 0),
         align = case_when(corr > 0 ~ 1,
                           corr < 0 ~ 0,
                           TRUE ~ 0))
  
f %>%
  ggplot(aes(reorder(group, corr), corr)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_segment(aes(x = reorder(group, corr), xend = reorder(group, corr),
                   y = 0 + .005 * sign(corr), yend = corr),
               linetype = 4, size = .75, color = "#340E51") +
  geom_point(size = 12, aes(fill = corr), shape = 21) +
  geom_text(aes(label = round(corr, digits = 2),
                color = ifelse(abs(corr) < .2, "black", "white")),
            size = 4, family = "c", fontface = "bold") +
  geom_text(aes(label = group, 
                y = budge, hjust = align), family = "rs", color = "#340E51") +
  # Add curve arrows for valence and acousticness
  annotate(geom = "curve", x = 6, xend = 7,
           y = -.5, yend = -.15, curvature = -.3, angle = 140, 
           arrow = arrow(length = unit(.1, "cm"), type = "closed"),
           size = .3) +
  annotate(geom = "curve", x = 2, xend = 1, curvature = -.25, angle = 120,
           y = .5, yend = .21, arrow = arrow(length = unit(.1, "cm"),
                                              type = "closed"),
           size = .3) +
  # Add correlation coefficient annotation
  annotate(geom = "text", x = 6, y = .59, label = c("Correlation\ncoefficients"),
           family = "c", color = "#340E51", lineheight = .9,
           size = 3) +
  annotate(geom = "segment", x = 6, xend = 6, y = .545, yend = .48, size = .2,
           arrow = arrow(length = unit(.075, "cm"), type = "closed")) +
  annotate(geom = "segment", x = 6.21, xend = 6.67, y = .625, yend = .646, size = .2,
           arrow = arrow(length = unit(.075, "cm"), type = "closed")) +
  # Add textbox annotations
  geom_textbox(data = tibble(1), inherit.aes = FALSE, hjust = 0, vjust = .75,
               minwidth = unit(2.5, "in"), box.color = "black", size = 5,
               x = 6, y = -.625, lineheight = 1,
               label = glue("Valence, a measure of musical positiveness, ",
                            "shows the strongest correlation with danceability."),
               family = "c", color = "white", fill = "#f28bd5") +
  geom_textbox(data = tibble(1), inherit.aes = FALSE, hjust = 1, vjust = .25,
               minwidth = unit(2.5, "in"), box.color = "black", size = 5,
               x = 2, y = .65, lineheight = 1,
               label = glue("The more acoustic the song, ",
                            "the less danceable it is."),
               family = "c", color = "white", fill = "#D12424") +
  scale_fill_gradient2(low = "#D12424", mid = "white", high = "#f28bd5") +
  scale_color_manual(values = c("black", "white")) +
  scale_y_continuous(breaks = c(-.6, .7), labels = c("Less Danceable", "More Danceable")) +
  theme(legend.position = "none",
        text = element_text(family = "rs", color = "#340E51"),
        plot.caption = element_text(hjust = .5, size = 8, lineheight = 1.25,
                                    color = "#c1afcc"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(hjust = c(0,1), color = "#340E51"),
        axis.line.x = element_line(arrow = arrow(length = unit(.2, "cm"), ends = "both", type = "closed"),
                                   color = "#340E51"),
        plot.title = element_text(size = 18, hjust = .5),
        plot.subtitle = element_text(size = 14, hjust = .5,
                                     margin = margin(b = 20))) +
  labs(x = "", y = "",
       title = "What makes a Spice Girls song danceable?",
       subtitle = "Sugar, spice, and everything valence",
       caption =  "Graphic by Spencer Schien (@MrPecners) | Data from Spotify") +
  coord_flip(clip = "off")

ggsave(filename = "plots/day_13_correlation.png", bg = "white",
       w = 9, h = 6)
