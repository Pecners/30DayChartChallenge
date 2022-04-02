library(tidyverse)
library(showtext)
library(ggimage)
library(ggtext)
library(glue)

data <- read_csv("data/pulitzer_winners_fiction.csv")

# Calculate ages

ages <- data %>%
  mutate(birth_year = as.numeric(str_extract(lifespan, "\\d{4}")),
         age = year - birth_year,
         decade = floor(age/10))

# Point to sticker image

img <- "images/pulitzer.png"

# Summarise by states

top_states <- ages %>%
  filter(!is.na(winner)) %>%
  group_by(author_origin) %>%
  tally() %>%
  filter(n > 4)

# Create dummie obs for plotting points

long_states <- map_df(1:nrow(top_states), function(i) {
  lim <- top_states[[i, "n"]]
  
  tibble(
    state = top_states[[i, "author_origin"]],
    n = 1:lim
  )
})

# Set theme

bg <- "#18273D"
gold <- "#d9b047"

font_add_google("Vollkorn", "v")
showtext_auto()

long_states %>%
  ggplot(aes(reorder(state, -n), n)) +
  geom_image(image = img, size = .085) +
  geom_textbox(x = 2.4, y = 12.25, hjust = 0, vjust = 1, fill = bg, box.size = 0,
               color = "white", family = "v", width = unit(4, "in"), size = 10,
               label = glue("**WEIGHT OF** <span style='color:{gold}'>**GOLD**</span>")) +
  geom_textbox(x = 2.4, y = 10.5, hjust = 0, fill = bg, box.size =0, vjust = 1,
               color = "white", family = "v", width = unit(3.5, "in"), size = 6,
               label = glue("Winning authors have hailed from these four states ",
                            "for 33 of the 94 total Pulitzer Prizes awarded for fiction.")) +
  scale_x_discrete(expand = c(.5, 0)) +
  coord_cartesian(xlim = c(1.5, 3.5)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "white", family = "v",
                                   size = 14),
        plot.caption = element_textbox(color = alpha("white", .25),
                                       family = "v", hjust = .5, size = 10),
        plot.margin = margin(5,0,5,0)) +
  labs(x = "", y = "",
       caption = glue("Data from The Pulitzer Prizes and Wikipedia | ",
                      "Analysis and graphic by Spencer Schien (@MrPecners)"))

ggsave(filename = "plots/day_2_pictogram.png", bg = bg)
