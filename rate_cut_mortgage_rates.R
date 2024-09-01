library(tidyverse)
library(glue)
library(ggtext)

# data <- tribble(~rate_cut, ~percent_postcut, ~percent_precut,
#         "Gulf-war Recession in 1990", 8.00, 10.5,
#         "Mid-cycle Adjustments in 1995", 7.00, 7.50,
#         "Global Currency Crisis in 1998", 6.74, 6.92) %>%
#   mutate(bump_precut = if_else(percent_precut > percent_postcut,
#                                percent_precut +1.0,
#                                percent_precut -1.0),
#          bump_postcut = if_else(percent_precut > percent_postcut,
#                                 percent_postcut -1.0,
#                                 percent_postcut +1.0))

data <- read_csv("/Users/takayukitamura/Documents/R_Computing/fed_10y/fed_mortgage .csv") %>%
  rename(rate_cut = ...1, percent_precut = `Pre-Fed's Rate-cut`, percent_postcut = `Post-Fed's Rate-cut`) %>%
  mutate(bump_precut = if_else(percent_precut > percent_postcut,
                                percent_precut +1.0,
                                percent_precut -1.0),
         bump_postcut = if_else(percent_precut > percent_postcut,
                                 percent_postcut -1.0,
                                 percent_postcut +1.0))


main_plot <- data %>% 
  pivot_longer(cols = -rate_cut, names_to = c(".value", "change"),
               names_sep = "_") %>% 
 mutate(rate_cut = factor(rate_cut, levels = rev(data$rate_cut))) %>%
  ggplot(aes(x=percent, y= rate_cut, color = change)) +
  geom_line(color="#E6E6E6", size=1.75) +
  geom_point(size = 2, show.legend = FALSE) +
  geom_text(aes(label=glue("{percent}%"), x=bump), size=4,  show.legend = FALSE) +
  scale_color_manual(name=NULL,
                     breaks = c("precut", "postcut"),
                     values = c("#727272", "#25607a"),
                     labels=c("Pre_rate_cut", "Post_rate_cut")) +
  scale_x_continuous(limits = c(2, 12),
                     breaks = seq(2, 12, by=2),
                     labels = glue("{seq(2,12, by=2)}%")) +
  labs(x = NULL,
       y = NULL,
       title = "30-year mortgage rate changes pre- & post-Fed's Rate Cut Decisions since 1990",
       caption = "Source: FRED(Rederal Reserve Economic Data") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    plot.caption = element_markdown(hjust = 0, color = "darkgray"),
    plot.caption.position = "plot",
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(color = "darkgray"),
    panel.grid.major.x = element_line(color = "gray", size = 0.1),
    panel.grid.major.y = element_line(color = "gray", size = 0.1, linetype = "dotted")
  )

ggsave("30_year_mortgage_fed.png", width = 6, height = 4)  
main_plot


library(tidyverse)
library(glue)
library(ggtext)
library(grid)

# data <- tribble(~rate_cut, ~percent_postcut, ~percent_precut,
#                 "Gulf-war Recession in 1990", 8.00, 10.5,
#                 "Mid-cycle Adjustments in 1995", 7.00, 7.50,
#                 "Global Currency Crisis in 1998", 6.74, 6.92) %>%
#   mutate(bump_precut = if_else(percent_precut > percent_postcut,
#                                percent_precut + 1.0,
#                                percent_precut - 1.0),
#          bump_postcut = if_else(percent_precut > percent_postcut,
#                                 percent_postcut - 1.0,
#                                 percent_postcut + 1.0))

# Create long format data and calculate x and xend for the arrows
long_data <- data %>%
  pivot_longer(cols = -rate_cut, names_to = c(".value", "change"),
               names_sep = "_") %>%
  mutate(rate_cut = factor(rate_cut, levels = rev(data$rate_cut))) %>%
  group_by(rate_cut) %>%
  mutate(x = percent[change == "precut"],
         xend = percent[change == "postcut"]) %>%
  ungroup()

main_plot <- ggplot(long_data, aes(x = percent, y = rate_cut, color = change)) +
  geom_line(color = "gray", size = 1.5) +
  geom_point(size = 2, show.legend = FALSE) +
  geom_segment(aes(x = x, xend = xend, 
                   y = rate_cut, yend = rate_cut),
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "black", size = 0.75) +
  geom_text(aes(label = glue("{percent}%"), x = bump), size = 4,  show.legend = FALSE) +
  scale_color_manual(name = NULL,
                     breaks = c("precut", "postcut"),
                     values = c("#727272", "#25607a"),
                     labels = c("Pre-rate cut", "Post-rate cut")) +
  scale_x_continuous(limits = c(2, 12),
                     breaks = seq(2, 12, by = 2),
                     labels = glue("{seq(2, 12, by = 2)}%")) +
  labs(x = NULL,
       y = NULL,
       title = "30-year mortgage rate changes post-Fed's Rate Cut Decisions since 1990",
       subtitle = "Average impact 0.9% relatively mild, since rate cuts were priced in the markets in advance", 
       caption = "Source: FRED (Federal Reserve Economic Data)") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    plot.subtitle = element_textbox_simple(),
    plot.caption = element_markdown(hjust = 0, color = "darkgray"),
    plot.caption.position = "plot",
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(color = "darkgray"),
    panel.grid.major.x = element_line(color = "gray", size = 0.1),
    panel.grid.major.y = element_line(color = "gray", size = 0.1, linetype = "dotted")
  )

main_plot
ggsave("30_year_mortgage_fed_arrow.png", width = 6, height = 4)  


