source("mapping/sp_data_prep.R")
library(ggthemes)
library(extrafont)
library(patchwork)
library(gridExtra)

theme_map <- theme(legend.position = c(0.5, -0.09),
                   legend.direction = "horizontal",
                  text = element_text(family = "DejaVu Sans Mono",
                                      color = "lightpink",
                                      margin = margin(t = 10)),
                  plot.title =  element_text(size = 11,
                                             hjust = 0.5),
                  plot.subtitle = element_text(size = 8,
                                               hjust = 0.5),
                  plot.caption = element_text(size = 5,
                                              hjust = 0.5),
                  axis.line = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  plot.background = element_rect(fill = "#244747"),
                  legend.background = element_rect(fill = "#244747"),
                  legend.box.background = element_rect(fill = "#244747", colour = NA),
                  panel.background = element_rect(fill= "#244747"),
                  axis.ticks.length.x = unit(3, "lines"),
                  axis.ticks.length.y = unit(1, "lines"),
                  panel.grid = element_blank(),
                  plot.margin = margin(5, 110, 5, 110)
                  )

theme_plot <- theme(text = element_text(family = "DejaVu Sans Mono",
                                       color = "lightpink",
                                       margin = margin(t = 10)),
                   plot.title =  element_text(size = 11,
                                              hjust = 0.5),
                   plot.subtitle = element_text(size = 8,
                                                hjust = 0.5),
                   plot.caption = element_text(size = 5,
                                               hjust = 0.5),
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   axis.text = element_text(color = "#9ae5de"),
                   plot.background = element_rect(fill = "#244747"),
                   legend.background = element_rect(fill = "#244747"),
                   legend.box.background = element_rect(fill = "#244747", colour = NA),
                   panel.background = element_rect(fill= "#244747"),
                   plot.margin = margin(5, 100, 5, 100))


my_map <- county_matched_processed %>%
  relocate(geometry, .after = poverty) %>%
  ggplot() +
  geom_sf(aes(fill = poverty), color = NA, size = 0.1) +
  labs(title = "Poverty rates in each county(delegation)",
       subtitle = "A clear concentraion of well been in the capital, and the costs.",
       caption = "grey areas are waterbodies") +
  scale_fill_viridis_c() +
  guides(fill = guide_legend(title = "poverty rates",
                      title.position = "top",
                      title.hjust = 0.5,
                      title.vjust = 5,
                      label.vjust = -1,
                      label.hjust = -1)) +
  theme_map


my_plot <- unnested_data %>%
  rename(states = "gov_name_f") %>%
  group_by(states) %>%
  summarize(avg_poverty = mean(poverty),
            avg_dropout = mean(dropout_rate_secondary)) %>%
  mutate(states = fct_reorder(states,avg_poverty)) %>%
  ggplot() +
  geom_col(aes(x = states, 
               y = avg_poverty)) +
  geom_point(aes(x = states, y = avg_dropout), 
             color = "#d4dddd", stat = "identity")+
  coord_flip() +
  labs(title = "Poverty and dropout rates for each state",
       subtitle = "Withour the porper statistical tools we can't really- \nconclude if there is a relation between both variables",
       y = "Average poverty(bars)\nAverage dropouts(points)",
       caption = "github : @bennour007 | twitter : @bennour007sin") +
  theme_plot


my_map + my_plot
