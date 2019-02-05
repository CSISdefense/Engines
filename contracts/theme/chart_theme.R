library(ggplot2)

chart_theme <- theme(
  plot.title = element_text(
    family = "Open Sans",
    color = "#554449",
    size = 16,
    face = "bold",
    margin = margin(20, 0, 20, 0),
    hjust = 0.5
  ),
  text = element_text(size = 10, family = "Open Sans"),
  panel.border = element_blank(),
  panel.background = element_rect(fill = "#FCFCFC", color = "#FCFCFC"),
  plot.background = element_rect(fill = "#FCFCFC", color = "#FCFCFC"),
  panel.grid.major.x = element_line(size = .1, color = "lightgray"),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_line(size = .1, color = "lightgray"),
  panel.grid.minor.y = element_line(size = .1, color = "lightgray"),
  legend.position = "right",
  legend.title = element_blank(),
  legend.text = element_text(size = 10, color = "#554449"),
  legend.key = element_rect(fill = "#FCFCFC"),
  legend.background = element_rect(fill = "#FCFCFC"),
  legend.key.width = unit(3, "line"),
  axis.text.x = element_text(
    size = 10,
    color = "#554449",
    margin = margin(0, 0, 0, 0)
  ),
  axis.ticks.length = unit(.00, "cm"),
  axis.text.y = element_text(
    size = 10,
    color = "#554449",
    margin = margin(0, 5, 0, 0)
  ),
  axis.title.x = element_text(
    size = 12,
    face = "bold",
    color = "#554449",
    margin = margin(15, 0, 0, 0)
  ),
  axis.title.y = element_text(
    size = 12,
    face = "bold",
    color = "#554449",
    margin = margin(0, 15, 0, 0)
  ), 
  panel.spacing.y = unit(1, "lines"),
  strip.background = element_rect(color = "#EDEDED", fill=c("#F2F2F2")),
  strip.text.x = element_text(family = "Open Sans",
                              size = rel(1),
                              face = "bold", 
                              color = "#554449"), 
  strip.text.y = element_text(family = "Open Sans",
                              size = rel(1),
                              face = "bold", 
                              color = "#554449") 
  
)
