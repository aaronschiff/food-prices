library(ggplot2)
library(ggthemes)

# Basic elements of a clean ggplot theme
clean_theme <- function(base_size = 12, base_family = "Helvetica", ...) {
  theme_foundation(base_size = base_size, base_family = base_family) + 
    theme(
      # Plot
      plot.title = element_text(face = "bold", size = rel(1.1), hjust = 0), 
      plot.margin = unit(c(10, 10, 10, 10), "mm"), 
      plot.background = element_rect(fill = "white", colour = "white"), 
      
      # Axes
      axis.line.y = element_blank(), 
      axis.line.x = element_blank(), 
      axis.ticks.x = element_line(colour = "#aaaaaa"), 
      axis.ticks.y = element_blank(), 
      axis.title = element_text(size = rel(1)), 
      axis.title.y = element_text(vjust = 0.95, angle = 0), 
      axis.title.x = element_text(hjust = 1, margin = unit(c(1.5, 0, 0, 0), "lines")), 
      axis.text = element_text(size = rel(0.9)), 
      
      # Gridlines
      panel.grid.major.y = element_line(colour = "#e8e8e8"), 
      panel.grid.major.x = element_blank(), 
      panel.grid.minor.y = element_blank(), 
      panel.grid.minor.x = element_blank(), 
      
      # Panel elements
      panel.border = element_blank(), 
      panel.background = element_rect(colour = "white"), 
      panel.spacing = unit(2, "lines"), 
      
      # Facets
      strip.background = element_blank(), 
      strip.text = element_text(face = "bold", hjust = 0, size = rel(1)),
      
      # Legend
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1)), 
      legend.title = element_text(face = "bold", size = rel(1))
    ) + 
    theme(...)
}

