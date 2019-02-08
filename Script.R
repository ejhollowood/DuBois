# Load libraries
library(tidyverse)

# Create tibble with data
pct <- c(99, 92.1, 81.6, 67.27, 50)
df <- tibble(pct)

# Create vectors of plot labels
year_label <- c("(1900?)", "1890", "1880", "1870", "1860")
pct_label <- c("99%", "92.1%", "81.6%", "67.27%", "(50%?)")

# Create plot with vertical bars
bar_plot <- ggplot(df) + 
  geom_bar(aes(x = -pct, y = pct), stat = "identity", width = 1.89, fill = "black")
  
# Create function to add horizontal bars
add_horizontal <- function(value, b_or_w, seg_width){
 geom_segment(aes_string(x = -value, y = value, xend = -100, yend = value), 
                   stat = "identity", size = seg_width, colour = b_or_w, lineend = "square")
}

# For loop to run function for all horizontal bars
  # first, the black horiztonal segments (the "outline")
  for(i in seq_along(pct)){
    bar_plot <- bar_plot + add_horizontal(value = pct[i], b_or_w = "black", seg_width = 2.2) 
  }
  # then, the white horizontal segments (the "fill")
  for(i in seq_along(pct)){
    bar_plot <- bar_plot + add_horizontal(value = pct[i], b_or_w = "white", seg_width = 2) 
  }

# Customise plot
bar_plot <- bar_plot +
    # x axis labels
    scale_x_continuous(breaks = -pct, 
                       limits = c(-103, -45), 
                       label = pct_label,
                       expand=c(0,0),
                       name = str_wrap("PERCENT OF ILLITERACY.", 10)) +
      
    # y axis labels
    scale_y_continuous(breaks = c(50, 67.27, 81.6, 92.1, 99), 
                       limits = c(0, 103),
                       label = year_label,
                       expand=c(0,0)) +

    # Modify theme
    theme(plot.margin = margin(20, 40, 20, 40),
          text = element_text(family = "Rajdhani", size = 8, colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(0,0,10,0)),
          axis.title.x = element_text(hjust = -0.2, vjust = 6.4, size = 5),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank()) +
    
    # Add title
    labs(title = "ILLITERACY.") +

# Save as png (sizing won't work in R Studio)
ggsave("illiteracy_ggplot.png", height = 12, width = 9, unit = "cm")