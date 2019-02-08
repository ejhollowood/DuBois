# Set up
library(tidyverse)

# Create tibble with data
pct <- c(99, 92.1, 81.6, 67.27, 50)
df <- tibble(pct) %>%
         mutate(xend = -100,
         m_pct = - pct)

# Create vectors of plot labels
year_label <- c("(1900?)", "1890", "1880", "1870", "1860")
pct_label <- c("99%", "92.1%", "81.6%", "67.27%", "(50%?)")

# Plot
bar_plot <- ggplot(df) + 
  # Vertical bars
  geom_bar(aes(x = -pct, y = pct), stat = "identity", width = 1.89, fill = "black") +
  # Horizontal black bars (the "outline")
  geom_segment(aes(x = m_pct, y = pct, xend = xend, yend = pct), 
               stat = "identity", size = 2.2, colour = "black", lineend = "square") +
  # Horizontal white bars (the "fill")
  geom_segment(aes(x = m_pct, y = pct, xend = xend, yend = pct), 
               stat = "identity", size = 2, colour = "white", lineend = "square") +
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
