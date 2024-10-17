# Set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(dplyr)
library(openxlsx)

xl_file <- "pumping_data.xlsx"
df <- as.data.frame(read.xlsx(xl_file))

csv_file <- "pumping_data.csv"
write.csv(df, csv_file, row.names = FALSE)

# Read the CSV content and copy it to the clipboard
csv_content <- paste(readLines(csv_file), collapse = "\n")
writeClipboard(csv_content)

png("pumps_av.png", width = 1600, height = 900)

ggplot(df, aes(condition, nb_pumps)) + ## df, x, y
  geom_boxplot(alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) + ## width = of boxes, lwd = line width of boxes
  geom_jitter(shape = 16, position = position_jitter(0.018), aes(color = as.factor(round)), show.legend = FALSE, cex = 2.8) +
  ylab("Number of pumps/10sec")

dev.off()


###Summary statistics
df %>%
  group_by(condition) %>%
  summarise(
    count = n(),
    mean_pumps = mean(nb_pumps),
    median_pumps = median(nb_pumps),
    sd_pumps = sd(nb_pumps),
    min_pumps = min(nb_pumps),
    max_pumps = max(nb_pumps)
  )





# Paper plot

ggplot(df, aes(condition, nb_pumps)) + ## df, x, y
  geom_boxplot(alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) + ## width = of boxes, lwd = line width of boxes
  geom_jitter(shape = 16, position = position_jitter(0.01), show.legend = FALSE, cex = 2.8) +
  ylab("Number of pumps/10sec") +
  theme_minimal() + # Use minimal theme to remove background
  theme(
    panel.grid = element_blank(), # Remove grid lines
    axis.text.y = element_text(size = 24), # Increase y-axis label size
    axis.title.y = element_text(size = 32, margin = margin(t = 0, r = 20, b = 0, l = 0)), # Increase y-axis title size and move it further from labels
    axis.line.y = element_line(color = "black", size = 0.5), # Add y-axis border line
    axis.ticks.y = element_line(color = "black", size = 0.5), # Add y-axis ticks
    axis.ticks.length.y = unit(0.25, "cm"), # Set length of y-axis ticks
    aspect.ratio = 1.5 # Increase the aspect ratio to make the plot narrower
  )



library(ggplot2)
library(gridExtra)
library(grid)

# Your existing plot code (unchanged)
p <- ggplot(df, aes(condition, nb_pumps)) +
  geom_boxplot(alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +
  geom_jitter(shape = 16, position = position_jitter(0.01), show.legend = FALSE, cex = 2.8) +
  ylab("Number of pumps/10sec") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 24),
    axis.title.y = element_text(size = 32, margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.line.y = element_line(color = "black", size = 0.5),
    axis.ticks.y = element_line(color = "black", size = 0.5),
    axis.ticks.length.y = unit(0.25, "cm"),
    aspect.ratio = 1.5,
    axis.title.x = element_blank(), # Remove x-axis title
    axis.text.x = element_blank(), # Remove x-axis labels
    axis.ticks.x = element_blank(), # Remove x-axis ticks
    plot.margin = margin(b = 0) # Remove bottom margin of the plot
  )

# Create the table data in a horizontal format
# Create the table data
table_data <- data.frame(
  Label = c("Ancestry habitat", "Growing habitat"),
  a = c("agar", "agar"),
  b = c("agar", "scaffold"),
  c = c("scaffold", "scaffold"),
  d = c("scaffold", "agar")
)

# Add ancestry_habitat and growing_habitat columns to df based on condition
df$ancestry_habitat <- ifelse(df$condition %in% c("a", "b"), "Agar ancestry", "Scaffold ancestry")
df$growing_habitat <- ifelse(df$condition %in% c("a", "d"), "agar", "scaffold")
)

# Create a grob (graphical object) for the horizontal table without column names
table_grob <- tableGrob(table_data, rows = NULL, cols = NULL, theme = ttheme_minimal(
  core = list(
    fg_params = list(fontsize = 14), # Increased font size
    bg_params = list(fill = c("white", "grey95"), col = "white")
  ),
  colhead = list(
    fg_params = list(fontsize = 0), # Set font size to 0 to hide column names
    bg_params = list(fill = "white", col = "white")
  ) # Set background to white
))

# Adjust the width of the first column (labels)
table_grob$widths[1] <- unit(6, "cm") # Increased width

# Adjust the width of other columns
table_grob$widths[2:5] <- unit(3, "cm") # Increased width of columns 2-5

# Remove the first row (which contained the column names)
table_grob <- gtable::gtable_filter(table_grob, "core-fg|core-bg")

# Remove top padding of the table
# table_grob$heights[1] <- unit(0, "cm")

# Combine the plot and table
combined_plot <- grid.arrange(
  p, table_grob,
  ncol = 1,
  heights = c(5, 1) # Adjusted height ratio to bring table closer to plot
)

# Display the combined plot
print(combined_plot)

# Scientific, minimal, simplistic ggplot theme with facet borders
scientific_theme <- theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    panel.grid.major = element_line(color = "grey90", size = 0.2),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    strip.background = element_rect(fill = "grey95", color = "black"),
    strip.text = element_text(size = 12, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.spacing = unit(0.5, "lines")
  )

# Update your existing plot with facets
p <- ggplot(df, aes(growing_habitat, nb_pumps, fill = as.factor(growing_habitat))) +
  geom_boxplot() +
  #geom_jitter(aes(shape = as.factor(ancestry_habitat), color = as.factor(growing_habitat)), size = 3) +  # Increased point size
  facet_wrap(~ancestry_habitat, scales = "free_x", ncol = 2) +
  labs(
    y = "Number of pumps/10sec",
    x = "Growing Condition",
    # title = "Pump Analysis by Ancestry and Growing Condition"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme(
    text = element_text(size = 14),  # Increased overall font size
    axis.text = element_text(size = 12),  # Increased axis text size
    axis.title = element_text(size = 16, face = "bold"),  # Increased axis title size
    strip.text = element_text(size = 14, face = "bold"),  # Increased facet label size
    legend.position = "none",  # Remove legend
    panel.background = element_blank(),  # Remove panel background
    plot.background = element_blank(),  # Remove plot background
    panel.spacing = unit(2, "lines")  # Increase space between facets
  )
# scientific_theme
p
# Display the plot
print(p)

# Save the plot
ggsave("scientific_pump_analysis_faceted_with_borders.png", p, width = 10, height = 8, dpi = 300)
