# Set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(dplyr)
library(openxlsx)
library(ggh4x)

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
df$ancestry_habitat <- ifelse(df$condition %in% c("a", "b"), "Agar\nancestry", "Scaffold\nancestry")
df$growing_habitat <- ifelse(df$condition %in% c("a", "d"), "agar", "scaffold")


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


# Define colors for agar and scaffold
agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

# Create the plot
p <- ggplot(df, aes(x = growing_habitat, y = nb_pumps, fill = as.factor(growing_habitat))) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.01) +
  #geom_jitter(shape = 16, position = position_jitter(0.01), show.legend = FALSE, cex = 0.2) +
  facet_wrap2(
    ~ ancestry_habitat,
    scales = "free_x",
    ncol = 2,
    strip = strip_themed(
      background_x = list(
        element_rect(fill = agar_color, color = NA),
        element_rect(fill = scaffold_color, color = NA)
      ),
      text_x = list(
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 0, b = 1)),
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 0, b = 0))
      )
    )
  ) +
  labs(
    y = "Number of pumps/10 sec",
    x = "Growing habitat",
  ) +
  scale_fill_manual(values = c("agar" = agar_color, "scaffold" = scaffold_color)) +
  scale_x_discrete(labels = c("agar" = "Agar", "scaffold" = "Scaffold")) +  # Rename x axis labels
  theme_minimal() +
  theme(
    text = element_text(size = 8),
    axis.text.y = element_text(size = 8, color = "black"),  # Increased from 12 to 16
    axis.text.x = element_text(size = 8, angle = 45, color = "black", margin = margin(t = -5, b = 0), hjust = 1),  # Increased from 12 to 16
    #axis.title = element_text(size = 6, face = "plain", margin = margin(t = 2, b = 2)),  # Added margin
    axis.title.x = element_text(margin = margin(t = 2)),  # Added extra margin for x-axis title
    axis.title.y = element_text(margin = margin(r = 2)),  # Added extra margin for y-axis title
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    #panel.spacing = unit(0.5, "lines"),
    strip.background = element_blank(),
    #strip.text = element_text(size = 8, face = "bold"),
    strip.text.x = element_text(margin = margin(t = 0, b = 0)),  # Reduced top and bottom margins
    #aspect.ratio = 7,
    plot.margin = unit(c(0, 0, 0, 0), "mm"),  # Remove margins around the plot
    panel.grid.major.x = element_blank(),  # Remove x axis grid lines
    panel.grid.minor.x = element_blank()   # Remove minor x axis grid lines if present
  )

#print(p)

# Save the plot with no white space
ggsave("C:/Users/aurel/Documents/Apples/pumping/pumping_analysis_paper.pdf", p, dpi = 300, bg = "white", width = 35, height = 60, units = "mm")



