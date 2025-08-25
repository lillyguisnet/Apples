library(ggplot2)
library(dplyr)
library(openxlsx)
library(ggh4x)
library(tidyr)

# --- NEW: Custom Shape Classification Function ---
# This function translates the logic from the Python script to classify shapes
# based on amplitude and curvature thresholds.
classify_shapes_custom_r <- function(df, 
                                     amp_col = "smoothed_max_amplitudes", 
                                     curv_col = "curvature_time_series",
                                     amp_high_thresh = 22.0, 
                                     amp_mod_thresh = 15.0,
                                     curv_high_thresh = 1.8, 
                                     curv_mod_thresh = 0.5) {
  
  # Ensure the required columns exist
  if (!amp_col %in% names(df) || !curv_col %in% names(df)) {
    stop("Amplitude or curvature columns not found in the dataframe.")
  }
  
  df %>%
    mutate(
      # Use absolute value for amplitude and curvature thresholds
      custom_shape = case_when(
        # O-shape: High amplitude + high curvature
        abs(.data[[amp_col]]) >= amp_high_thresh & abs(.data[[curv_col]]) >= curv_high_thresh ~ "O-shape",
        
        # 6-shape: Moderate amplitude + high curvature
        abs(.data[[amp_col]]) >= amp_mod_thresh & abs(.data[[amp_col]]) < amp_high_thresh & abs(.data[[curv_col]]) >= curv_high_thresh ~ "6-shape",
        
        # U-shape: High amplitude + moderate curvature
        abs(.data[[amp_col]]) >= amp_high_thresh & abs(.data[[curv_col]]) >= curv_mod_thresh & abs(.data[[curv_col]]) < curv_high_thresh ~ "U-shape",
        
        # C-shape: Moderate amplitude + moderate curvature
        abs(.data[[amp_col]]) >= amp_mod_thresh & abs(.data[[amp_col]]) < amp_high_thresh & abs(.data[[curv_col]]) >= curv_mod_thresh & abs(.data[[curv_col]]) < curv_high_thresh ~ "C-shape",
        
        # S-shape: Everything else
        TRUE ~ "S-shape"
      )
    )
}


# --- Data Loading and Initial Processing ---

# Define file path and load data
xl_file <- "C:/Users/aurel/Documents/Apples/crawling/data/allmerged_merged_timeseries_data.csv"
df <- as.data.frame(read.csv(xl_file))

df <- df %>%
  extract(video_id, into = c("environment", "worm_condition", "uid"), 
          regex = "([a-zA-Z]+)-([a-zA-Z])-([0-9]+-[0-9]+)", remove = FALSE)

# Convert environment and worm_condition to factors
df$environment <- as.factor(df$environment)
df$worm_condition <- as.factor(df$worm_condition)

# --- APPLY THE NEW SHAPE CLASSIFICATION ---
# We use the same thresholds as specified in the Python script's main() function.
# This adds a new column called 'custom_shape' to the dataframe.
df <- classify_shapes_custom_r(
  df,
  amp_high_thresh = 0.3,
  amp_mod_thresh = 0.1,
  curv_high_thresh = 1.5,
  curv_mod_thresh = 1
)

# Plot the distribution of smoothed_max_amplitudes
ggplot(df, aes(x = smoothed_max_amplitudes)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  labs(title = "Distribution of smoothed_max_amplitudes", x = "smoothed_max_amplitudes", y = "Count") +
  theme_minimal()

# Plot the distribution of curvature_time_series
ggplot(df, aes(x = curvature_time_series)) +
  geom_histogram(bins = 50, fill = "salmon", color = "black") +
  labs(title = "Distribution of curvature_time_series", x = "curvature_time_series", y = "Count") +
  theme_minimal()

# --- NEW: Calculate Percentages for Custom Shapes ---

# Calculate percentages for the new custom shapes for each video
custom_shape_percentages <- df %>%
  group_by(environment, worm_condition, uid) %>%
  summarise(
    total_frames = n(),

    # Count each of the new custom shapes, excluding z-axis turns
    o_shapes = sum(custom_shape == "O-shape", na.rm = TRUE),
    `6_shapes` = sum(custom_shape == "6-shape", na.rm = TRUE), # Use backticks for names starting with a number
    u_shapes = sum(custom_shape == "U-shape", na.rm = TRUE),
    c_shapes_custom = sum(custom_shape == "C-shape", na.rm = TRUE),
    s_shapes_custom = sum(custom_shape == "S-shape", na.rm = TRUE),
    
    # Calculate proportion of total frames for each shape (0-1 scale)
    o_shape_percentage = (o_shapes / total_frames),
    `6_shape_percentage` = (`6_shapes` / total_frames),
    u_shape_percentage = (u_shapes / total_frames),
    c_shape_custom_percentage = (c_shapes_custom / total_frames),
    s_shape_custom_percentage = (s_shapes_custom / total_frames),
    
  )

# Join the new custom shape percentages back to the main dataframe
df_final <- df %>%
  left_join(custom_shape_percentages, by = c("environment", "worm_condition", "uid"))

# Save the final results to a new CSV file
#write.csv(df_final, "crawling/data/combined_data_with_custom_shape_percentages.csv", row.names = FALSE)

# You can now proceed with any further analysis or plotting using the new 'custom_shape' column
# and the calculated percentages.


#Plot the custom shape percentages

# Reshape data for plotting with the new custom shapes
plot_data <- custom_shape_percentages %>%
  mutate(
    ancestry = case_when(
      worm_condition %in% c("a", "b") ~ "Agar ancestry",
      TRUE ~ "Scaffold ancestry"
    ),
    growing = case_when(
      worm_condition %in% c("a", "d") ~ "Agar growth",
      TRUE ~ "Scaffold growth"
    )
  ) %>%
  pivot_longer(
    cols = c(o_shape_percentage, `6_shape_percentage`, u_shape_percentage,
             c_shape_custom_percentage, s_shape_custom_percentage),
    names_to = "shape_type",
    values_to = "percentage"
  ) %>%
  mutate(
    shape_type = factor(case_when(
      shape_type == "o_shape_percentage" ~ "O",
      shape_type == "6_shape_percentage" ~ "6",
      shape_type == "u_shape_percentage" ~ "U",
      shape_type == "c_shape_custom_percentage" ~ "C",
      shape_type == "s_shape_custom_percentage" ~ "S"
    ), levels = c("O", "6", "U", "C", "S"))
  )




#Plot shape by frame for 1 video
# For per-frame shape coloring, we need a data frame with one row per frame and a shape_type for each frame.
# Assuming you have such a data frame, e.g., df_final with columns: frames, shape_type, uid, etc.

ggplot(df_final %>% filter(uid == "03032022191610-0000"), aes(x = frames, y = 1)) +
  geom_tile(aes(fill = custom_shape)) +
  scale_fill_manual(
    values = c(
      "O-shape" = "red",
      "6-shape" = "blue",
      "U-shape" = "green",
      "C-shape" = "yellow",
      "S-shape" = "purple"
    ),
    na.value = "grey90"
  ) +
  labs(
    x = "Frame",
    y = "",
    fill = "Shape"
  ) +
  scale_x_continuous(
    breaks = function(x) seq(floor(min(x, na.rm = TRUE)/25)*25, ceiling(max(x, na.rm = TRUE)/25)*25, by = 25)
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    aspect.ratio = 0.2
  )





####Plots for paper
agar_color <- "#66C2A5"
scaffold_color <- "#FC8D62"

# Plot the custom shape percentages

# Relabel and reorder environment: "nofood" = "none", "food" = "low", "thick" = "high"
plot_data$environment <- factor(
  plot_data$environment,
  levels = c("nofood", "food", "thick"),
  labels = c("none", "low", "high")
)

p <- ggplot(plot_data, aes(x = shape_type, y = percentage, fill = growing)) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.01) +
  facet_nested(
    environment ~ ancestry + growing,
    scales = "fixed",
    labeller = labeller(
      ancestry = c("Agar ancestry" = "Agar ancestry", "Scaffold ancestry" = "Scaffold ancestry"),
      growing = c("Agar growth" = "Agar growth", "Scaffold growth" = "Scaffold growth"),
      environment = c("none" = "none", "low" = "low", "high" = "high")
    ),
    strip = strip_nested(
      background_x = list(
        element_rect(fill = agar_color, color = "white", linewidth = 0.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 0.5)
      ),
      background_y = element_rect(fill = "gray85", color = "white", linewidth = 1.5),
      text_x = list(
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 1, b = 1)),
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 1, b = 1))
      )
    )
  ) +
  scale_fill_manual(values = c("Agar growth" = agar_color, "Scaffold growth" = scaffold_color)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0, 1), labels = function(x) ifelse(x == 0, "0", sub("^0\\.", ".", sprintf("%.2f", x)))) +
  labs(
    x = "Shape",
    y = "Proportion of time"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 8),
    axis.text.y = element_text(size = 8, color = "black", margin = margin(r = 0)),
    axis.text.x = element_text(size = 8, color = "black"),
    #axis.title = element_text(size = 8, face = "plain", margin = margin(t = 22, b = 20)),
    axis.title.x = element_text(margin = margin(t = 2)),
    axis.title.y = element_text(margin = margin(r = 1)),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0.1, "lines"),
    strip.background = element_blank(),
    #strip.text = element_text(size = 24, face = "plain"),
    strip.text.x = element_text(margin = margin(t = 0, b = 0)),
    strip.text.y = element_text(margin = margin(l = 2, r = 2)),
    plot.margin = unit(c(0, 0, 0, 0), "mm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    ##panel.grid.major.y = element_line(color = "gray90"),
    panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.1),
    panel.grid.minor.y = element_blank()
  )

ggsave("C:/Users/aurel/Documents/Apples/crawling/plots/custom_shape_percentage_boxplot_by_environment_paper.png", p, dpi = 300, bg = "white", width = 90, height = 70, units = "mm")


## --- NEW: Enhanced Sinuosity Plot (styled like swimming plot) ---

# Prepare data with ancestry and growing categories
sinuosity_plot_data <- path_data_parsed %>%
  mutate(
    ancestry = case_when(
      worm_condition %in% c("a", "b") ~ "Agar ancestry",
      TRUE ~ "Scaffold ancestry"
    ),
    growing = case_when(
      worm_condition %in% c("a", "d") ~ "Agar growth",
      TRUE ~ "Scaffold growth"
    )
  )

# Build the plot with matching styling
plot_sinuosity2 <- ggplot(sinuosity_plot_data, aes(x = factor(environment, levels = c("nofood", "food", "thick")), y = sinuosity, fill = growing)) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.01) +
  facet_nested(
    ~ ancestry + growing,
    scales = "fixed",
    labeller = labeller(
      ancestry = c("Agar ancestry" = "Agar ancestry", "Scaffold ancestry" = "Scaffold ancestry"),
      growing  = c("Agar growth" = "Agar growth", "Scaffold growth" = "Scaffold growth")
    ),
    strip = strip_nested(
      background_x = list(
        element_rect(fill = agar_color,     color = "white", linewidth = 0.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 0.5)
      ),
      text_x = list(
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 2, b = 2)),
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 2, b = 2))
      )
    )
  ) +
  labs(
    x = "Food density",
    y = "Tortuosity"
  ) +
  scale_fill_manual(values = c("Agar growth" = agar_color, "Scaffold growth" = scaffold_color)) +
  scale_x_discrete(labels = c("nofood" = "none", "food" = "low", "thick" = "high"), limits = c("nofood", "food", "thick")) +
  scale_y_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
  theme_minimal() +
  theme(
    text = element_text(size = 8),
    axis.text.y = element_text(size = 8, color = "black", margin = margin(r = 0)),
    axis.text.x = element_text(size = 8, color = "black", angle = 45, hjust = 1, margin = margin(t = -6)),
    axis.title.x = element_text(margin = margin(t = 1)),
    axis.title.y = element_text(margin = margin(r = 1)),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0.1, "lines"),
    strip.background = element_blank(),
    strip.text.x = element_text(margin = margin(t = 0, b = 0)),
    plot.margin = unit(c(0, 0, 0, 0), "mm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    ##panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    #panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.1)
  )

# Save the plot
out_path <- "crawling/plots/path_sinuosity_boxplot_by_environment.png"
ggsave(out_path, plot_sinuosity2, dpi = 300, bg = "white", width = 85, height = 60, units = "mm")




###Speed plot

# Load path analysis data if not already loaded
if (!exists("path_data_parsed")) {
  file_path <- "crawling/data/allmerged_merged_path_analysis.csv"
  path_data <- read.csv(file_path)
  
  # Parse the video_id column
  path_data_parsed <- path_data %>%
    extract(video_id, into = c("environment", "worm_condition", "uid"), 
            regex = "([a-zA-Z]+)-([a-zA-Z])-([0-9]+-[0-9]+)", remove = FALSE)
  
  # Convert environment and worm_condition to factors
  path_data_parsed$environment <- as.factor(path_data_parsed$environment)
  path_data_parsed$worm_condition <- as.factor(path_data_parsed$worm_condition)
}

# Prepare data with ancestry and growing categories
speed_plot_data <- path_data_parsed %>%
  mutate(
    ancestry = case_when(
      worm_condition %in% c("a", "b") ~ "Agar ancestry",
      TRUE ~ "Scaffold ancestry"
    ),
    growing = case_when(
      worm_condition %in% c("a", "d") ~ "Agar growth",
      TRUE ~ "Scaffold growth"
    )
  ) %>%
  select(environment, ancestry, growing, avg_forward_speed, avg_backward_speed) %>%
  pivot_longer(
    cols = c(avg_forward_speed, avg_backward_speed),
    names_to = "direction",
    values_to = "speed"
  ) %>%
  mutate(
    direction = factor(case_when(
      direction == "avg_forward_speed" ~ "Forward",
      direction == "avg_backward_speed" ~ "Backward"
    ), levels = c("Forward", "Backward"))
  ) %>%
  filter(speed > 0)

# Relabel and reorder environment: "nofood" = "none", "food" = "low", "thick" = "high"
speed_plot_data$environment <- factor(
  speed_plot_data$environment,
  levels = c("nofood", "food", "thick"),
  labels = c("none", "low", "high")
)

# Build the plot with similar styling to the shape percentage plot
plot_speed <- ggplot(speed_plot_data, aes(x = direction, y = speed, fill = growing)) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.01) +
  facet_nested(
    environment ~ ancestry + growing,
    scales = "fixed",
    labeller = labeller(
      ancestry = c("Agar ancestry" = "Agar ancestry", "Scaffold ancestry" = "Scaffold ancestry"),
      growing = c("Agar growth" = "Agar growth", "Scaffold growth" = "Scaffold growth"),
      environment = c("nofood" = "none", "food" = "low", "thick" = "high")
    ),
    strip = strip_nested(
      background_x = list(
        element_rect(fill = agar_color, color = "white", linewidth = 0.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 0.5)
      ),
      background_y = element_rect(fill = "gray85", color = "white", linewidth = 1.5),
      text_x = list(
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 1, b = 1)),
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 1, b = 1))
      )
    )
  ) +
  scale_fill_manual(values = c("Agar growth" = agar_color, "Scaffold growth" = scaffold_color)) +
  scale_y_continuous(limits = c(0, max(speed_plot_data$speed, na.rm = TRUE)), breaks = function(x) seq(0, 2, by = 1)) +
  labs(
    x = "Direction of motion",
    y = "Average speed (pixels/frame)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 8),
    axis.text.y = element_text(size = 8, color = "black", margin = margin(r = 0)),
    axis.text.x = element_text(size = 8, color = "black", angle = 45, hjust = 1, margin = margin(t = -2)),
    #axis.title = element_text(size = 8, face = "plain", margin = margin(t = 22, b = 20)),
    axis.title.x = element_text(margin = margin(t = 2)),
    axis.title.y = element_text(margin = margin(r = 1)),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0.1, "lines"),
    strip.background = element_blank(),
    #strip.text = element_text(size = 24, face = "plain"),
    strip.text.x = element_text(margin = margin(t = 0, b = 0)),
    strip.text.y = element_text(margin = margin(l = 2, r = 2)),
    plot.margin = unit(c(0, 0, 0, 0), "mm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    ##panel.grid.major.y = element_line(color = "gray90"),
    panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.1),
    panel.grid.minor.y = element_blank()
  )

# Save the plot
ggsave("crawling/plots/avg_speed_direction_boxplot_by_environment_paper.png", plot_speed, dpi = 300, bg = "white", width = 90, height = 70, units = "mm")



###Portion of direction of motion plot

# Prepare data for proportion of time in each direction (Forward, Backward, Stationary)
direction_prop_data <- path_data_parsed %>%
  mutate(
    total_frames = 600,
    ancestry = case_when(
      worm_condition %in% c("a", "b") ~ "Agar ancestry",
      TRUE ~ "Scaffold ancestry"
    ),
    growing = case_when(
      worm_condition %in% c("a", "d") ~ "Agar growth",
      TRUE ~ "Scaffold growth"
    )
  ) %>%
  select(environment, ancestry, growing, total_frames, forward_frames, backward_frames, stationary_frames) %>%
  pivot_longer(
    cols = c(forward_frames, backward_frames, stationary_frames),
    names_to = "direction",
    values_to = "frames"
  ) %>%
  mutate(
    proportion = frames / total_frames,
    direction = factor(case_when(
      direction == "forward_frames" ~ "Forward",
      direction == "backward_frames" ~ "Backward",
      direction == "stationary_frames" ~ "Stationary"
    ), levels = c("Forward", "Backward", "Stationary"))
  )

# Relabel and reorder environment as before
direction_prop_data$environment <- factor(
  direction_prop_data$environment,
  levels = c("nofood", "food", "thick"),
  labels = c("none", "low", "high")
)

# Build the plot
plot_direction_prop <- ggplot(direction_prop_data, aes(x = direction, y = proportion, fill = growing)) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.01) +
  facet_nested(
    environment ~ ancestry + growing,
    scales = "fixed",
    labeller = labeller(
      ancestry = c("Agar ancestry" = "Agar ancestry", "Scaffold ancestry" = "Scaffold ancestry"),
      growing = c("Agar growth" = "Agar growth", "Scaffold growth" = "Scaffold growth"),
      environment = c("none" = "none", "low" = "low", "high" = "high")
    ),
    strip = strip_nested(
      background_x = list(
        element_rect(fill = agar_color, color = "white", linewidth = 0.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 0.5)
      ),
      background_y = element_rect(fill = "gray85", color = "white", linewidth = 1.5),
      text_x = list(
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 1, b = 1)),
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 1, b = 1))
      )
    )
  ) +
  scale_fill_manual(values = c("Agar growth" = agar_color, "Scaffold growth" = scaffold_color)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0, 1), labels = function(x) ifelse(x == 0, "0", sub("^0\\.", ".", sprintf("%.2f", x)))) +
  labs(
    x = "Direction of motion",
    y = "Proportion of time"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 8),
    axis.text.y = element_text(size = 8, color = "black", margin = margin(r = 0)),
    axis.text.x = element_text(size = 8, color = "black", angle = 45, hjust = 1, margin = margin(t = -2)),
    axis.title.x = element_text(margin = margin(t = 2)),
    axis.title.y = element_text(margin = margin(r = 1)),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0.1, "lines"),
    strip.background = element_blank(),
    strip.text.x = element_text(margin = margin(t = 0, b = 0)),
    strip.text.y = element_text(margin = margin(l = 2, r = 2)),
    plot.margin = unit(c(0, 0, 0, 0), "mm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.1),
    panel.grid.minor.y = element_blank()
  )

# Save the plot
ggsave("crawling/plots/proportion_direction_boxplot_by_environment_paper.png", plot_direction_prop, dpi = 300, bg = "white", width = 90, height = 70, units = "mm")


### Distance-related path metrics -------------------------------------------------

# Prepare a reusable data frame with ancestry / growing factors
path_metrics_data <- path_data_parsed %>%
  mutate(
    ancestry = case_when(
      worm_condition %in% c("a", "b") ~ "Agar ancestry",
      TRUE ~ "Scaffold ancestry"
    ),
    growing = case_when(
      worm_condition %in% c("a", "d") ~ "Agar growth",
      TRUE ~ "Scaffold growth"
    )
  )

# Add relative time column (when furthest point reached)
path_metrics_data <- path_metrics_data %>%
  mutate(furthest_point_when = furthest_point_frame / 600)

# Helper to build a styled plot for a given y-variable and y-label
build_metric_plot <- function(df, y_var, y_lab) {
  ggplot(df, aes(x = factor(environment, levels = c("nofood", "food", "thick")), y = .data[[y_var]], fill = growing)) +
    geom_boxplot(lwd = 0.1, outlier.size = 0.01) +
    facet_nested(
      ~ ancestry + growing,
      scales = "fixed",
      labeller = labeller(
        ancestry = c("Agar ancestry" = "Agar ancestry", "Scaffold ancestry" = "Scaffold ancestry"),
        growing  = c("Agar growth" = "Agar growth", "Scaffold growth" = "Scaffold growth")
      ),
      strip = strip_nested(
        background_x = list(
          element_rect(fill = agar_color,     color = "white", linewidth = 0.5),
          element_rect(fill = scaffold_color, color = "white", linewidth = 0.5)
        ),
        text_x = list(
          element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 2, b = 2)),
          element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 2, b = 2))
        )
      )
    ) +
    labs(
      x = "Food density",
      y = y_lab
    ) +
    scale_fill_manual(values = c("Agar growth" = agar_color, "Scaffold growth" = scaffold_color)) +
    scale_x_discrete(labels = c("nofood" = "none", "food" = "low", "thick" = "high"), limits = c("nofood", "food", "thick")) +
    theme_minimal() +
    theme(
      text = element_text(size = 8),
      axis.text.y = element_text(size = 8, color = "black", margin = margin(r = 0)),
      axis.text.x = element_text(size = 8, color = "black", angle = 45, hjust = 1, margin = margin(t = -6)),
      axis.title.x = element_text(margin = margin(t = 1)),
      axis.title.y = element_text(margin = margin(r = 1)),
      legend.position = "none",
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.spacing = unit(0.1, "lines"),
      strip.background = element_blank(),
      strip.text.x = element_text(margin = margin(t = 0, b = 0)),
      plot.margin = unit(c(0, 0, 0, 0), "mm"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
}

# Total distance traveled -------------------------------------------------------
plt_total_distance <- build_metric_plot(path_metrics_data, "total_distance", "Total distance travelled (pixels)") +
  theme(axis.text.y = element_text(size = 8, color = "black", margin = margin(r = 0), angle = 90, hjust = 0.5),
        axis.text.x = element_text(size = 8, color = "black", angle = 45, hjust = 1, margin = margin(t = 2)))

ggsave("crawling/plots/path_total_distance_boxplot_by_environment.png", plt_total_distance, dpi = 300, bg = "white", width = 85, height = 60, units = "mm")

# Furthest point distance -------------------------------------------------------
plt_furthest_dist <- build_metric_plot(path_metrics_data, "furthest_point_distance", "Furthest distance from start (pixels)") +
  theme(axis.text.y = element_text(size = 8, color = "black", margin = margin(r = 0), angle = 90, hjust = 0.5))

ggsave("crawling/plots/path_furthest_distance_boxplot_by_environment.png", plt_furthest_dist, dpi = 300, bg = "white", width = 85, height = 60, units = "mm")

# Relative time when furthest point reached -------------------------------------
plt_furthest_when <- build_metric_plot(path_metrics_data, "furthest_point_when", "Fraction of recording time\nwhen furthest distance is reached") +
  #theme(axis.text.y = element_text(size = 8, color = "black", margin = margin(r = -2))) +
  scale_y_continuous(
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    limits = c(0, 1),
    labels = function(x) {
      sapply(x, function(val) {
        if (val == 0) {
          "0"
        } else if (val == 1) {
          "1"
        } else {
          sub("^0\\.", ".", sprintf("%.1f", val))
        }
      })
    }
  )

ggsave("crawling/plots/path_furthest_when_boxplot_by_environment.png", plt_furthest_when, dpi = 300, bg = "white", width = 85, height = 60, units = "mm")




###Head bend analysis -----------------------------------------------------------

# Load head-bend analysis data if not already loaded
if (!exists("head_data_parsed")) {
  head_file <- "crawling/data/allmerged_merged_head_bend_analysis.csv"
  head_data <- read.csv(head_file)

  head_data_parsed <- head_data %>%
    extract(video_id, into = c("environment", "worm_condition", "uid"),
            regex = "([a-zA-Z]+)-([a-zA-Z])-([0-9]+-[0-9]+)", remove = FALSE)

  head_data_parsed$environment <- as.factor(head_data_parsed$environment)
  head_data_parsed$worm_condition <- as.factor(head_data_parsed$worm_condition)
}

# Prepare metrics data with ancestry / growing and derived depth metric
head_metrics_data <- head_data_parsed %>%
  mutate(
    ancestry = case_when(
      worm_condition %in% c("a", "b") ~ "Agar ancestry",
      TRUE ~ "Scaffold ancestry"
    ),
    growing = case_when(
      worm_condition %in% c("a", "d") ~ "Agar growth",
      TRUE ~ "Scaffold growth"
    ),
    avg_abs_depth = (abs(avg_peak_depth) + abs(avg_trough_depth)) / 2
  )

# avg_bend_frequency plot -------------------------------------------------------
plt_bend_freq <- build_metric_plot(head_metrics_data, "avg_bend_frequency", "Average head bend frequency (Hz)")

ggsave("crawling/plots/head_avg_bend_frequency_boxplot_by_environment.png", plt_bend_freq, dpi = 300, bg = "white", width = 85, height = 60, units = "mm")

# avg_abs_depth plot ------------------------------------------------------------
plt_abs_depth <- build_metric_plot(head_metrics_data, "avg_abs_depth", "Average head bend depth (pixels)")

ggsave("crawling/plots/head_avg_abs_depth_boxplot_by_environment.png", plt_abs_depth, dpi = 300, bg = "white", width = 85, height = 60, units = "mm")







