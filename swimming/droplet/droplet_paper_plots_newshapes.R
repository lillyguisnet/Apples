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
      # The case_when statement is the R equivalent of Python's if/elif/else chain
      custom_shape = case_when(
        # O-shape: High amplitude + high curvature
        .data[[amp_col]] >= amp_high_thresh & abs(.data[[curv_col]]) >= curv_high_thresh ~ "O-shape",
        
        # 6-shape: Moderate amplitude + high curvature
        .data[[amp_col]] >= amp_mod_thresh & .data[[amp_col]] < amp_high_thresh & abs(.data[[curv_col]]) >= curv_high_thresh ~ "6-shape",
        
        # U-shape: High amplitude + moderate curvature
        .data[[amp_col]] >= amp_high_thresh & abs(.data[[curv_col]]) >= curv_mod_thresh & abs(.data[[curv_col]]) < curv_high_thresh ~ "U-shape",
        
        # C-shape: Moderate amplitude + moderate curvature
        .data[[amp_col]] >= amp_mod_thresh & .data[[amp_col]] < amp_high_thresh & abs(.data[[curv_col]]) >= curv_mod_thresh & abs(.data[[curv_col]]) < curv_high_thresh ~ "C-shape",
        
        # S-shape: Everything else
        TRUE ~ "S-shape"
      )
    )
}


# --- Data Loading and Initial Processing ---

# Define file path and load data
xl_file <- "swimming/droplet/combined_data_all.csv"
df <- as.data.frame(read.csv(xl_file))

# --- APPLY THE NEW SHAPE CLASSIFICATION ---
# We use the same thresholds as specified in the Python script's main() function.
# This adds a new column called 'custom_shape' to the dataframe.
df <- classify_shapes_custom_r(
  df,
  amp_high_thresh = 22.0,
  amp_mod_thresh = 15.0,
  curv_high_thresh = 1.8,
  curv_mod_thresh = 0.5
)


# --- NEW: Calculate Percentages for Custom Shapes ---

# Calculate percentages for the new custom shapes for each video
custom_shape_percentages <- df %>%
  group_by(viscosity, condition, id) %>%
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
  left_join(custom_shape_percentages, by = c("viscosity", "condition", "id"))

# Save the final results to a new CSV file
write.csv(df_final, "swimming/droplet/combined_data_with_custom_shape_percentages.csv", row.names = FALSE)

# You can now proceed with any further analysis or plotting using the new 'custom_shape' column
# and the calculated percentages.




######Plots for paper######

# Shape percentages by viscosity
# Reshape data for plotting with the new custom shapes
plot_data <- custom_shape_percentages %>%
  mutate(
    ancestry = case_when(
      condition %in% c("a", "b") ~ "Agar ancestry",
      TRUE ~ "Scaffold ancestry"
    ),
    growing = case_when(
      condition %in% c("a", "d") ~ "Agar growth", 
      TRUE ~ "Scaffold growth"
    )
  ) %>%
  # Pivot the new shape percentage columns
  pivot_longer(
    cols = c(o_shape_percentage, `6_shape_percentage`, u_shape_percentage, 
             c_shape_custom_percentage, s_shape_custom_percentage),
    names_to = "shape_type",
    values_to = "percentage"
  ) %>%
  # Create clean labels for the plot's x-axis
  mutate(
    shape_type = factor(case_when(
      shape_type == "o_shape_percentage" ~ "O",
      shape_type == "6_shape_percentage" ~ "6",
      shape_type == "u_shape_percentage" ~ "U",
      shape_type == "c_shape_custom_percentage" ~ "C",
      shape_type == "s_shape_custom_percentage" ~ "S"
    ), levels = c("O", "6", "U", "C", "S")) # Set factor levels for a logical order
  )

# Calculate sample sizes for verification
sample_sizes <- plot_data %>%
  group_by(condition, viscosity) %>%
  summarise(n = n_distinct(id))

# Optional: Output sample sizes to clipboard or console
# write.table(sample_sizes, "clipboard", sep="\t", row.names=FALSE)
print(sample_sizes)


# Create and save the plot
png("swimming/droplet/plots/custom_shape_percentage_boxplot_by_viscosity.png", 
    width = 2000, height = 1800)

agar_color <- "#66C2A5"
scaffold_color <- "#FC8D62"

p <- ggplot(plot_data, aes(x = shape_type, y = percentage, fill = growing)) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.01) +
  facet_nested(
    viscosity ~ ancestry + growing,
    scales = "fixed",
    labeller = labeller(
      ancestry = c("Agar ancestry" = "Agar ancestry", "Scaffold ancestry" = "Scaffold ancestry"),
      growing = c("Agar growth" = "Agar growth", "Scaffold growth" = "Scaffold growth"),
      viscosity = c(
        "ngm" = "0% MC",
        "visc05" = "0.5% MC", 
        "visc1" = "1% MC"
      )
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
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8), limits = c(0, 0.95), labels = function(x) ifelse(x == 0, "0", sub("^0\\.", ".", sprintf("%.1f", x)))) +
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

ggsave("C:/Users/aurel/Documents/Apples/swimming/droplet/plots/custom_shape_percentage_boxplot_by_viscosity_paper.png", p, dpi = 300, bg = "white", width = 90, height = 70, units = "mm")





dev.off()



#Max bend amplitude by viscosity

# Calculate average amplitudes per video, excluding z-axis turns and normalizing by worm length
avg_amplitudes_per_video <- df_final %>%
  filter(!z_axis_turn) %>%
  mutate(
    max_amplitude_percentage = max_amplitudes / (worm_lengths / 2),
    smoothed_max_amplitude_percentage = smoothed_max_amplitudes / (worm_lengths / 2)
  ) %>%
  group_by(video_id) %>%
  summarise(
    max_amplitudes = mean(max_amplitude_percentage), 
    smoothed_max_amplitudes = mean(smoothed_max_amplitude_percentage),
    condition = first(condition),
    viscosity = first(viscosity)
  )

# Calculate mean values for condition "a" and viscosity "ngm" as baseline
ngm_means <- avg_amplitudes_per_video %>%
  filter(condition == "a", viscosity == "ngm") %>%
  summarise(
    mean_max = mean(max_amplitudes),
    mean_smoothed = mean(smoothed_max_amplitudes)
  )

# Normalize values relative to baseline
avg_amplitudes_per_video <- avg_amplitudes_per_video %>%
  mutate(
    normalized_max_amplitudes = (max_amplitudes - ngm_means$mean_max) / ngm_means$mean_max,
    normalized_smoothed_max_amplitudes = (smoothed_max_amplitudes - ngm_means$mean_smoothed) / ngm_means$mean_smoothed
  )

avg_amplitudes_per_video <- avg_amplitudes_per_video %>%
  mutate(
    ancestry = case_when(
      condition %in% c("a", "b") ~ "agar",
      TRUE ~ "scaffold"
    ),
    growing = case_when(
      condition %in% c("a", "d") ~ "agar",
      TRUE ~ "scaffold"
    )
  )


# Create plot with facets for viscosity
png("swimming/droplet/plots/normalized_avg_max_amplitude_boxplots.png", 
    width = 1200, height = 800)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette


p <- ggplot(avg_amplitudes_per_video, 
       aes(x = viscosity, y = max_amplitudes, fill = growing)) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.01) +
  facet_nested(~ ancestry + growing,
    labeller = labeller(
      ancestry = c(
        "agar" = "Agar ancestry",
        "scaffold" = "Scaffold ancestry"
      ),
      growing = c(
        "agar" = "Agar growth",
        "scaffold" = "Scaffold growth"
      )
    ),
    strip = strip_nested(
      background_x = list(
        element_rect(fill = agar_color, color = "white", linewidth = 0.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 0.5)
      ),
      text_x = list(
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 2, b = 2)),
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 2, b = 2))
      )
    )
  ) +
  scale_fill_manual(values = c("agar" = agar_color, "scaffold" = scaffold_color)) +
  scale_x_discrete(labels = c("ngm" = "0", "visc05" = "0.5", "visc1" = "1")) +
  #scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Methylcellulose concentration (%)",
    y = "Maximum bending amplitude (% of body max.)"
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
    strip.text.y = element_text(margin = margin(l = 1, r = 1)),
    plot.margin = unit(c(0, 0, 0, 0), "mm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    ##panel.grid.major.y = element_line(color = "gray90"),
    ##panel.border = element_rect(color = "black", fill = NA),
    ##panel.grid.minor.y = element_line(color = "gray95")
  ) +
  scale_y_continuous(labels = function(x) sub("^0\\.", ".", sprintf("%.2f", x)))

ggsave("C:/Users/aurel/Documents/Apples/swimming/droplet/plots/normalized_avg_max_amplitude_boxplots_paper.png", p, dpi = 300, bg = "white", width = 85, height = 60, units = "mm")





dev.off()




# Interpolated frequencies >= 1 Hz (Temporal frequency)
# Load interpolated frequencies data
interpolated_freqs_df <- read.csv("swimming/droplet/new_interpolated_freqs_df_all.csv")

# Calculate average interpolated frequencies by video, filtering for values >= 1
avg_freq_by_video <- interpolated_freqs_df %>%
  filter(interpolated_freq >= 1) %>%
  group_by(viscosity, condition, video_id) %>%
  summarise(
    avg_interpolated_freq = mean(interpolated_freq)
  ) %>%
  ungroup() %>%
  mutate(
    ancestry = case_when(
      condition %in% c("a", "b") ~ "Agar ancestry",
      TRUE ~ "Scaffold ancestry"
    ),
    growing = case_when(
      condition %in% c("a", "d") ~ "Agar growth",
      TRUE ~ "Scaffold growth"
    )
  )

# Create plot
png("swimming/droplet/plots/avg_interpolated_freqs_boxplot_1hz.png", 
    width = 1200, height = 800)

ggplot(avg_freq_by_video, 
       aes(x = viscosity, y = avg_interpolated_freq, fill = growing)) +
  geom_boxplot(color = "black") +
  facet_nested(~ ancestry + growing,
    labeller = labeller(
      ancestry = c(
        "Agar ancestry" = "Agar ancestry",
        "Scaffold ancestry" = "Scaffold ancestry"
      ),
      growing = c(
        "Agar growth" = "Agar growth",
        "Scaffold growth" = "Scaffold growth"
      )
    ),
    strip = strip_nested(
      background_x = list(
        element_rect(fill = agar_color, color = "white", linewidth = 1.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 1.5)
      ),
      text_x = list(
        element_text(color = "#000000", size = 24, face = "plain", margin = margin(t = 5, b = 8)),
        element_text(color = "#000000", size = 24, face = "plain", margin = margin(t = 5, b = 5))
      )
    )
  ) +
  scale_fill_manual(values = c("Agar growth" = agar_color, "Scaffold growth" = scaffold_color)) +
  scale_x_discrete(labels = c("ngm" = "0", "visc05" = "0.5", "visc1" = "1")) +
  scale_y_continuous(breaks = seq(0, max(avg_freq_by_video$avg_interpolated_freq), by = 0.2)) +
  labs(
    x = "Methylcellulose concentration (%)",
    y = "Average swimming oscillation rate (bends/s)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22, vjust = 0.5),
    axis.title = element_text(size = 24, face = "plain", margin = margin(t = 22, b = 20)),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(1.5, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(size = 24, face = "plain"),
    strip.text.x = element_text(margin = margin(t = 5, b = 5)),
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    #panel.border = element_rect(color = "black", fill = NA),
    panel.grid.minor.y = element_line(color = "gray95")
  )

dev.off()





#Wavelength by viscosity

# Calculate average values per video_id for normalized wavelengths (S-shape)
avg_normalized_df <- df_final %>%
  mutate(video_id = paste(viscosity, condition, id, sep = "_")) %>%
  filter(grepl('S-shape', shape, fixed = TRUE)) %>%
  group_by(video_id, condition, viscosity) %>%
  summarise(
    normalized_wavelengths = mean(normalized_wavelengths),
    .groups = 'drop'
  )

# Add ancestry and growing columns
avg_normalized_df <- avg_normalized_df %>%
  mutate(
    ancestry = case_when(
      condition %in% c("a", "b") ~ "agar",
      TRUE ~ "scaffold"
    ),
    growing = case_when(
      condition %in% c("a", "d") ~ "agar",
      TRUE ~ "scaffold"
    )
  )

# Check the structure and content of avg_normalized_df
print(str(avg_normalized_df))
print(unique(avg_normalized_df$ancestry))
print(unique(avg_normalized_df$growing))

# Create plot
png("swimming/droplet/plots/normalized_wavelengths_boxplot_by_viscosity.png", 
    width = 1200, height = 800)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

p <- ggplot(avg_normalized_df, aes(x = viscosity, y = normalized_wavelengths, fill = growing)) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.01) +
  facet_nested(~ ancestry + growing,
    labeller = labeller(
      ancestry = c("agar" = "Agar ancestry", "scaffold" = "Scaffold ancestry"),
      growing = c("agar" = "Agar growth", "scaffold" = "Scaffold growth")
    ),
    strip = strip_nested(
      background_x = list(
        element_rect(fill = agar_color, color = "white", linewidth = 0.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 0.5)
      ),
      text_x = list(
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 2, b = 2)),
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 2, b = 2))
      )
    )
  ) +
  scale_fill_manual(values = c("agar" = agar_color, "scaffold" = scaffold_color)) +
  scale_x_discrete(labels = c("ngm" = "0", "visc05" = "0.5", "visc1" = "1")) +
  labs(
    x = "Methylcellulose concentration (%)",
    y = "Wavelength during S-shape"
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
    strip.text.y = element_text(margin = margin(l = 1, r = 1)),
    plot.margin = unit(c(0, 0, 0, 0), "mm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    ##panel.grid.major.y = element_line(color = "gray90"),
    ##panel.border = element_rect(color = "black", fill = NA),
    ##panel.grid.minor.y = element_line(color = "gray95")
  )

ggsave("C:/Users/aurel/Documents/Apples/swimming/droplet/plots/normalized_wavelengths_boxplot_by_viscosity_paper.png", p, dpi = 300, bg = "white", width = 85, height = 60, units = "mm")




dev.off()

# Check for 'S-shape' entries in df_final
print(table(df_final$shape))

# If there are 'S-shape' entries, let's see what's happening in the data processing
if ('S-shape' %in% df_final$shape) {
  print("S-shape entries found. Debugging data processing...")
  
  # Check intermediate steps
  step1 <- df_final %>% filter(shape == 'S-shape')
  print(str(step1))
  
  step2 <- step1 %>% 
    group_by(video_id, condition, viscosity) %>%
    summarise(
      normalized_wavelengths = mean(normalized_wavelengths),
      .groups = 'drop'
    )
  print(str(step2))
  
  # If step2 is not empty, the issue is in the mutate step
  if (nrow(step2) > 0) {
    step3 <- step2 %>%
      mutate(
        ancestry = case_when(
          condition %in% c("a", "b") ~ "agar",
          TRUE ~ "scaffold"
        ),
        growing = case_when(
          condition %in% c("a", "d") ~ "agar",
          TRUE ~ "scaffold"
        )
      )
    print(str(step3))
  }
} else {
  print("No 'S-shape' entries found in df_final. Check your shape categories.")
}

# If there are no 'S-shape' entries, we might need to adjust our filtering criteria
# For example, if the shape is stored as 'b'S-shape'' instead of 'S-shape':
if (!'S-shape' %in% df_final$shape) {
  print("Attempting with modified shape filter...")
  avg_normalized_df <- df_final %>%
    filter(grepl('S-shape', shape, fixed = TRUE)) %>%
    group_by(video_id, condition, viscosity) %>%
    summarise(
      normalized_wavelengths = mean(normalized_wavelengths),
      .groups = 'drop'
    ) %>%
    mutate(
      ancestry = case_when(
        condition %in% c("a", "b") ~ "agar",
        TRUE ~ "scaffold"
      ),
      growing = case_when(
        condition %in% c("a", "d") ~ "agar",
        TRUE ~ "scaffold"
      )
    )
  print(str(avg_normalized_df))
}

# If avg_normalized_df is still empty, we need to investigate df_final further
if (nrow(avg_normalized_df) == 0) {
  print("avg_normalized_df is still empty. Checking df_final...")
  print(str(df_final))
  print(head(df_final))
}


