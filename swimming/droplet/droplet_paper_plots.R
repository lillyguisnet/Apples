library(ggplot2)
library(dplyr)
library(openxlsx)
library(ggh4x)

xl_file = "swimming/droplet/combined_data_all.csv"
df <- as.data.frame(read.csv(xl_file))


detect_z_axis_turns <- function(df) {
  # Group by unique video ID
  grouped <- df %>% group_by(viscosity, condition, id)
  
  # Function to detect z-axis turns for each group
  detect_turns <- function(group) {
    mean_length <- mean(group$worm_lengths)
    std_length <- sd(group$worm_lengths)
    threshold <- mean_length - std_length
    tibble(
      z_axis_turn = group$worm_lengths < threshold,
      video_id = paste(group$viscosity[1], group$condition[1], group$id[1], sep = "_")
    )
  }
  
  # Apply the function to each group
  result <- grouped %>% 
    do(detect_turns(.)) %>% 
    ungroup()
  
  # Add the results as new columns
  df <- df %>%
    mutate(
      z_axis_turn = result$z_axis_turn,
      video_id = result$video_id
    )
  
  return(df)
}

# Apply the function to the dataframe
df_z <- detect_z_axis_turns(df)

# Count unique occurrences of z_axis_turn
z_turn_counts <- df_z %>%
  group_by(z_axis_turn) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(z_axis_turn == TRUE) %>%
  pull(percentage) #13.63618


  

# Calculate shape percentages for each video
shape_percentages <- df_z %>%
  group_by(viscosity, condition, id) %>%
  summarise(
    total_frames = n(),
    z_axis_turns = sum(z_axis_turn),
    non_z_axis_frames = total_frames - z_axis_turns,
    c_shapes = sum(shape == "b'C-shape'" & !z_axis_turn),
    s_shapes = sum(shape == "b'S-shape'" & !z_axis_turn), 
    straight = sum(shape == "b'Straight'" & !z_axis_turn),
    z_axis_turn_percentage = (z_axis_turns / total_frames) * 100,
    c_shape_percentage = (c_shapes / total_frames) * 100,
    s_shape_percentage = (s_shapes / total_frames) * 100,
    straight_percentage = (straight / total_frames) * 100,
    c_shape_percentage_excluding_z = ifelse(non_z_axis_frames > 0, 
                                          (c_shapes / non_z_axis_frames) * 100, 0),
    s_shape_percentage_excluding_z = ifelse(non_z_axis_frames > 0,
                                          (s_shapes / non_z_axis_frames) * 100, 0),
    straight_percentage_excluding_z = ifelse(non_z_axis_frames > 0,
                                           (straight / non_z_axis_frames) * 100, 0)
  )

# Join percentages back to original dataframe
df_final <- df_z %>%
  left_join(shape_percentages, by = c("viscosity", "condition", "id"))

# Save results
write.csv(df_final, "swimming/droplet/combined_data_with_shape_percentages.csv", row.names = FALSE)




######Plots for paper######

#Shape percentages by viscosity
# Create plot for shape percentages by viscosity
df_final <- df_final %>%
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
# Order viscosities with visc05 at end
viscosity_order <- c(sort(unique(shape_percentages$viscosity)))

# Reshape data for plotting
library(tidyr)  # For pivot_longer

plot_data <- shape_percentages %>%
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
  pivot_longer(
    cols = c(straight_percentage_excluding_z, s_shape_percentage_excluding_z,
             c_shape_percentage_excluding_z, z_axis_turn_percentage),
    names_to = "shape_type",
    values_to = "percentage"
  ) %>%
  mutate(
    shape_type = case_when(
      shape_type == "straight_percentage_excluding_z" ~ "Straight",
      shape_type == "s_shape_percentage_excluding_z" ~ "S-shape",
      shape_type == "c_shape_percentage_excluding_z" ~ "C-shape",
      shape_type == "z_axis_turn_percentage" ~ "Z-axis Turn"
    )
  )


sample_sizes <- plot_data %>%
  group_by(condition, viscosity) %>%
  summarise(n = n_distinct(id))

write.table(sample_sizes, "clipboard", sep="\t", row.names=FALSE)
print(sample_sizes)


# Create plot
png("swimming/droplet/plots/shape_percentage_boxplot_by_viscosity_excluding_z.png", 
    width = 2000, height = 1800)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

ggplot(plot_data, aes(x = shape_type, y = percentage, fill = growing)) +
  geom_boxplot(color = "black") +
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
        element_rect(fill = agar_color, color = "white", linewidth = 1.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 1.5)
      ),
      background_y = element_rect(fill = "gray85", color = "white", linewidth = 1.5),
      text_x = list(
        element_text(color = "#000000", size = 24, face = "plain", margin = margin(t = 5, b = 8)),
        element_text(color = "#000000", size = 24, face = "plain", margin = margin(t = 5, b = 5))
      )
    )
  ) +
  scale_fill_manual(values = c("Agar growth" = agar_color, "Scaffold growth" = scaffold_color)) +
  labs(
    x = "Shape type",
    y = "Percentage of frames"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22, angle = 45, hjust = 1),
    axis.title = element_text(size = 28, face = "plain", margin = margin(t = 22, b = 20)),
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
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid.minor.y = element_line(color = "gray95")
  )

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

ggplot(avg_amplitudes_per_video, 
       aes(x = viscosity, y = max_amplitudes, fill = growing)) +
  geom_boxplot(color = "black") +
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
        element_rect(fill = agar_color, color = "white", linewidth = 1.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 1.5)
      ),
      text_x = list(
        element_text(color = "#000000", size = 24, face = "plain", margin = margin(t = 5, b = 8)),
        element_text(color = "#000000", size = 24, face = "plain", margin = margin(t = 5, b = 5))
      )
    )
  ) +
  scale_fill_manual(values = c("agar" = agar_color, "scaffold" = scaffold_color)) +
  scale_x_discrete(labels = c("ngm" = "0", "visc05" = "0.5", "visc1" = "1")) +
  #scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Methylcellulose concentration (%)",
    y = "Average maximum bending amplitude in proportion to body length"
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

ggplot(avg_normalized_df, aes(x = viscosity, y = normalized_wavelengths, fill = growing)) +
  geom_boxplot(color = "black") +
  facet_nested(~ ancestry + growing,
    labeller = labeller(
      ancestry = c("agar" = "Agar ancestry", "scaffold" = "Scaffold ancestry"),
      growing = c("agar" = "Agar growth", "scaffold" = "Scaffold growth")
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
  scale_fill_manual(values = c("agar" = agar_color, "scaffold" = scaffold_color)) +
  scale_x_discrete(labels = c("ngm" = "0", "visc05" = "0.5", "visc1" = "1")) +
  labs(
    x = "Methylcellulose concentration (%)",
    y = "Number of wavelengths per body length during S-shape"
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


