# Load necessary libraries
library(tidyverse)
library(patchwork)

# Create the output directory if it doesn't exist
output_dir <- "crawling/plots_timeseries"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Read the data
file_path <- "crawling/data/allmerged_merged_timeseries_data.csv"
timeseries_data <- read_csv(file_path)

# Parse the video_id column
timeseries_data_parsed <- timeseries_data %>%
  extract(video_id, into = c("environment", "worm_condition", "uid"), 
          regex = "([a-zA-Z]+)-([a-zA-Z])-([0-9]+-[0-9]+)", remove = FALSE)

# Glimpse the data
glimpse(timeseries_data_parsed)

# Get unique combinations of environment and worm_condition
groups <- timeseries_data_parsed %>%
  distinct(environment, worm_condition)

# Loop through each group, create a plot, and save it
for (i in 1:nrow(groups)) {
  current_env <- groups$environment[i]
  current_worm <- groups$worm_condition[i]
  
  # Filter data for the current group
  group_data <- timeseries_data_parsed %>%
    filter(environment == current_env, worm_condition == current_worm)
  
  # Get a list of unique video IDs in the current group
  uids_in_group <- unique(group_data$uid)
  
  # Create a list to hold the plot for each replicate
  replicate_plot_list <- list()
  
  # Loop through each replicate to create a combined amp/curv plot
  for (current_uid in uids_in_group) {
    replicate_data <- group_data %>%
      filter(uid == current_uid)
    
    # Amplitude Plot
    plot_amp <- ggplot(replicate_data, aes(x = frames, y = 1)) +
      geom_tile(aes(fill = max_amplitudes)) +
      scale_fill_viridis_c(option = "viridis") +
      ylab(current_uid) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5)
      ) +
      labs(fill = "Amplitude")
      
    # Curvature Plot
    plot_curv <- ggplot(replicate_data, aes(x = frames, y = 1)) +
      geom_tile(aes(fill = curvature_time_series)) +
      scale_fill_viridis_c(option = "cividis") +
      ylab("") +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()
      ) +
      labs(x = "Frame Number", fill = "Curvature")
      
    # Combine the two plots for the current replicate
    replicate_plot_list[[current_uid]] <- plot_amp / plot_curv
  }
  
  # Combine all replicate plots into a single column
  final_plot <- wrap_plots(replicate_plot_list, ncol = 1) +
    plot_annotation(title = paste("Amplitude and Curvature Kymographs for", current_env, "-", current_worm))
  
  # Define the output filename
  filename <- paste0(output_dir, "/kymograph_paired_", current_env, "_", current_worm, ".png")
  
  # Save the plot
  ggsave(filename, final_plot, width = 14, height = 2 * n_distinct(group_data$uid), limitsize = FALSE)
  
  print(paste("Saved plot:", filename))
}

print("Timeseries analysis complete.")
