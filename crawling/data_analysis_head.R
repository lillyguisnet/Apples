# Load necessary libraries
library(tidyverse)

# Read the data
file_path <- "crawling/data/allmerged_merged_head_bend_analysis.csv"
bend_data <- read_csv(file_path)

# Parse the video_id column
bend_data_parsed <- bend_data %>%
  extract(video_id, into = c("environment", "worm_condition", "uid"), 
          regex = "([a-zA-Z]+)-([a-zA-Z])-([0-9]+-[0-9]+)", remove = FALSE)

# Convert environment and worm_condition to factors
bend_data_parsed$environment <- as.factor(bend_data_parsed$environment)
bend_data_parsed$worm_condition <- as.factor(bend_data_parsed$worm_condition)

# Glimpse the data
glimpse(bend_data_parsed)

# Plot 1: Average bend frequency by environment and worm condition
plot1 <- ggplot(bend_data_parsed, aes(x = environment, y = avg_bend_frequency, fill = worm_condition)) +
  geom_boxplot() +
  facet_wrap(~worm_condition) +
  labs(title = "Average Bend Frequency by Environment and Worm Condition",
       x = "Environment",
       y = "Average Bend Frequency (Hz)") 

# Save plot 1
ggsave("crawling/plotshead/bend_frequency.png", plot1)

# Calculate corrected bend amplitude and other metrics
bend_data_parsed <- bend_data_parsed %>%
  mutate(
    bend_amplitude = (abs(avg_peak_depth) + abs(avg_trough_depth)) / 2,
    max_bend_amplitude = (abs(max_peak_depth) + abs(max_trough_depth)) / 2,
    total_head_swings = num_peaks + num_troughs
  )

# Plot 2: Corrected Bend amplitude by environment and worm condition
plot2 <- ggplot(bend_data_parsed, aes(x = environment, y = bend_amplitude, fill = worm_condition)) +
  geom_boxplot() +
  facet_wrap(~worm_condition) +
  labs(title = "Average Bend Amplitude by Environment and Worm Condition",
       x = "Environment",
       y = "Average Bend Amplitude (degrees)")

# Save plot 2
ggsave("crawling/plotshead/bend_amplitude_corrected.png", plot2)

# Plot 3: Scatter plot of bend frequency vs corrected amplitude
plot3 <- ggplot(bend_data_parsed, aes(x = avg_bend_frequency, y = bend_amplitude, color = environment)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~worm_condition) +
  labs(title = "Bend Frequency vs. Corrected Amplitude",
       x = "Average Bend Frequency (Hz)",
       y = "Bend Amplitude (degrees)")

# Save plot 3
ggsave("crawling/plotshead/freq_vs_amplitude_corrected.png", plot3)

# Plot 4: Total head swings
plot4 <- ggplot(bend_data_parsed, aes(x = environment, y = total_head_swings, fill = worm_condition)) +
    geom_boxplot() +
    facet_wrap(~worm_condition) +
    labs(title = "Total Head Swings by Environment and Worm Condition",
         x = "Environment",
         y = "Total Head Swings")

# Save plot 4
ggsave("crawling/plotshead/total_head_swings.png", plot4)

# Plot 5: Dominant frequency
plot5 <- ggplot(bend_data_parsed, aes(x = environment, y = dominant_freq, fill = worm_condition)) +
    geom_boxplot() +
    facet_wrap(~worm_condition) +
    labs(title = "Dominant Frequency by Environment and Worm Condition",
         x = "Environment",
         y = "Dominant Frequency (Hz)")

# Save plot 5
ggsave("crawling/plotshead/dominant_frequency.png", plot5)

# Plot 6: Maximum bend amplitude
plot6 <- ggplot(bend_data_parsed, aes(x = environment, y = max_bend_amplitude, fill = worm_condition)) +
    geom_boxplot() +
    facet_wrap(~worm_condition) +
    labs(title = "Maximum Bend Amplitude by Environment and Worm Condition",
         x = "Environment",
         y = "Maximum Bend Amplitude (degrees)")

# Save plot 6
ggsave("crawling/plotshead/max_bend_amplitude.png", plot6)


# Plot 7: Average bend frequency vs Dominant Frequency
plot7 <- ggplot(bend_data_parsed, aes(x = avg_bend_frequency, y = dominant_freq, color = environment)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~worm_condition) +
  labs(title = "Average Bend Frequency vs. Dominant Frequency",
       x = "Average Bend Frequency (Hz)",
       y = "Dominant Frequency (Hz)") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")


# Save plot 7
ggsave("crawling/plotshead/avg_vs_dominant_freq.png", plot7)


print("Analysis complete. Plots saved in crawling/plotshead/")
