# Load necessary libraries
library(tidyverse)
library(ggh4x)

# Read the data
file_path <- "crawling/data/allmerged_merged_path_analysis.csv"
path_data <- read_csv(file_path)

# Parse the video_id column
path_data_parsed <- path_data %>%
  extract(video_id, into = c("environment", "worm_condition", "uid"), 
          regex = "([a-zA-Z]+)-([a-zA-Z])-([0-9]+-[0-9]+)", remove = FALSE)

# Convert environment and worm_condition to factors
path_data_parsed$environment <- as.factor(path_data_parsed$environment)
path_data_parsed$worm_condition <- as.factor(path_data_parsed$worm_condition)

# Calculate percentage of time in each state
path_data_parsed <- path_data_parsed %>%
  mutate(
    total_frames = forward_frames + backward_frames + stationary_frames,
    pct_forward = (forward_frames / total_frames) * 100,
    pct_backward = (backward_frames / total_frames) * 100,
    pct_stationary = (stationary_frames / total_frames) * 100
  )

# Glimpse the data
glimpse(path_data_parsed)

# --- Plotting ---

# Plot 1: Time spent in each state (as a percentage)
plot1_data <- path_data_parsed %>%
  select(environment, worm_condition, pct_forward, pct_backward, pct_stationary) %>%
  pivot_longer(cols = c(pct_forward, pct_backward, pct_stationary), 
               names_to = "state", values_to = "percentage")

plot1 <- ggplot(plot1_data, aes(x = environment, y = percentage, fill = state)) +
  geom_boxplot() +
  facet_grid(worm_condition ~ state) +
  labs(title = "Time Spent in Different Movement States",
       x = "Environment",
       y = "Percentage of Time (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("crawling/plotspath/path_movement_states.png", plot1, width = 10, height = 8)


# Plot 2: Average speed
plot2 <- ggplot(path_data_parsed, aes(x = environment, y = avg_speed, fill = worm_condition)) +
  geom_boxplot() +
  facet_wrap(~worm_condition) +
  labs(title = "Average Speed by Environment and Worm Condition",
       x = "Environment",
       y = "Average Speed (pixels/frame)")

ggsave("crawling/plotspath/path_avg_speed.png", plot2)


# Plot 3: Sinuosity
plot3 <- ggplot(path_data_parsed, aes(x = environment, y = sinuosity, fill = worm_condition)) +
  geom_boxplot() +
  facet_wrap(~worm_condition) +
  labs(title = "Path Sinuosity by Environment and Worm Condition",
       x = "Environment",
       y = "Sinuosity")

ggsave("crawling/plotspath/path_sinuosity.png", plot3)

# Plot 4: Total Distance
plot4 <- ggplot(path_data_parsed, aes(x = environment, y = total_distance, fill = worm_condition)) +
  geom_boxplot() +
  facet_wrap(~worm_condition) +
  labs(title = "Total Distance Traveled by Environment and Worm Condition",
       x = "Environment",
       y = "Total Distance (pixels)")

ggsave("crawling/plotspath/path_total_distance.png", plot4)


# Plot 5: Number of forward vs backward bouts
plot5 <- ggplot(path_data_parsed, aes(x = forward_bouts, y = backward_bouts, color = environment)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~worm_condition) +
  labs(title = "Number of Forward vs. Backward Bouts",
       x = "Number of Forward Bouts",
       y = "Number of Backward Bouts") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")


ggsave("crawling/plotspath/path_bouts_scatter.png", plot5)

