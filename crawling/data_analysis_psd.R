# Load required libraries
library(tidyverse)

# Path to the PSD data
psd_path <- "crawling/data/allmerged_merged_f_psd_data.csv"

# Read the data -------------------------------------------------------------
psd_df <- read_csv(psd_path, show_col_types = FALSE)

# Parse the composite `video_id` into useful categorical variables ----------
psd_df <- psd_df %>%
  mutate(
    env  = str_extract(video_id, "^[^-]+"),                       # "food", "nofood", "thick"
    worm = str_split_fixed(video_id, "-", 3)[,2],                 # "a", "b", "c", "d"
    uid  = str_split_fixed(video_id, "-", 3)[,3]                  # unique recording id
  )

# ---------------------------------------------------------------------------
# QUICK EXPLORATORY METRICS PER VIDEO
# ---------------------------------------------------------------------------
video_metrics <- psd_df %>%
  arrange(video_id, f) %>%
  group_by(video_id, env, worm) %>%
  summarise(
    total_power = sum(psd, na.rm = TRUE),           # simple integral approximation
    peak_freq   = f[which.max(psd)],                # frequency with maximum PSD
    peak_psd    = max(psd, na.rm = TRUE),
    .groups     = "drop"
  )

# ---------------------------------------------------------------------------
# CREATE OUTPUT DIRECTORY FOR PLOTS
# ---------------------------------------------------------------------------
plot_dir <- "crawling/plots"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

# ---------------------------------------------------------------------------
# 1) Average PSD curve by environmental condition ---------------------------
# ---------------------------------------------------------------------------
env_avg <- psd_df %>%
  group_by(env, f, worm) %>%
  summarise(
    mean_psd = mean(psd, na.rm = TRUE),
    sd_psd   = sd(psd,   na.rm = TRUE),
    .groups  = "drop"
  )

g_env <- ggplot(env_avg, aes(x = f, y = mean_psd, colour = env, fill = env)) +
  geom_line(size = 1) +
  facet_wrap(~worm) +
  geom_ribbon(aes(ymin = mean_psd - sd_psd, ymax = mean_psd + sd_psd),
              alpha = 0.2, colour = NA) +
  labs(title = "Average Power Spectrum Density by Environment",
       x = "Frequency (Hz)", y = "Mean PSD") +
  theme_bw()

ggsave(file.path(plot_dir, "avg_psd_env.png"), g_env, width = 7, height = 4, dpi = 300)

# ---------------------------------------------------------------------------
# 2) Total power distribution across env & worm -----------------------------
# ---------------------------------------------------------------------------
g_tp <- ggplot(video_metrics, aes(x = interaction(env, worm, sep = "-"),
                                  y = total_power, fill = env)) +
  geom_boxplot(outlier.alpha = 0.4) +
  labs(title = "Total Power per Video", x = "Condition (env-worm)", y = "Total Power (a.u.)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(plot_dir, "total_power_boxplot.png"), g_tp, width = 7, height = 4, dpi = 300)

# ---------------------------------------------------------------------------
# 3) Peak frequency distribution -------------------------------------------
# ---------------------------------------------------------------------------
g_pf <- ggplot(video_metrics, aes(x = env, y = peak_freq, fill = worm)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1, outlier.shape = NA, position = position_dodge(width = 0.9)) +
  labs(title = "Peak Frequency Distribution", x = "Environment", y = "Peak Frequency (Hz)") +
  theme_bw()

ggsave(file.path(plot_dir, "peak_freq_violin.png"), g_pf, width = 7, height = 4, dpi = 300)

# ---------------------------------------------------------------------------
# 4) Save the metrics table for further analysis ----------------------------
# ---------------------------------------------------------------------------
write_csv(video_metrics, file.path(plot_dir, "video_metrics_summary.csv"))

# Message to user (printed when script is run) ------------------------------
cat("Analysis complete! Plots saved to:", plot_dir, "\n")
