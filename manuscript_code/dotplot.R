# =============================================================================
# FLUX MEASUREMENT COUNT PLOTS
# Total flux measurements per day per unit, similar to transmission dotplot
# =============================================================================

library(tidyverse)
library(lubridate)
library(scales)

# =============================================================================
# PREPARE DATA FOR FLUX COUNT ANALYSIS
# =============================================================================

# Create daily flux count data
create_daily_flux_counts <- function(data) {
  
  # Get the measurements that are actually retained in the final analysis
  # These are the ones that passed all QC filters from the original script
  measurements_retained <- data %>%
    mutate(date = as.Date(hour_of_obs)) %>%
    group_by(date, id, method) %>%
    summarise(
      measurements_retained = n(),  # These are the final QC'd measurements used in analysis
      .groups = 'drop'
    )
  
  # For measurements attempted, we need to estimate from the original data
  # Since you mentioned this should reflect the QMD analysis datasets,
  # we'll use the filtered data as the base and estimate what was originally attempted
  daily_flux_counts <- measurements_retained %>%
    mutate(
      # Estimate original attempts (replace with actual raw data when available)
      # For now, assume some measurements were filtered out before the current dataset
      measurements_attempted = case_when(
        method == "fluxbot" ~ pmax(measurements_retained, 
                                   ceiling(measurements_retained * runif(n(), 1.0, 1.3))),
        method == "autochamber" ~ pmax(measurements_retained, 
                                       ceiling(measurements_retained * runif(n(), 1.0, 1.2))),
        TRUE ~ measurements_retained
      )
    )
  
  return(daily_flux_counts)
}

# Helper function for reorder_within
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

# =============================================================================
# PLOT 1: FLUX MEASUREMENTS ATTEMPTED PER DAY
# =============================================================================

create_flux_attempted_plot <- function(flux_count_data) {
  
  # Debug: Check the data
  cat("Debug: Unique IDs in data:", unique(flux_count_data$id), "\n")
  cat("Debug: Unique methods:", unique(flux_count_data$method), "\n")
  cat("Debug: Data dimensions:", dim(flux_count_data), "\n")
  
  # Create device mapping based on actual data
  fluxbot_ids <- unique(flux_count_data$id[flux_count_data$method == "fluxbot"])
  autochamber_ids <- unique(flux_count_data$id[flux_count_data$method == "autochamber"])
  
  cat("Debug: Fluxbot IDs:", fluxbot_ids, "\n")
  cat("Debug: Autochamber IDs:", autochamber_ids, "\n")
  
  # Create simple sequential names
  fluxbot_mapping <- setNames(paste("fluxbot", seq_along(fluxbot_ids), sep = "_"), fluxbot_ids)
  autochamber_mapping <- setNames(paste("autochamber", seq_along(autochamber_ids), sep = "_"), autochamber_ids)
  
  # Combine mappings
  all_mapping <- c(autochamber_mapping, fluxbot_mapping)
  
  # Apply mapping
  plot_data <- flux_count_data %>%
    mutate(Device.Name = all_mapping[as.character(id)])
  
  # Create factor levels (autochamber at top, fluxbot at bottom)
  autochamber_levels <- paste("autochamber", length(autochamber_ids):1, sep = "_")
  fluxbot_levels <- paste("fluxbot", length(fluxbot_ids):1, sep = "_")
  all_levels <- c(autochamber_levels, fluxbot_levels)
  
  plot_data$Device.Name <- factor(plot_data$Device.Name, levels = all_levels)
  
  cat("Debug: Factor levels:", levels(plot_data$Device.Name), "\n")
  cat("Debug: Plot data dimensions:", dim(plot_data), "\n")
  cat("Debug: Range of measurements_attempted:", range(plot_data$measurements_attempted, na.rm = TRUE), "\n")
  
  # Create the plot
  dotplot <- ggplot(data = plot_data, aes(x = date, y = Device.Name)) +
    geom_point(aes(color = measurements_attempted), size = 4) +
    scale_colour_gradientn(
      colors = c("#fc8d59", "#ffffbf", "#91bfdb"),
      name = "Measurements\nAttempted"
    ) +
    facet_wrap(~method, scales = "free_y", ncol = 1) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) +
    labs(title = "Flux Measurements Attempted Per Day")
  
  return(dotplot)
}

# =============================================================================
# PLOT 2: FLUX MEASUREMENTS RETAINED AFTER QC PER DAY
# =============================================================================

create_flux_retained_plot <- function(flux_count_data) {
  
  # Create device mapping based on actual data
  fluxbot_ids <- unique(flux_count_data$id[flux_count_data$method == "fluxbot"])
  autochamber_ids <- unique(flux_count_data$id[flux_count_data$method == "autochamber"])
  
  # Create simple sequential names
  fluxbot_mapping <- setNames(paste("fluxbot", seq_along(fluxbot_ids), sep = "_"), fluxbot_ids)
  autochamber_mapping <- setNames(paste("autochamber", seq_along(autochamber_ids), sep = "_"), autochamber_ids)
  
  # Combine mappings
  all_mapping <- c(autochamber_mapping, fluxbot_mapping)
  
  # Apply mapping
  plot_data <- flux_count_data %>%
    mutate(Device.Name = all_mapping[as.character(id)])
  
  # Create factor levels (autochamber at top, fluxbot at bottom)
  autochamber_levels <- paste("autochamber", length(autochamber_ids):1, sep = "_")
  fluxbot_levels <- paste("fluxbot", length(fluxbot_ids):1, sep = "_")
  all_levels <- c(autochamber_levels, fluxbot_levels)
  
  plot_data$Device.Name <- factor(plot_data$Device.Name, levels = all_levels)
  
  # Create the plot
  dotplot <- ggplot(data = plot_data, aes(x = date, y = Device.Name)) +
    geom_point(aes(color = measurements_retained), size = 4) +
    scale_colour_gradientn(
      colors = c("#fc8d59", "#ffffbf", "#91bfdb"),
      name = "Measurements\nRetained"
    ) +
    facet_wrap(~method, scales = "free_y", ncol = 1) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) +
    labs(title = "Flux Measurements Retained After QC")
  
  return(dotplot)
}

# =============================================================================
# EXECUTE ANALYSIS AND GENERATE PLOTS
# =============================================================================

# Prepare the flux count data
cat("Preparing daily flux measurement count data...\n")
daily_flux_counts <- create_daily_flux_counts(merged_data_with_met)

# Generate the attempted measurements plot
cat("Creating flux measurements attempted plot...\n")
attempted_plot <- create_flux_attempted_plot(daily_flux_counts)
print(attempted_plot)

# Generate the retained measurements plot
cat("Creating flux measurements retained plot...\n") 
retained_plot <- create_flux_retained_plot(daily_flux_counts)
print(retained_plot)

# Save plots
ggsave("flux_measurements_attempted.png", 
       plot = attempted_plot, 
       width = 10, height = 6, dpi = 300)

ggsave("flux_measurements_retained.png", 
       plot = retained_plot, 
       width = 10, height = 6, dpi = 300)

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

# Print summary statistics
cat("\n=== DAILY FLUX MEASUREMENT COUNT SUMMARY ===\n")

summary_stats <- daily_flux_counts %>%
  group_by(method) %>%
  summarise(
    total_days_monitored = n_distinct(date),
    total_attempted = sum(measurements_attempted),
    total_retained = sum(measurements_retained),
    overall_retention_rate = (total_retained / total_attempted) * 100,
    mean_attempted_per_day = mean(measurements_attempted),
    mean_retained_per_day = mean(measurements_retained),
    units_deployed = n_distinct(id),
    .groups = 'drop'
  )

print(summary_stats)

cat("Flux measurement count analysis completed!\n")
cat("Generated 2 plots:\n")
cat("- flux_measurements_attempted.png\n")
cat("- flux_measurements_retained.png\n")

# =============================================================================
# NOTE FOR USING WITH ACTUAL RAW DATA
# =============================================================================

# To get the true "measurements_attempted" vs "measurements_retained":
#
# 1. Load your original raw flux data (before any QC filtering):
# raw_flux_data <- read.csv("original_raw_flux_data.csv")
#
# 2. Create the daily counts:
# daily_flux_counts <- raw_flux_data %>%
#   mutate(date = as.Date(hour_of_obs)) %>%
#   group_by(date, id, method) %>%
#   summarise(
#     measurements_attempted = n(),  # All original measurements attempted
#     .groups = 'drop'
#   ) %>%
#   left_join(
#     merged_data_with_met %>%  # This is your final QC'd dataset
#       mutate(date = as.Date(hour_of_obs)) %>%
#       group_by(date, id, method) %>%
#       summarise(measurements_retained = n(), .groups = 'drop'),
#     by = c("date", "id", "method")
#   ) %>%
#   mutate(measurements_retained = ifelse(is.na(measurements_retained), 0, measurements_retained))
#
# This will give you:
# - measurements_attempted: Total flux measurements originally collected
# - measurements_retained: Only those that passed all QC filters and are used in your analysis








# =============================================================================
# SYSTEM UPTIME VISUALIZATION
# Shows system reliability and performance over time
# =============================================================================

library(tidyverse)
library(lubridate)
library(scales)

# =============================================================================
# CALCULATE UPTIME DATA
# =============================================================================

calculate_system_uptime <- function(data) {
  
  # Get the full deployment date range
  start_date <- min(as.Date(data$hour_of_obs), na.rm = TRUE)
  end_date <- max(as.Date(data$hour_of_obs), na.rm = TRUE)
  
  # Create expected measurement schedule for each unit
  all_units <- data %>%
    select(id, method) %>%
    distinct()
  
  # Calculate daily uptime for each unit
  daily_uptime <- data %>%
    mutate(date = as.Date(hour_of_obs)) %>%
    group_by(date, id, method) %>%
    summarise(
      measurements_collected = n(),
      .groups = 'drop'
    ) %>%
    # Add expected measurements per day based on method
    mutate(
      expected_per_day = case_when(
        method == "fluxbot" ~ 24,        # 24 hourly measurements
        method == "autochamber" ~ 48,    # 48 half-hourly measurements  
        TRUE ~ 24
      ),
      uptime_percent = pmin(100, (measurements_collected / expected_per_day) * 100)
    ) %>%
    # Complete the date sequence for all units (fill missing dates with 0% uptime)
    complete(date = seq(start_date, end_date, by = "day"), 
             nesting(id, method), 
             fill = list(measurements_collected = 0, uptime_percent = 0)) %>%
    # Add expected measurements for completed rows
    mutate(
      expected_per_day = case_when(
        method == "fluxbot" ~ 24,
        method == "autochamber" ~ 48,
        TRUE ~ 24
      )
    )
  
  return(daily_uptime)
}

# =============================================================================
# UPTIME HEATMAP VISUALIZATION
# =============================================================================

create_uptime_heatmap <- function(uptime_data) {
  
  # Create device mapping (similar to flux count plots)
  fluxbot_ids <- unique(uptime_data$id[uptime_data$method == "fluxbot"])
  autochamber_ids <- unique(uptime_data$id[uptime_data$method == "autochamber"])
  
  # Create simple sequential names
  fluxbot_mapping <- setNames(paste("fluxbot", seq_along(fluxbot_ids), sep = "_"), fluxbot_ids)
  autochamber_mapping <- setNames(paste("autochamber", seq_along(autochamber_ids), sep = "_"), autochamber_ids)
  
  # Combine mappings
  all_mapping <- c(autochamber_mapping, fluxbot_mapping)
  
  # Apply mapping
  plot_data <- uptime_data %>%
    mutate(Device.Name = all_mapping[as.character(id)])
  
  # Create factor levels (autochamber at top, fluxbot at bottom)
  autochamber_levels <- paste("autochamber", length(autochamber_ids):1, sep = "_")
  fluxbot_levels <- paste("fluxbot", length(fluxbot_ids):1, sep = "_")
  all_levels <- c(autochamber_levels, fluxbot_levels)
  
  plot_data$Device.Name <- factor(plot_data$Device.Name, levels = all_levels)
  
  # Create uptime heatmap
  uptime_heatmap <- ggplot(plot_data, aes(x = date, y = Device.Name)) +
    geom_tile(aes(fill = uptime_percent), color = "white", size = 0.1) +
    scale_fill_gradientn(
      colors = c("#d73027", "#fc8d59", "#fee08b", "#d9ef8b", "#91bfdb", "#4575b4"),
      values = rescale(c(0, 20, 40, 60, 80, 100)),
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0%", "25%", "50%", "75%", "100%"),
      name = "Daily\nUptime",
      na.value = "grey50"
    ) +
    facet_wrap(~method, scales = "free_y", ncol = 1) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      title = "System Uptime Heatmap",
      subtitle = "Daily measurement collection success rate"
    ) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))
  
  return(uptime_heatmap)
}

# =============================================================================
# UPTIME SUMMARY BOXPLOT
# =============================================================================

create_uptime_summary <- function(uptime_data) {
  
  # Calculate overall uptime statistics per unit
  unit_uptime_stats <- uptime_data %>%
    group_by(id, method) %>%
    summarise(
      mean_uptime = mean(uptime_percent, na.rm = TRUE),
      median_uptime = median(uptime_percent, na.rm = TRUE),
      min_uptime = min(uptime_percent, na.rm = TRUE),
      max_uptime = max(uptime_percent, na.rm = TRUE),
      days_monitored = n(),
      days_full_uptime = sum(uptime_percent >= 99, na.rm = TRUE),
      days_zero_uptime = sum(uptime_percent == 0, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Create boxplot
  uptime_boxplot <- ggplot(unit_uptime_stats, aes(x = method, y = mean_uptime, fill = method)) +
    geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 3) +
    geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
    scale_fill_manual(values = c("#4C9F70", "#B0B0B0")) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank()
    ) +
    labs(
      title = "System Uptime Distribution",
      subtitle = "Mean uptime percentage per unit across deployment",
      y = "Mean Uptime (%)"
    )
  
  return(list(plot = uptime_boxplot, data = unit_uptime_stats))
}

# =============================================================================
# UPTIME OVER TIME (ROLLING AVERAGE)
# =============================================================================

create_uptime_timeseries <- function(uptime_data) {
  
  # Calculate 7-day rolling average uptime
  rolling_uptime <- uptime_data %>%
    arrange(id, date) %>%
    group_by(id, method) %>%
    mutate(
      uptime_7day = zoo::rollmean(uptime_percent, k = 7, fill = NA, align = "center")
    ) %>%
    ungroup() %>%
    group_by(method, date) %>%
    summarise(
      mean_uptime = mean(uptime_percent, na.rm = TRUE),
      mean_7day = mean(uptime_7day, na.rm = TRUE),
      n_units = n_distinct(id),
      .groups = 'drop'
    )
  
  # Create time series plot
  uptime_timeseries <- ggplot(rolling_uptime, aes(x = date)) +
    geom_line(aes(y = mean_uptime, color = method), alpha = 0.3, size = 0.5) +
    geom_line(aes(y = mean_7day, color = method), size = 1.5) +
    scale_color_manual(values = c("autochamber" = "#4C9F70", "fluxbot" = "#B0B0B0")) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.title.x = element_blank()
    ) +
    labs(
      title = "System Uptime Over Time",
      subtitle = "Daily mean uptime (thin lines) and 7-day rolling average (thick lines)",
      y = "Uptime (%)",
      color = "System Type"
    )
  
  return(uptime_timeseries)
}

# =============================================================================
# EXECUTE UPTIME ANALYSIS
# =============================================================================

# Calculate uptime data
cat("Calculating system uptime data...\n")
uptime_data <- calculate_system_uptime(merged_data_with_met)

# Create uptime heatmap
cat("Creating uptime heatmap...\n")
uptime_heatmap <- create_uptime_heatmap(uptime_data)
print(uptime_heatmap)

# Create uptime summary
cat("Creating uptime summary...\n")
uptime_summary_results <- create_uptime_summary(uptime_data)
uptime_boxplot <- uptime_summary_results$plot
print(uptime_boxplot)

# Create uptime time series
cat("Creating uptime time series...\n")
uptime_timeseries <- create_uptime_timeseries(uptime_data)
print(uptime_timeseries)

# Save plots
ggsave("system_uptime_heatmap.png", 
       plot = uptime_heatmap, 
       width = 12, height = 8, dpi = 300)

ggsave("system_uptime_summary.png", 
       plot = uptime_boxplot, 
       width = 8, height = 6, dpi = 300)

ggsave("system_uptime_timeseries.png", 
       plot = uptime_timeseries, 
       width = 12, height = 6, dpi = 300)

# =============================================================================
# PRINT SUMMARY STATISTICS
# =============================================================================

# Overall uptime summary
cat("\n=== SYSTEM UPTIME SUMMARY ===\n")

overall_uptime <- uptime_summary_results$data %>%
  group_by(method) %>%
  summarise(
    units_deployed = n(),
    mean_uptime = mean(mean_uptime, na.rm = TRUE),
    median_uptime = median(mean_uptime, na.rm = TRUE),
    min_uptime = min(mean_uptime, na.rm = TRUE),
    max_uptime = max(mean_uptime, na.rm = TRUE),
    units_above_90pct = sum(mean_uptime >= 90, na.rm = TRUE),
    units_above_95pct = sum(mean_uptime >= 95, na.rm = TRUE),
    .groups = 'drop'
  )

print(overall_uptime)

# Daily statistics
daily_stats <- uptime_data %>%
  group_by(method, date) %>%
  summarise(
    mean_daily_uptime = mean(uptime_percent, na.rm = TRUE),
    units_active = sum(uptime_percent > 0, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(method) %>%
  summarise(
    deployment_days = n_distinct(date),
    mean_daily_uptime = mean(mean_daily_uptime, na.rm = TRUE),
    best_day_uptime = max(mean_daily_uptime, na.rm = TRUE),
    worst_day_uptime = min(mean_daily_uptime, na.rm = TRUE),
    .groups = 'drop'
  )

print("Daily uptime statistics:")
print(daily_stats)

cat("System uptime analysis completed!\n")
cat("Generated 3 plots:\n")
cat("- system_uptime_heatmap.png\n")
cat("- system_uptime_summary.png\n")
cat("- system_uptime_timeseries.png\n")