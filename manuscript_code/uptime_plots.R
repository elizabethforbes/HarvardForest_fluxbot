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
  
  # First, let's examine the actual data PER UNIT to understand measurement frequency
  cat("=== DEBUGGING MEASUREMENT FREQUENCY PER UNIT ===\n")
  
  measurement_freq_check <- data %>%
    mutate(date = as.Date(hour_of_obs),
           hour = hour(hour_of_obs)) %>%
    group_by(method, id, date) %>%
    summarise(
      daily_measurements = n(),
      unique_hours = n_distinct(hour),
      .groups = 'drop'
    ) %>%
    group_by(method) %>%
    summarise(
      max_daily_per_unit = max(daily_measurements, na.rm = TRUE),
      mean_daily_per_unit = mean(daily_measurements, na.rm = TRUE),
      median_daily_per_unit = median(daily_measurements, na.rm = TRUE),
      units_analyzed = n_distinct(id),
      .groups = 'drop'
    )
  
  print("Actual measurement frequency analysis PER UNIT:")
  print(measurement_freq_check)
  
  # Also check what a typical good day looks like
  typical_day_check <- data %>%
    mutate(date = as.Date(hour_of_obs)) %>%
    group_by(method, id, date) %>%
    summarise(daily_measurements = n(), .groups = 'drop') %>%
    group_by(method) %>%
    summarise(
      p95_daily = quantile(daily_measurements, 0.95, na.rm = TRUE),
      p90_daily = quantile(daily_measurements, 0.90, na.rm = TRUE),
      p75_daily = quantile(daily_measurements, 0.75, na.rm = TRUE),
      .groups = 'drop'
    )
  
  print("Daily measurement percentiles PER UNIT:")
  print(typical_day_check)
  
  # Get the full deployment date range
  start_date <- min(as.Date(data$hour_of_obs), na.rm = TRUE)
  end_date <- max(as.Date(data$hour_of_obs), na.rm = TRUE)
  
  # Calculate daily uptime for each unit using the 95th percentile as "expected"
  # This represents a "good day" rather than theoretical maximum
  daily_uptime <- data %>%
    mutate(date = as.Date(hour_of_obs)) %>%
    group_by(date, id, method) %>%
    summarise(
      measurements_collected = n(),
      .groups = 'drop'
    ) %>%
    # Use the 95th percentile as "expected" measurements per day
    left_join(typical_day_check %>% select(method, p95_daily), by = "method") %>%
    rename(expected_per_day = p95_daily) %>%
    mutate(
      uptime_percent = pmin(100, (measurements_collected / expected_per_day) * 100)
    ) %>%
    # Complete the date sequence for all units (fill missing dates with 0% uptime)
    complete(date = seq(start_date, end_date, by = "day"), 
             nesting(id, method), 
             fill = list(measurements_collected = 0, uptime_percent = 0)) %>%
    # Add expected measurements for completed rows
    left_join(typical_day_check %>% select(method, p95_daily), by = "method") %>%
    mutate(expected_per_day = ifelse(is.na(expected_per_day), p95_daily, expected_per_day))
  
  cat("Expected measurements per day based on 95th percentile:\n")
  expected_summary <- daily_uptime %>%
    group_by(method) %>%
    summarise(expected_per_day = first(expected_per_day), .groups = 'drop')
  print(expected_summary)
  
  return(daily_uptime)
}

# =============================================================================
# UPTIME HEATMAP VISUALIZATION
# =============================================================================

create_uptime_heatmap <- function(uptime_data) {
  
  # Apply original IDs as device names (consistent with attempted plot)
  plot_data <- uptime_data %>%
    mutate(Device.Name = id)  # This keeps original names: autochamber1, autochamber2, etc.
  
  # Create factor levels sorted numerically
  autochamber_ids <- unique(uptime_data$id[uptime_data$method == "autochamber"])
  fluxbot_ids <- unique(uptime_data$id[uptime_data$method == "fluxbot"])
  
  # Sort numerically by extracting the number from each ID
  autochamber_levels <- autochamber_ids[order(as.numeric(gsub("\\D", "", autochamber_ids)), decreasing = TRUE)]
  fluxbot_levels <- fluxbot_ids[order(as.numeric(gsub("\\D", "", fluxbot_ids)), decreasing = TRUE)]
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
    theme_classic() +
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
# UPTIME SUMMARY BOXPLOT AND BARPLOT
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
    theme_classic() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Apply original IDs as device names (consistent with attempted plot)
  plot_data <- unit_uptime_stats %>%
    mutate(Device.Name = id)  # This keeps original names: autochamber1, autochamber2, etc.
  
  # Create factor levels sorted numerically
  autochamber_ids <- unique(unit_uptime_stats$id[unit_uptime_stats$method == "autochamber"])
  fluxbot_ids <- unique(unit_uptime_stats$id[unit_uptime_stats$method == "fluxbot"])
  
  # Sort numerically by extracting the number from each ID
  autochamber_levels <- autochamber_ids[order(as.numeric(gsub("\\D", "", autochamber_ids)), decreasing = TRUE)]
  fluxbot_levels <- fluxbot_ids[order(as.numeric(gsub("\\D", "", fluxbot_ids)), decreasing = TRUE)]
  all_levels <- c(autochamber_levels, fluxbot_levels)
  
  plot_data$Device.Name <- factor(plot_data$Device.Name, levels = all_levels)
  
  # Prepare data for barplot
  barplot_data <- plot_data %>%
    arrange(method, desc(mean_uptime))
  
  uptime_barplot <- ggplot(barplot_data, aes(x = reorder(Device.Name, mean_uptime), y = mean_uptime, fill = method)) +
    geom_col(alpha = 0.8) +
    scale_fill_manual(values = c("#4C9F70", "#B0B0B0")) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank(),
      legend.position = "none",
      strip.text = element_blank()
    ) +
    labs(
      y = "Mean Uptime (%)"
    ) +
    facet_wrap(~method, scales = "free_x")
  
  return(list(boxplot = uptime_boxplot, barplot = uptime_barplot, data = unit_uptime_stats))
}

# =============================================================================
# UPTIME OVER TIME (INDIVIDUAL UNITS + DAILY AVERAGES)
# =============================================================================

create_uptime_timeseries <- function(uptime_data) {
  
  # Debug: Check what's in the data
  cat("Debug: Unique units in uptime data:\n")
  units_check <- uptime_data %>%
    group_by(method) %>%
    summarise(n_units = n_distinct(id), .groups = 'drop')
  print(units_check)
  
  # Individual daily uptime (no rolling average - exact daily values)
  individual_daily <- uptime_data %>%
    arrange(id, date)
  
  # Debug: Check individual daily data
  cat("Debug: Sample of individual daily data:\n")
  sample_individual <- individual_daily %>%
    select(id, method, date, uptime_percent) %>%
    slice_head(n = 10)
  print(sample_individual)
  
  # Calculate system-wide daily averages (not rolling)
  system_daily <- individual_daily %>%
    group_by(method, date) %>%
    summarise(
      mean_daily_uptime = mean(uptime_percent, na.rm = TRUE),
      n_units = n_distinct(id),
      .groups = 'drop'
    )
  
  # Debug: Check system daily data
  cat("Debug: Sample of system daily data:\n")
  sample_system <- system_daily %>%
    slice_head(n = 10)
  print(sample_system)
  
  # Create time series plot with individual units + system averages
  uptime_timeseries <- ggplot() +
    # Individual unit lines (thin, transparent) - daily exact values per chamber
    geom_line(data = individual_daily, 
              aes(x = date, y = uptime_percent, group = id, color = method), 
              alpha = 0.3, size = 0.3) +
    # System average lines (thick) - daily averages across all units
    geom_line(data = system_daily, 
              aes(x = date, y = mean_daily_uptime, color = method), 
              size = 1.5) +
    scale_color_manual(values = c("autochamber" = "#4C9F70", "fluxbot" = "#B0B0B0")) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
    theme_classic() +
    theme(
      legend.position = "bottom",
      axis.title.x = element_blank()
    ) +
    labs(
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
uptime_boxplot <- uptime_summary_results$boxplot
uptime_barplot <- uptime_summary_results$barplot
print(uptime_boxplot)
print(uptime_barplot)

# Create uptime time series
cat("Creating uptime time series...\n")
uptime_timeseries <- create_uptime_timeseries(uptime_data)
print(uptime_timeseries)

# Create combined layout: barplot (3/4 width) and boxplot (1/4 width) on top, timeseries in middle, flux retained dotplot on bottom
library(patchwork)

# Load the flux retained plot from the previous analysis (assuming it exists)
# If the flux count data and plots were created earlier, we can add it here
# For now, we'll create a placeholder or you can run the flux count code first

uptime_combined <- (uptime_barplot + uptime_boxplot + plot_layout(widths = c(3, 1))) / 
  uptime_timeseries / 
  retained_plot+labs(title=NULL) + 
  plot_layout(heights = c(1, 1, 2))

print(uptime_combined)


uptime_combined_simple <- (uptime_barplot + uptime_boxplot + plot_layout(widths = c(3, 1))) / 
  uptime_timeseries 

print(uptime_combined_simple)

print(retained_plot)

# Save plots
ggsave("system_uptime_heatmap.png", 
       plot = uptime_heatmap, 
       width = 12, height = 8, dpi = 300)

ggsave("system_uptime_summary.png", 
       plot = uptime_boxplot, 
       width = 8, height = 6, dpi = 300)

ggsave("system_uptime_barplot.png", 
       plot = uptime_barplot, 
       width = 12, height = 6, dpi = 300)

ggsave("system_uptime_timeseries.png", 
       plot = uptime_timeseries, 
       width = 12, height = 6, dpi = 300)

ggsave("system_uptime_combined.png", 
       plot = uptime_combined, 
       width = 16, height = 16, dpi = 300)

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
    max_uptime = max(max_uptime, na.rm = TRUE),
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
cat("Generated 5 plots:\n")
cat("- system_uptime_heatmap.png\n")
cat("- system_uptime_summary.png\n")
cat("- system_uptime_barplot.png\n")
cat("- system_uptime_timeseries.png\n")
cat("- system_uptime_combined.png (barplot + boxplot top, timeseries middle, flux retained bottom)\n")