# Plot starting CO2 concentration over time with rainfall (filtered data)
library(ggplot2)
library(dplyr)
library(viridis)
library(scales)
library(lubridate)
library(fuzzyjoin)

# First, prepare the meteorological data
# Load HF met data (adjust the date range to match your study period)
met_data <- read.csv("https://harvardforest.fas.harvard.edu/data/p00/hf001/hf001-10-15min-m.csv")
met_data$ymd <- ymd(str_split_fixed(met_data$datetime, "T", 2)[,1])
met_data <- met_data[which(met_data$ymd >= ymd("2023-10-01") & met_data$ymd <= ymd("2023-11-02")),]

met_data <- met_data %>% 
  select(datetime, prec) %>% 
  rename(precip = prec) %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M", tz = "America/New_York"))

# Merge concentration data with precipitation data using fuzzy join
concentration_with_precip <- all_flux_comparison_filtered %>%
  filter(!is.na(starting_conc)) %>%
  difference_left_join(met_data, 
                       by = c("hour_of_obs" = "datetime"),
                       max_dist = as.difftime(60, units = "mins")) %>%
  # Take the closest match
  group_by(hour_of_obs, starting_conc) %>%
  slice_min(abs(as.numeric(difftime(hour_of_obs, datetime, units = "mins")))) %>%
  ungroup() %>%
  select(hour_of_obs, starting_conc, precip)

# Create the dual-axis plot with point density coloring
library(ggpointdensity)

concentration_precip_plot <- ggplot(concentration_with_precip, aes(x = hour_of_obs)) +
  # Add precipitation bars (scaled to secondary axis)
  geom_col(aes(y = precip * 1000), alpha = 0.3, fill = "blue", width = 3600) +  # Scale precip to full range
  # Add concentration points with density coloring
  geom_pointdensity(aes(y = starting_conc), size = 1.5, alpha = 0.3) +
  #scale_color_viridis_c(name = "Observation\nDensity", option = "magma") +
  # Define custom breaks to mimic viridis/turbo spacing
  scale_color_viridis_c(
    name = "Observation\nDensity", 
    option = "inferno",
    direction = 1,
    trans = "log10"  # This changes the spacing - try "log10" or "exp" too
  ) +
  # Add horizontal lines for concentration categories
  geom_hline(yintercept = c(450, 600, 850), linetype = "dashed", 
             color = c("darkblue", "orange", "red"), alpha = 0.7) +
  # Create secondary y-axis for precipitation - show full range
  scale_y_continuous(
    #name = "Starting CO₂ Concentration (ppm)",
    name = expression("Starting CO"[2] ~ "Concentration"),
    sec.axis = sec_axis(~ . / 1000, name = "Precipitation (mm)"),
    limits = c(0, max(concentration_with_precip$starting_conc, na.rm = TRUE))
  ) +
  labs(
    x = "Date and Time",
    #title = "Starting CO₂ Concentrations and Precipitation Over Time",
    #subtitle = "Blue bars show precipitation; points colored by local data density"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.y.right = element_text(color = "blue")
  ) +
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "3 days")

print(concentration_precip_plot)
ggsave("starting_concentration_precipitation_timeseries.png", concentration_precip_plot, 
       width = 14, height = 6, dpi = 300)

# Alternative version: Daily aggregation to better show patterns
daily_summary <- concentration_with_precip %>%
  mutate(date = as.Date(hour_of_obs)) %>%
  group_by(date) %>%
  summarise(
    n_obs = n(),
    mean_conc = mean(starting_conc, na.rm = TRUE),
    max_conc = max(starting_conc, na.rm = TRUE),
    high_conc_events = sum(starting_conc >= 850, na.rm = TRUE),
    daily_precip = mean(precip, na.rm = TRUE),  # Should ideally be sum, but using mean to avoid double-counting
    .groups = 'drop'
  )

daily_plot <- ggplot(daily_summary, aes(x = date)) +
  # Add precipitation bars
  geom_col(aes(y = daily_precip * 100), alpha = 0.4, fill = "blue") +  # Scale precip for visibility
  # Add daily max concentration
  geom_point(aes(y = max_conc, color = high_conc_events), size = 3) +
  geom_line(aes(y = max_conc), alpha = 0.5) +
  scale_color_gradient(name = "High Conc\nEvents", low = "green", high = "red") +
  # Add horizontal lines for concentration categories  
  geom_hline(yintercept = 850, linetype = "dashed", color = "red", alpha = 0.7) +
  scale_y_continuous(
    name = "Daily Maximum CO₂ Concentration (ppm)",
    sec.axis = sec_axis(~ . / 100, name = "Daily Precipitation (mm)")
  ) +
  labs(
    x = "Date",
    title = "Daily Maximum CO₂ Concentrations vs Precipitation",
    subtitle = "Blue bars = precipitation; points = max daily concentration (colored by # high events)"
  ) +
  theme_classic() +
  theme(
    axis.title.y.right = element_text(color = "blue"),
    axis.text.y.right = element_text(color = "blue")
  ) +
  scale_x_date(date_labels = "%m/%d", date_breaks = "3 days")

print(daily_plot)
ggsave("daily_concentration_precipitation_summary.png", daily_plot, 
       width = 12, height = 6, dpi = 300)

# Summary statistics comparing precipitation and high-concentration events
precip_analysis <- concentration_with_precip %>%
  mutate(
    high_conc = starting_conc >= 850,
    any_precip = precip > 0,
    significant_precip = precip > 0.5  # Adjust threshold as needed
  )

precip_summary <- precip_analysis %>%
  group_by(high_conc) %>%
  summarise(
    n = n(),
    mean_precip = mean(precip, na.rm = TRUE),
    prop_any_precip = mean(any_precip, na.rm = TRUE),
    prop_sig_precip = mean(significant_precip, na.rm = TRUE),
    .groups = 'drop'
  )

print("Summary of precipitation patterns by concentration level:")
print(precip_summary)

# Statistical test for association between precipitation and high concentrations
if(sum(precip_analysis$high_conc, na.rm = TRUE) > 0 & sum(precip_analysis$any_precip, na.rm = TRUE) > 0) {
  precip_test <- chisq.test(precip_analysis$high_conc, precip_analysis$any_precip)
  cat("\nChi-square test for association between high CO2 and precipitation:\n")
  cat("Chi-square =", round(precip_test$statistic, 3), 
      ", p-value =", format(precip_test$p.value, scientific = TRUE), "\n")
}

cat("\nTemporal correlation analysis:\n")
cat("- Total observations:", nrow(concentration_with_precip), "\n")
cat("- High concentration events (≥850 ppm):", sum(concentration_with_precip$starting_conc >= 850, na.rm = TRUE), "\n")
cat("- Observations with precipitation >0:", sum(concentration_with_precip$precip > 0, na.rm = TRUE), "\n")
cat("- High conc events coinciding with precip:", 
    sum(concentration_with_precip$starting_conc >= 850 & concentration_with_precip$precip > 0, na.rm = TRUE), "\n")