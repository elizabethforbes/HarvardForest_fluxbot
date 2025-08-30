# Load required libraries
library(ggplot2)
library(dplyr)
library(viridis)
library(hexbin)
library(ggpointdensity)

# ==============================================================================
# OPTION 1: Hexagonal Binning (Best for large datasets)
# ==============================================================================

p1_hex <- ggplot(rolling_analysis_data, aes(x = starting_conc_3h, y = flux_difference_3h)) +
  geom_hex(bins = 30, alpha = 0.8) +
  scale_fill_viridis_c(name = "Count", option = "plasma", trans = "log10") +
  geom_smooth(method = "lm", color = "red", se = TRUE, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "3-Hour Mean Starting CO₂ Concentration (ppm)",
    y = "3-Hour Mean Flux Bias (Fluxbot - Autochamber)",
    title = "Data Density: Hexagonal Binning",
    subtitle = "Darker hexagons = higher data density"
  ) +
  theme_classic()

print(p1_hex)

# ==============================================================================
# OPTION 2: 2D Density Contours with Points
# ==============================================================================

p2_density <- ggplot(rolling_analysis_data, aes(x = starting_conc_3h, y = flux_difference_3h)) +
  geom_point(alpha = 0.3, color = "gray40", size = 0.8) +
  geom_density_2d_filled(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "white", size = 1) +
  scale_fill_viridis_d(name = "Density", option = "plasma") +
  labs(
    x = "3-Hour Mean Starting CO₂ Concentration (ppm)",
    y = "3-Hour Mean Flux Bias (Fluxbot - Autochamber)",
    title = "Data Density: 2D Density Contours",
    subtitle = "Contour lines show data concentration areas"
  ) +
  theme_classic()

print(p2_density)

# ==============================================================================
# OPTION 3: Point Density Coloring
# ==============================================================================

p3_pointdensity <- ggplot(rolling_analysis_data, aes(x = starting_conc_3h, y = flux_difference_3h)) +
  geom_pointdensity(size = 1.5, alpha = 0.7) +
  scale_color_viridis_c(name = "Local\nDensity", option = "plasma") +
  geom_smooth(method = "lm", color = "red", se = TRUE, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "3-Hour Mean Starting CO₂ Concentration (ppm)",
    y = "3-Hour Mean Flux Bias (Fluxbot - Autochamber)",
    title = "Data Density: Point Density Coloring",
    subtitle = "Points colored by local data density"
  ) +
  theme_classic()

print(p3_pointdensity)

# ==============================================================================
# OPTION 4: Marginal Density Plots (ggExtra style)
# ==============================================================================

# Create base plot for marginal densities
p4_base <- ggplot(rolling_analysis_data, aes(x = starting_conc_3h, y = flux_difference_3h)) +
  geom_point(alpha = 0.4, color = "darkblue", size = 1) +
  geom_smooth(method = "lm", color = "red", se = TRUE, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "3-Hour Mean Starting CO₂ Concentration (ppm)",
    y = "3-Hour Mean Flux Bias (Fluxbot - Autochamber)",
    title = "Data Density: Points with Marginal Distributions"
  ) +
  theme_classic()

# Create marginal density for x-axis (starting concentration)
p4_top <- ggplot(rolling_analysis_data, aes(x = starting_conc_3h)) +
  geom_density(fill = "lightblue", alpha = 0.7, color = "darkblue") +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

# Create marginal density for y-axis (bias)
p4_right <- ggplot(rolling_analysis_data, aes(x = flux_difference_3h)) +
  geom_density(fill = "lightcoral", alpha = 0.7, color = "darkred") +
  coord_flip() +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

# Print base plot (marginal plots would need cowplot or patchwork to combine)
print(p4_base)
print(p4_top)
print(p4_right)

# ==============================================================================
# OPTION 5: Binned Scatter with Size Mapping
# ==============================================================================

# Create bins for concentration ranges
binned_data <- rolling_analysis_data %>%
  mutate(
    conc_bin = round(starting_conc_3h / 50) * 50,  # 50 ppm bins
    bias_bin = round(flux_difference_3h / 0.2) * 0.2  # 0.2 bias unit bins
  ) %>%
  group_by(conc_bin, bias_bin) %>%
  summarise(
    count = n(),
    mean_conc = mean(starting_conc_3h),
    mean_bias = mean(flux_difference_3h),
    .groups = 'drop'
  ) %>%
  filter(count > 0)

p5_binned <- ggplot(binned_data, aes(x = mean_conc, y = mean_bias)) +
  geom_point(aes(size = count, color = count), alpha = 0.7) +
  scale_size_continuous(name = "Count", range = c(1, 8)) +
  scale_color_viridis_c(name = "Count", option = "plasma", trans = "sqrt") +
  geom_smooth(data = rolling_analysis_data, 
              aes(x = starting_conc_3h, y = flux_difference_3h),
              method = "lm", color = "red", se = TRUE, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "3-Hour Mean Starting CO₂ Concentration (ppm)",
    y = "3-Hour Mean Flux Bias (Fluxbot - Autochamber)",
    title = "Data Density: Binned Scatter Plot",
    subtitle = "Point size and color represent number of observations in each bin"
  ) +
  theme_classic()

print(p5_binned)

# ==============================================================================
# OPTION 6: Split by Concentration Ranges (Faceting)
# ==============================================================================

# Create broader concentration categories for faceting
facet_data <- rolling_analysis_data %>%
  mutate(
    conc_range = case_when(
      starting_conc_3h < 500 ~ "300-500 ppm\n(Low)",
      starting_conc_3h >= 500 & starting_conc_3h < 700 ~ "500-700 ppm\n(Normal)",
      starting_conc_3h >= 700 & starting_conc_3h < 900 ~ "700-900 ppm\n(Elevated)",
      starting_conc_3h >= 900 ~ "900+ ppm\n(High)"
    ),
    conc_range = factor(conc_range, levels = c("300-500 ppm\n(Low)", "500-700 ppm\n(Normal)", 
                                               "700-900 ppm\n(Elevated)", "900+ ppm\n(High)"))
  )

p6_facet <- ggplot(facet_data, aes(x = starting_conc_3h, y = flux_difference_3h)) +
  geom_point(alpha = 0.6, color = "darkblue", size = 1.2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~conc_range, scales = "free_x", ncol = 2) +
  labs(
    x = "3-Hour Mean Starting CO₂ Concentration (ppm)",
    y = "3-Hour Mean Flux Bias (Fluxbot - Autochamber)",
    title = "Data Density: Faceted by Concentration Range",
    subtitle = "Each panel shows detailed patterns within concentration ranges"
  ) +
  theme_classic() +
  theme(strip.text = element_text(size = 10))

print(p6_facet)

# ==============================================================================
# SUMMARY AND RECOMMENDATIONS
# ==============================================================================

cat("=== DATA DENSITY VISUALIZATION OPTIONS ===\n\n")

cat("OPTION 1 - Hexagonal Binning:\n")
cat("✓ Best for very large datasets\n")
cat("✓ Shows density clearly without overplotting\n")
cat("✓ Works well with log-scale coloring\n\n")

cat("OPTION 2 - 2D Density Contours:\n") 
cat("✓ Shows density regions clearly\n")
cat("✓ Good for identifying concentration 'hotspots'\n")
cat("✓ Maintains individual point visibility\n\n")

cat("OPTION 3 - Point Density Coloring:\n")
cat("✓ Each point colored by local density\n")
cat("✓ Good balance between detail and overview\n")
cat("✓ Easy to interpret\n\n")

cat("OPTION 4 - Marginal Densities:\n")
cat("✓ Shows distribution of both variables separately\n") 
cat("✓ Clearly shows concentration clustering < 800 ppm\n")
cat("✓ Good for understanding data structure\n\n")

cat("OPTION 5 - Binned Scatter:\n")
cat("✓ Point size shows local density\n")
cat("✓ Reduces overplotting while showing patterns\n")
cat("✓ Good for statistical analysis\n\n")

cat("OPTION 6 - Faceted Ranges:\n")
cat("✓ Separate detailed view of each concentration range\n")
cat("✓ Shows patterns within sparse high-concentration data\n")
cat("✓ Good for comparing bias patterns across ranges\n\n")

cat("RECOMMENDATION: Try hexagonal binning (#1) or point density coloring (#3)\n")
cat("for the clearest visualization of your concentration clustering!\n")