# Load required libraries
library(ggplot2)
library(dplyr)
library(viridis)

# Step 1: Merge starting concentration data into the main dataset
# First, prepare the concentration data from HF_fluxestimates
concentration_data <- HF_fluxestimates %>%
  select(hour_of_obs, id, starting_concen, ending_concen) %>%
  rename(starting_conc = starting_concen, ending_conc = ending_concen)

# Create a pivot of merged_data to get autochamber and fluxbot in same rows
# First, separate fluxbot and autochamber data
fluxbot_data <- merged_data %>%
  filter(method == "fluxbot") %>%
  left_join(concentration_data, by = c("hour_of_obs", "id")) %>%
  select(hour_of_obs, stand, fluxL_umolm2sec, starting_conc, ending_conc) %>%
  rename(fluxbot_flux = fluxL_umolm2sec)

autochamber_data <- merged_data %>%
  filter(method == "autochamber") %>%
  group_by(hour_of_obs, stand) %>%
  summarise(autochamber_flux = mean(fluxL_umolm2sec, na.rm = TRUE), .groups = 'drop')

# Merge fluxbot and autochamber data
all_flux_comparison <- fluxbot_data %>%
  left_join(autochamber_data, by = c("hour_of_obs", "stand")) %>%
  filter(!is.na(fluxbot_flux) & !is.na(autochamber_flux))

# Plot 1: All data with starting concentration coloring where available
p1 <- all_flux_comparison %>% 
  ggplot(aes(x = autochamber_flux, y = fluxbot_flux)) +
  geom_point(aes(color = starting_conc), size = 2, alpha = 0.3) +
  scale_color_viridis_c(name = "Starting CO₂\n(ppm)", option = "plasma", na.value = "gray60") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
  labs(
    x = expression(paste("Autochamber flux estimate, ", mu, "mol ", CO[2], " ", m^-2, s^-1)),
    y = expression(paste("Fluxbot flux estimate, ", mu, "mol ", CO[2], " ", m^-2, s^-1)),
    title = "All Fluxbot vs Autochamber Flux Estimates",
    subtitle = "Points colored by starting CO₂ concentration (gray = no concentration data)"
  ) +
  theme_classic() +
  coord_fixed() +
  xlim(0, 6) + ylim(0, 6)

# Print the plot
print(p1)

# Plot 2: Faceted by stand
p2 <- all_flux_comparison %>% 
  ggplot(aes(x = autochamber_flux, y = fluxbot_flux)) +
  geom_point(aes(color = starting_conc), size = 2, alpha = 0.7) +
  geom_point(data = filter(all_flux_comparison, is.na(starting_conc)), 
             color = "gray60", size = 1.5, alpha = 0.5) +
  scale_color_viridis_c(name = "Starting CO₂\n(ppm)", option = "plasma", na.value = "gray60") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
  labs(
    x = expression(paste("Autochamber flux estimate, ", mu, "mol ", CO[2], " ", m^-2, s^-1)),
    y = expression(paste("Fluxbot flux estimate, ", mu, "mol ", CO[2], " ", m^-2, s^-1)),
    title = "All Fluxbot vs Autochamber Flux Estimates by Stand",
    subtitle = "Points colored by starting CO₂ concentration (gray = no concentration data)"
  ) +
  theme_classic() +
  coord_fixed() +
  xlim(0, 6) + ylim(0, 6) +
  facet_wrap(~stand, labeller = as_labeller(c("healthy" = "Stand 1 (Healthy)", 
                                              "unhealthy" = "Stand 2 (Unhealthy)")))

# Print the faceted plot
print(p2)

# Plot 3: Only points with starting concentration data (for clearer concentration patterns)
p3 <- all_flux_comparison %>% 
  filter(!is.na(starting_conc)) %>%
  ggplot(aes(x = autochamber_flux, y = fluxbot_flux, color = starting_conc)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_viridis_c(name = "Starting CO₂\n(ppm)", option = "plasma") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
  labs(
    x = expression(paste("Autochamber flux estimate, ", mu, "mol ", CO[2], " ", m^-2, s^-1)),
    y = expression(paste("Fluxbot flux estimate, ", mu, "mol ", CO[2], " ", m^-2, s^-1)),
    title = "Fluxbot vs Autochamber Flux Estimates with Starting Concentrations",
    subtitle = "Only measurements with starting CO₂ concentration data shown"
  ) +
  theme_classic() +
  coord_fixed() +
  xlim(0, 6) + ylim(0, 6)

print(p3)

# Summary statistics
cat("Summary of all flux comparison data:\n")
cat("Total observations:", nrow(all_flux_comparison), "\n")
cat("Observations with concentration data:", sum(!is.na(all_flux_comparison$starting_conc)), "\n")
cat("Observations without concentration data:", sum(is.na(all_flux_comparison$starting_conc)), "\n")

if(sum(!is.na(all_flux_comparison$starting_conc)) > 0) {
  cat("Starting concentration range:", range(all_flux_comparison$starting_conc, na.rm = TRUE), "ppm\n")
}
cat("Fluxbot flux range:", range(all_flux_comparison$fluxbot_flux, na.rm = TRUE), "µmol m⁻² s⁻¹\n")
cat("Autochamber flux range:", range(all_flux_comparison$autochamber_flux, na.rm = TRUE), "µmol m⁻² s⁻¹\n")

