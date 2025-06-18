library(ineq)

# Filter the data to only include hours where there are at least 12 autochambers and 12 fluxbots
filtered_data <- merged_data %>%
  group_by(hour_of_obs, stand) %>%
  filter(
    sum(method == "autochamber" & !is.na(fluxL_umolm2sec)) >= 12 &
      sum(method == "fluxbot" & !is.na(fluxL_umolm2sec)) >= 12
  ) %>%
  ungroup()

# Summarize flux data by location (e.g., chamber or sensor ID) to capture spatial heterogeneity
flux_summary_by_location <- filtered_data %>%
  group_by(method, id, stand) %>%
  summarize(mean_flux = mean(fluxL_umolm2sec, na.rm = TRUE)) %>%
  ungroup()

# Function to calculate Lorenz curve data for ggplot and Gini coefficient
lorenz_data_with_gini <- function(flux) {
  lc <- Lc(flux)
  data.frame(p = lc$p, L = lc$L)
}




# Calculate Lorenz curve data and Gini coefficients for each method based on spatial locations
lorenz_per_method <- flux_summary_by_location %>%
  group_by(method) %>%
  summarize(
    data = list(lorenz_data_with_gini(mean_flux)),
    Gini_flux = Gini(mean_flux, na.rm = TRUE)
  ) %>%
  unnest(cols = c(data))

# Plot Lorenz curves to visualize spatial heterogeneity and overlay Gini coefficients
ggplot(lorenz_per_method, aes(x = p, y = L, color = method)) +
  geom_line(size = 1) +
  geom_point(size = 2, alpha = 0.6) +  # Add individual points
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") + # 45-degree equality line
  facet_wrap(~ method) +
  scale_color_manual(values = c("#4C9F70", "#B0B0B0")) +
  labs(
    x = "Cumulative Share of Chambers",
    y = "Cumulative Share of Flux"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    panel.grid.major = element_line(color = "gray90", size = 0.25), # Add major gridlines
    #panel.grid.minor = element_line(color = "gray90", size = 0.25)  # Add minor gridlines
  ) +
  geom_text(aes(x = 0.8, y = 0.2, label = paste("Gini:", sprintf("%.2f", Gini_flux))),
            color = "black", size = 4, inherit.aes = FALSE)






# Calculate CV by treatment
cv_by_treatment <- filtered_data %>%
  group_by(method) %>%
  summarize(
    mean_flux = mean(fluxL_umolm2sec, na.rm = TRUE),
    sd_flux = sd(fluxL_umolm2sec, na.rm = TRUE),
    cv_flux = (sd_flux / mean_flux) * 100
  )
cv_by_treatment
