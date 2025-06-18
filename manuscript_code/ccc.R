
# Load the epiR package
library(epiR)

# Assuming your data is in 'merged_data_with_met'
# predicted_flux is the predicted flux column
# fluxL_umolm2sec is the actual flux column

# Calculate Lin's Concordance Correlation Coefficient (CCC)
ccc_result <- epi.ccc(merged_data_with_met$predicted_flux, merged_data_with_met$fluxL_umolm2sec)

# Extract the CCC value
ccc_value <- ccc_result$rho.c

# Add CCC to your plot title
ggplot(merged_data_with_met, aes(x = predicted_flux, y = fluxL_umolm2sec)) +
  geom_point(alpha = 0.6) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#4C72B0", size=1) +
  labs(title = paste("Predicted vs Actual Flux (R² =", round(rsq, 2), ", CCC =", round(ccc_value, 2), ")"),
       x = "Predicted Flux (umol/m2/sec)", 
       y = "Actual Flux (umol/m2/sec)") +
  theme_classic()
