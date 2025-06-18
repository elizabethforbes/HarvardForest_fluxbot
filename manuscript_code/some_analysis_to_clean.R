library(lme4)
library(nlme)
library(lmerTest)
library(lubridate)

# Select and rename columns in HF_fluxestimates
HF_fluxestimates_subset <- HF_fluxestimates %>%
  mutate(date = as.Date(as.POSIXct(hour_of_obs)))
HF_fluxestimates_subset <- HF_fluxestimates_subset %>%
  select(id, hour_of_obs, date, fluxL_umolm2sec, stand)
HF_fluxestimates_subset$method<-"fluxbot"

# HF_fluxestimates_subset$hour_of_obs<-ymd_hms(HF_fluxestimates_subset$hour_of_obs)



# HFauto_fluxestimates$fluxL_umolm2sec<-HFauto_fluxestimates$rs
# HFauto_fluxestimates$id<-HFauto_fluxestimates$autochamber

# Select and rename columns in HFauto_fluxestimates
HFauto_fluxestimates_subset <- HFauto_fluxestimates %>%
  mutate(date = as.Date(as.POSIXct(hour_of_obs)))
HFauto_fluxestimates_subset <- HFauto_fluxestimates_subset %>% 
  select(id, hour_of_obs, date, fluxL_umolm2sec, stand)
HFauto_fluxestimates_subset$method<-"autochamber"

# HFauto_fluxestimates_subset$hour_of_obs<-ymd_hms(HFauto_fluxestimates_subset$hour_of_obs)

# # Quick fix: Standardize hour_of_obs in both datasets before rbinding
# HF_fluxestimates_subset <- HF_fluxestimates_subset %>%
#   mutate(hour_of_obs = if(is.list(hour_of_obs)) {
#     ymd_hms(unlist(hour_of_obs))
#   } else {
#     as.POSIXct(hour_of_obs, tz = "America/New_York")
#   })
# 
# HFauto_fluxestimates_subset <- HFauto_fluxestimates_subset %>%
#   mutate(hour_of_obs = as.POSIXct(hour_of_obs))

# Bind the rows from both datasets
merged_data <- rbind(HF_fluxestimates_subset, HFauto_fluxestimates_subset)
# create day, hour variables: 
merged_data <- merged_data %>%
  mutate(
    # Don't force timezone conversion - use what's already there
    day_of_year = yday(hour_of_obs),
    hour_of_day = hour(hour_of_obs)
  )


ggplot(merged_data, aes(y=as.factor(id), x=fluxL_umolm2sec))+
  geom_jitter(alpha=0.25, aes(color=stand))+
  geom_boxplot(outliers=F, fill=NA)+
  facet_wrap(~method, scales = "free_y")+
  scale_color_manual(values = c("#4C9F70", "#B0B0B0"))

model <- lmer(fluxL_umolm2sec ~ stand + (1 | id) + method + yday(hour_of_obs) + hour(hour_of_obs), data = merged_data)

summary(model)

str(merged_data)


library(mgcv)

# Ensure id is a factor and hour_of_obs is in the correct format
merged_data$id <- as.factor(merged_data$id)

# Fit the GAM model with a smooth term for hour of day and random effect for id
model_gam <- gam(fluxL_umolm2sec ~ stand + s(hour(hour_of_obs)) + method + s(yday(hour_of_obs)) + s(id, bs = "re"), 
                 data = merged_data, method = "REML")

# Summarize and plot the model
summary(model_gam)
plot(model_gam)

# Extract the DOY (Day of Year) from the Time column
met_data$doy <- yday(met_data$datetime)
met_data$year <- year(met_data$datetime)

# Subset the data for DOY between 270 and 310, in the year 2023 (deployment year)
met_data_subset <- met_data %>%
  filter(year == 2023) %>% 
  filter(doy >= 273 & doy <= 308)


# Create a new column for the date (without time) from the Time column
met_data_subset$date <- as.Date(met_data_subset$datetime)

# Calculate the daily mean of soiltemp_10cm_HF
daily_mean_soiltemp <- met_data_subset %>%
  group_by(date) %>%
  summarize(daily_mean_soiltemp = mean(s10t, na.rm = TRUE))

ggplot(aes(y=daily_mean_soiltemp, x=date), data=daily_mean_soiltemp)+
  geom_point()+
  geom_smooth(method = "loess", span = 0.28, se=F)+
  theme_classic()

# Convert hour to a factor
merged_data$hour_factor <- as.factor(hour(merged_data$hour_of_obs))

# Fit the mixed model with hour as a factor
model_factor <- lmer(fluxL_umolm2sec ~ stand + (1 | id) + method + yday(hour_of_obs) + hour_factor, 
                     data = merged_data)
summary(model_factor)



str(met_data)






# Filter dataset for hours where both methods and stands have at least 5 chambers (id)
filtered_data <- merged_data %>%
  group_by(hour_of_obs, method, stand) %>%
  filter(n_distinct(id) >= 5) %>%
  ungroup() %>%
  group_by(hour_of_obs) %>%
  filter(all(c("fluxbot", "autochamber") %in% method) & 
           all(c("healthy", "unhealthy") %in% stand)) %>%
  ungroup()

# Step 1: Calculate the mean flux for each hour, stand, and method
mean_data <- filtered_data %>%
  group_by(hour_of_obs, stand, method) %>%
  summarize(mean_flux = mean(fluxL_umolm2sec, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Pivot data so that we have autochamber and fluxbot as columns
pivoted_data <- mean_data %>%
  pivot_wider(names_from = method, values_from = mean_flux)

pivoted_data <- pivoted_data %>%
  filter((autochamber > 1) & (!is.na(fluxbot) | !is.na(autochamber)))


# Step 3: Plot autochamber vs fluxbot
ggplot(pivoted_data, aes(x = autochamber, y = fluxbot)) +
  geom_point(size=2, alpha=0.5) +
  geom_abline(intercept = 0, slope=1, linetype="dashed", size=1, color="black")+
  labs(x = "Autochamber Mean Flux", y = "Fluxbot Mean Flux",
       title = "Autochamber vs Fluxbot Mean Flux") +
  theme_classic()+
  geom_smooth(method="lm", color="red")+
  xlim(1,4.5)+ylim(1,4.5)








# Convert datetime columns to POSIXct if they are not already
merged_data$hour_of_obs <- as.POSIXct(merged_data$hour_of_obs)
met_data$Time <- as.POSIXct(met_data$datetime, format = "%Y-%m-%dT%H:%M")


library(fuzzyjoin)

# Perform fuzzy join based on closest datetime with a larger time difference limit (60 mins)
merged_data_with_met <- difference_left_join(merged_data, met_data_subset, 
                                             by = c("hour_of_obs" = "Time"),
                                             max_dist = as.difftime(60, units = "mins")) %>%
  # Add a time difference column to verify the matching process
  mutate(time_diff = abs(difftime(hour_of_obs, Time, units = "mins"))) %>%
  # Keep only the closest match for each row in `merged_data`
  group_by(id, hour_of_obs) %>%
  slice_min(time_diff, with_ties = FALSE) %>%
  ungroup()


# Function to rename the id column
merged_data_with_met <- merged_data_with_met %>%
  mutate(id = case_when(
    grepl("^\\d+$", id) ~ paste0("autochamber", id),  # If the id is a number only, prepend "chamber"
    grepl("^fluxes_bot", id) ~ gsub("fluxes_bot", "fluxbot", id),  # Replace "fluxes_bot" with "chamber"
    TRUE ~ id  # Leave others unchanged
  ))


# Calculate mean flux for each id, stand, and method
mean_flux_order <- merged_data_with_met %>%
  group_by(id, stand, method) %>%
  summarize(mean_flux = mean(fluxL_umolm2sec, na.rm = TRUE)) %>%
  arrange(desc(method), desc(mean_flux)) %>%
  ungroup()

# Reorder the id factor based on the calculated mean flux within each stand and method
merged_data_with_met <- merged_data_with_met %>%
  mutate(id = factor(id, levels = mean_flux_order$id))


# Plot the data with reordered id
ggplot(merged_data_with_met, aes(y = as.factor(id), x = fluxL_umolm2sec)) +
  geom_jitter(alpha = 0.3, aes(color = method), height = 0.2) +
  geom_boxplot(outliers = FALSE, fill = NA) +
  facet_wrap(~stand, scales = "free_y") +
  labs(y = "", 
       x=expression(paste(CO[2], " Flux, ", mu, "mol ", m^-2, s^-1))) +
  scale_color_manual(values = c("#4C9F70", "#B0B0B0")) +
  theme_classic() +
  theme(legend.position="none")


library(car)

# Perform Levene's Test for variances between plots and methods
leveneTest(fluxL_umolm2sec ~ stand * method, data = merged_data_with_met)


# View the resulting merged dataframe with the time difference column

ggplot(merged_data_with_met, aes(y=as.factor(id), x=fluxL_umolm2sec))+
  geom_jitter(alpha=0.3, aes(color=stand), height=0.2)+
  geom_boxplot(outliers=F, fill=NA)+
  facet_wrap(~method, scales = "free_y")


ggplot(merged_data_with_met, aes(x = s10t, y = fluxL_umolm2sec, color=method)) +
  geom_point(alpha = 0.1) +
  #facet_wrap(~method) +
  geom_smooth(method = "nls", 
              formula = y ~ a * exp(b * x), 
              method.args = list(start = list(a = 1, b = 0.1)), 
              se = FALSE, # turn off standard error ribbons for a cleaner look
              ) +
  theme_minimal() +
  labs(x = "Soil Temperature (10cm)", 
       y = "Flux (umol/m2/s)", 
       title = "Q10 Model Fit for Flux vs. Soil Temperature")+
  scale_color_manual(values = c("#4C9F70", "#B0B0B0"))
  

# Fit the exponential model using nls
nls_model <- nls(fluxL_umolm2sec ~ a * exp(b * s10t), 
                 data = merged_data_with_met[which(merged_data_with_met$method=="fluxbot"),], 
                 start = list(a = 1, b = 0.1))

# Extract the parameter b
b_estimate <- coef(nls_model)["b"]

# Calculate Q10 from the b parameter
Q10 <- exp(10 * b_estimate)

# Display the Q10 value
Q10

autochambers<-merged_data_with_met%>%filter(method=="autochamber")

# Fit the GAM model with a smooth term for hour of day and random effect for id
model_gam <- gam(fluxL_umolm2sec ~ stand + s(hour(hour_of_obs)) + 
                   s(id, bs = "fs") + s(s10t), 
                 data = autochambers, method = "REML")

summary(model_gam)
plot(model_gam)




# Fit the GAM model with a smooth term for hour of day and random effect for id
model_gam <- gam(fluxL_umolm2sec ~ stand + s(hour(hour_of_obs)) + method + 
                   s(id, bs = "fs") + s(s10t), 
                 data = merged_data_with_met, method = "REML")

# Summarize and plot the model
summary(model_gam)
plot(model_gam)
concurvity(model_gam)


# Get predicted values from the model
merged_data_with_met$predicted_flux <- predict(model_gam, newdata = merged_data_with_met)

# Calculate R-squared for model fit
rsq <- summary(model_gam)$r.sq

# Assuming 'model' has already been fitted and 'rsq' is calculated
# Extract slope and intercept from the model for the equation
model <- lm(fluxL_umolm2sec ~ predicted_flux, data = merged_data_with_met)
slope <- coef(model)[2]
intercept <- coef(model)[1]
rsq <- summary(model)$r.squared

# Create the equation and R² label
equation_label <- paste0("y = ", round(intercept, 2), " + ", round(slope, 2), "x,  R² = ", round(rsq, 2))

# Create the scatter plot and display the equation and R² on the plot
ggplot(merged_data_with_met, aes(x = predicted_flux, y = fluxL_umolm2sec)) +
  geom_point(alpha = 0.2) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#5A82C9", size=1) +
  labs(
    x = expression(paste("Predicted Flux, ", mu, "mol ", CO[2], " ", m^-2, s^-1)),
    y = expression(paste("Actual Flux, ", mu, "mol ", CO[2], " ", m^-2, s^-1))
  ) +
  theme_classic() +
  annotate("text", x = Inf, y = Inf, label = equation_label, hjust = 2, vjust = 1.5, size = 5, color = "#5A82C9")







# Calculate a 3-hour rolling average 
library(zoo)

merged_data_with_met <- merged_data_with_met %>%
  arrange(hour_of_obs) %>%
  group_by(stand, method) %>%
  mutate(rolling_avg = rollapply(fluxL_umolm2sec, width = 24*6, FUN = mean, fill = NA, align = "center")) %>%
  ungroup()

# Create the plot with a smoothed line for the rolling average
p1 <- ggplot(data = merged_data_with_met, aes(x = ymd_hms(hour_of_obs), y = fluxL_umolm2sec)) +
  geom_point(alpha = 0.2, aes(color = method)) +  # Raw data points
  #geom_smooth(method = "loess", span = 0.3, color = "black", se = F) +  # Loess smooth line for raw data
  #geom_smooth(aes(y = rolling_avg), method = "loess", span = 0.3, color = "black", se = F, size = 1) +  # Smoothed line for rolling average
  geom_line(aes(y = rolling_avg), color = "black", size = 1) +  # Rolling average as a line
  theme_classic() +
  theme(legend.position = "none") +
  facet_wrap(~stand * method, ncol = 2) +
  scale_color_manual(values = c("#4C9F70", "#B0B0B0"))+
  xlab("")+ylab(expression(paste(CO[2], " Flux, ", mu, "mol ", m^-2, s^-1)))

# Print the plot
p1


# p1<-ggplot(data=merged_data_with_met, aes(x=ymd_hms(hour_of_obs), y=fluxL_umolm2sec))+
#   geom_point(alpha=0.2, aes(color=method))+
#   geom_smooth(method = "loess", span = 0.3, color="black", se=F)+
#   theme_classic()+
#   theme(legend.position = "none")+
#   facet_wrap(~stand*method, ncol=2)+
#   scale_color_manual(values=c("#4C9F70", "#B0B0B0"))+
#   scale_x_datetime(limits = x_limits)  # Use scale_x_datetime() for POSIXct limits

p2<-ggplot(data=merged_data_with_met, aes(x=ymd_hms(hour_of_obs), y=s10t))+
  geom_line(color="#A14A3F", size=1)+
  #geom_smooth(method = "loess", span = 0.3, se=F, color="black")+
  theme_classic()+
  xlim(min(merged_data_with_met$hour_of_obs[which(merged_data_with_met$method=="fluxbot")], na.rm=T), 
       max(merged_data_with_met$hour_of_obs[which(merged_data_with_met$method=="fluxbot")], na.rm=T))+
  xlab("")+ylab(expression(paste("Soil Temp (", degree, "C)")))
p2

(p1 / (p2 | plot_spacer())  + plot_layout(widths = c(1, 0.5),
                                          heights=c(1.5, 0.5)))









# First, count chambers for each combination of hour, method, and stand
chamber_counts <- merged_data %>%
  group_by(hour_of_obs, method, stand) %>%
  summarise(n_chambers = n_distinct(id), .groups = 'drop') %>%  # Count distinct chambers per group
  ungroup()

# Then, filter for hours where both methods have at least 6 chambers in each stand
valid_hours <- chamber_counts %>%
  group_by(hour_of_obs) %>%
  # Ensure there are at least 6 chambers for each combination of method and stand
  filter(all(c("fluxbot", "autochamber") %in% method) &         # Both methods must be present
           all(c("healthy", "unhealthy") %in% stand) &            # Both stands must be present
           all(n_chambers[method == "fluxbot" & stand == "healthy"] >= 5) &
           all(n_chambers[method == "fluxbot" & stand == "unhealthy"] >= 5) &
           all(n_chambers[method == "autochamber" & stand == "healthy"] >= 5) &
           all(n_chambers[method == "autochamber" & stand == "unhealthy"] >= 5)) %>%
  ungroup() %>%
  distinct(hour_of_obs)  # Keep only distinct valid hours


# 
# # First, count chambers for each combination of hour, method, and stand
# chamber_counts <- merged_data %>%
#   group_by(hour_of_obs, method, stand) %>%
#   summarise(n_chambers = n_distinct(id), .groups = 'drop') %>%
#   ungroup()
# 
# # Then, filter for hours where the total number of chambers (across all methods and stands) is at least 20
# valid_hours <- chamber_counts %>%
#   group_by(hour_of_obs) %>%
#   summarise(total_chambers = sum(n_chambers)) %>%
#   filter(total_chambers >= 20) %>%
#   ungroup() %>%
#   distinct(hour_of_obs)  # Keep only distinct valid hours
# 
# # Filter the original dataset to only include the valid hours
 filtered_data <- merged_data %>%
   filter(hour_of_obs %in% valid_hours$hour_of_obs)


filtered_data <- filtered_data %>%
  filter(!is.na(hour_of_obs)) 

ggplot(filtered_data, aes(x=fluxL_umolm2sec, y=as.factor(date), color=method))+
  geom_point()+
  geom_boxplot()+
  facet_wrap(~stand)

ggplot(filtered_data, aes(x=fluxL_umolm2sec, fill=method))+
  geom_density(alpha=0.7)+
  geom_jitter(aes(y=method), alpha=0.1, height=0.2)+
  facet_wrap(~stand)+
  scale_fill_manual(values = c("#4C9F70", "#B0B0B0"))


library(ggplot2)


# Calculate the means of fluxL_umolm2sec for each method and stand
means_data <- filtered_data %>%
  group_by(stand, method) %>%
  summarize(mean_flux = mean(fluxL_umolm2sec, na.rm = TRUE))

# Plot with vertical lines for means
ggplot(filtered_data, aes(x = fluxL_umolm2sec, fill = method)) +
  geom_density(alpha = 0.7) +
  geom_jitter(aes(y = method), alpha = 0.1, height = 0.2) +
  geom_vline(data = means_data, aes(xintercept = mean_flux, color = method), linetype = "dashed", size = 0.8) +  # Add vertical dashed lines for means
  facet_wrap(~stand, ncol = 2) +
  theme_minimal() + ylab("")+
  theme(strip.text.y = element_text(angle = 0), legend.position = "bottom") +
  scale_fill_manual(values = c("#4C9F70", "#B0B0B0"))+
  scale_color_manual(values = c("#4C9F70", "#B0B0B0"))




# Calculate the means of fluxL_umolm2sec for each method
means_data <- filtered_data %>%
  group_by(method) %>%
  summarize(mean_flux = mean(fluxL_umolm2sec, na.rm = TRUE))

# Plot with vertical lines for means
ggplot(filtered_data, aes(x = fluxL_umolm2sec, fill = method)) +
  geom_density(alpha = 0.7) +
  geom_jitter(aes(y = method), alpha = 0.1, height = 0.2) +
  geom_vline(data = means_data, aes(xintercept = mean_flux, color = method), linetype = "dashed", size = 0.8) +  # Add vertical dashed lines for means
  theme_minimal() + ylab("")+
  theme(strip.text.y = element_text(angle = 0), legend.position = "bottom")  +
  scale_fill_manual(values = c("#4C9F70", "#B0B0B0"))+
  scale_color_manual(values = c("#4C9F70", "#B0B0B0"))+
  xlab(expression(paste(CO[2], " Flux, ", mu, "mol ", m^-2, s^-1)))







