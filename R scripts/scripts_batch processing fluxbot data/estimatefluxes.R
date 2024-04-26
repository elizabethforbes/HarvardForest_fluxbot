# function to convert iterative observation intervals of co2 accumulation over 4min time periods (in concentration, ppm) to units of co2 mass accumulation; the function uses ideal gas laws and a humidity correction to convert from ppm to kg co2. Once converted, the function then takes both a linear and quadratic regression of each accumulation interval, and uses the resultant slopes (beta values) to calculate a rate of increase in CO2 over time, per unit area.

# currently the function requires an input of the time series dataframe itself, a custom intervals dataframe (two columns, one labeled "Start" and one labeled "End" with each row indicating the ideal start and end of each interval in the time series df in POSIXct format), and an indication of the method (e.g. is it "LGR" or "fluxbot" generated data).

calculate_regressions_custom_intervals <- function(time_series_df, custom_intervals_df, 
                                                   method, volume, area) {
  
  # Remove rows with NAs
  time_series_df <- time_series_df[complete.cases(time_series_df), ]
  
  # Extract timestamp and data columns from the time series dataframe
  timestamp_col <- as.POSIXct(time_series_df$Time, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
  data_col <- time_series_df$co2     
  
  # Initialize an empty dataframe to store the results
  results_df <- data.frame(
    start_timestamp = character(),
    end_timestamp = character(),
    linear_beta = numeric(),
    quadratic_beta = numeric()
  )
  
  # Quality control thresholds: if an interval 'fails' any of these it is skipped and excluded from the final output
  diff_threshold <- 5 # difference in co2 concentration delta from start-end needs to be greater than 5ppm
  max_threshold <- 3000 # eliminate any intervals that contain co2 concentrations greater than 3k ppm
  length_threshold <- 15 # the length of an interval needs to be at least 15 observations (40 obs per 4mins interval for fluxbot)
  
  # constants for conversion of concentration to mass:
  R_m = 8.314472  # Ideal gas constant for mass [(m3*Pa)/(K*mol)]
  R_specific <- 287.058 # specific gas constant for dry air (J/kg*K)
  mol_mass <- 28.9628 # molar mass dry air, g/mol
  volume <- volume # cm3; volume of fluxbot chamber, typically 768cm3 without additional tubing/attachments
  area <- area # cm2; area of soil that fluxbot collar encompasses, typically 81cm2
  g_per_mol = 44.009 # molar mass of CO2, g/mol
  
  # Loop through each row in the custom intervals dataframe to ID the start and end point of each interval
  for (i in seq_len(nrow(custom_intervals_df))) {
    # Extract start and end points for the current row
    start_point <- custom_intervals_df$Start[i]
    end_point <- custom_intervals_df$End[i]
    
    # Find the closest timestamp to start_point and end_point in the time series data itself:
    start_index <- which.min(abs(timestamp_col - start_point))
    end_index <- which.min(abs(timestamp_col - end_point))
    
    # Check if the closest start and end points are within 30 minutes
    if (abs(start_point - timestamp_col[start_index]) > 1800 || abs(end_point - timestamp_col[end_index]) > 1800) {
      cat("Skipping interval due to no close timepoints:", j, "\n")
      next  # Skip to the next iteration
    }
    
    # Extract the CO2 concentration data for each interval iteratively using the above timestamps:
    interval_data <- time_series_df$co2[start_index:end_index]
    interval_timestamp <- time_series_df$Time[start_index:end_index]
    # identify the interval pressure to run QAQC too:
    interval_pressure <- time_series_df$pressure[start_index:end_index]
    # determine total length of interval; reports in mins, so convert to sec:
    length_interval <- (as.numeric(time_series_df$Time[end_index] - time_series_df$Time[start_index]))*60
    
    # Quality control checks with printouts of reasons for excluding a given interval:
    if (any(is.na(interval_data))) {
      cat("Skipping interval due to NAs in the data:", i, "\n")
      next  # Skip to the next iteration
    }
    
    if (abs(interval_data[1] - interval_data[length(interval_data)]) < diff_threshold) {
      cat("Skipping interval due to extremely small delta:", i, "\n")
      next  # Skip to the next iteration
    }
    
    if (interval_data[length(interval_data)] - interval_data[1] < 0){
      cat("Skipping interval due to negative delta:", i, "\n")
      next
    }
    
    # if(max(interval_data) > max_threshold) {
      # cat("Skipping interval: concentration too high:", i, "\n")
      # next # Skip to the next iteration
    # }
    
    if(length(interval_data) < length_threshold) {
      cat("Skipping interval due to insufficient length:", i, "\n")
      next # Skip to the next iteration
    }
    
    # temporary fix for intervals with weird pressure data, March 4th 2024, 
    # while I figure out how to sub in good pressure data when needed
    # Implement quality control check for pressure data
    if (any(interval_pressure > 1050) || any(interval_pressure < 965)) {
      cat("Skipping interval due to error in pressure data:", i, "\n")
      next  # Skip to the next iteration
    }
    
    # Remove rows with specific value for co2 indicating a data transmission error, within the interval data
    interval_data <- interval_data[interval_data != 65535]
    interval_data <- interval_data[interval_data != 65533]
    
    # calculate rho_a, air density in kg/m3, including humidity:
    # first step, calculate saturation vapor pressure in hPa
    e_star <- 10*(0.61978 * exp((17.27 * time_series_df$tempC)[start_index:end_index]/
                                  (time_series_df$tempC[start_index:end_index] + 
                                     273.3))) # saturation vapor pressure, hPa
    # next: saturation density (kg water / kg air) given saturation vapor pressure (e_star) and observed air pressure
    # 0.62198 = ratio of molecular masses of water and dry air (18.01528 / 28.9645)
    x_s <- 0.62198 * e_star / (time_series_df$pressure[start_index:end_index] - e_star) 
    
    # next: calculate humidity ratio using saturation density and observed relative humidity:
    x <- x_s * time_series_df$humidity[start_index:end_index] / 100 # divide by 100 to convert to a fraction
    
    # finally, calculate the observed density of humidity-corrected air in the chamber:
    # first: density of air in kg/m3 using specific gas constant for dry air
    rho_d <- (time_series_df$pressure[start_index:end_index] * 100) / 
      (R_specific * (time_series_df$tempC[start_index:end_index] + 273.15)) # here converting T to Kelvin from Celcius
    # air density in kg/m3 with humidity correction as calculated in "x" above:
    # 1.609 = gas constant ratio between water vapor and air
    rho_a <- rho_d * (1+x) / (1 + 1.609 * x) 
    
    # convert (general) co2 in ppm to mol/m3
    mol_kg = 1/(mol_mass/1000) # mass of air in mol/kg
    # calculate mol of moist air per m3 in the chamber given previously-calculated moist air densities
    mol_m3 = mol_kg * rho_a # mass of moist air in chamber in mol/m3
    
    # convert (our observed) co2 in ppm to molar density using chamber volume:
    mol_gas_m3 <- mol_m3 * (interval_data/1000000) # returns mol/m3 (conversion from cm3 to m3 = 1,000,000)
    
    # convert molar concentration of CO2 into concentration in kg/m3
    kg_gas_m3 <- mol_gas_m3 * (g_per_mol/1000) # (conversion of g to kg = 1000)
    
    # convert to just mass in kg (volume reported in cm3, converting to m3 here, inverse conversion = 0.000001):
    kg_gas <- kg_gas_m3 * (volume * 0.000001) # returns kg of co2 in the chamber at each time point in interval
    
    # Linear regression
    linear_model <- lm(kg_gas ~ as.numeric(difftime(interval_timestamp, min(interval_timestamp), units = "secs")))
    linear_results <- summary(linear_model)
    linear_beta <- coef(linear_results)[2]  # Extract the beta coefficient; kg/s
    delta_kg_L = linear_beta*(length_interval) # units in kg
    delta_g_L = delta_kg_L*1000 # units in g
    delta_mol_L = delta_g_L / g_per_mol # units in mols
    fluxL = delta_kg_L / length_interval / (area*0.0001) # flux estimate in kg/m2/sec
    fluxL_umol = (delta_mol_L*1000000) / length_interval / (area * 0.0001) # flux estimate in umol/m2/sec
    
    # Quadratic regression
    quadratic_model <- lm(kg_gas ~ 
                            as.numeric(difftime(interval_timestamp, min(interval_timestamp), 
                                                units = "secs")) +
                            I(as.numeric(difftime(interval_timestamp, 
                                                  min(interval_timestamp), 
                                                  units = "secs"))^2))
    quadratic_results <- summary(quadratic_model)
    quadratic_beta <- coef(quadratic_results)[2]  # Extract the beta coefficient; kg/s
    delta_kg_Q  = quadratic_beta*(length_interval) # units in kg
    delta_g_Q = delta_kg_Q*1000 # units in g
    delta_mol_Q = delta_g_Q / g_per_mol # units in mols
    fluxQ = delta_kg_Q / length_interval / (area*0.0001) # flux estimate in kg/m2/sec
    fluxQ_umol = (delta_mol_Q*1000000) / length_interval / (area * 0.0001) # flux estimate in umol/m2/sec
    
    # Append the results to the dataframe
    results_df <- rbind(results_df, data.frame(
      start_timestamp = start_point,
      end_timestamp = end_point,
      hour_of_obs = round(end_point, "hour"),
      method = method,
      linear_beta = linear_beta, # kg/s
      quadratic_beta = quadratic_beta, # kg/s
      length_interval = length_interval, # total interval length in seconds
      delta_kg_L = linear_beta/length_interval, # units in kg
      delta_kg_Q  = quadratic_beta/length_interval, # units in kg
      fluxL_kgm2sec = fluxL, # flux estimate in kg/m2/sec
      fluxQ_kgm2sec = fluxQ, # flux estimate in kg/m2/sec
      fluxL_umolm2sec = fluxL_umol, # flux estimate in umol/m2/sec
      fluxQ_umolm2sec = fluxQ_umol  # flux estimate in umol/m2/sec
    ))
  }
  
  # Return the results
  return(results_df)
}
