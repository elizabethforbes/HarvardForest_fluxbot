convert_co2_to_mass <- function(time_series_df, start_timestamp, end_timestamp) {
  # Constants
  R_specific <- 287.058 # Specific gas constant for dry air (J/kg*K)
  mol_mass_co2 <- 44.009 # Molar mass of CO2 (g/mol)
  
  # Extract CO2 concentration data within the specified interval
  interval_data <- subset(time_series_df, Time >= start_timestamp & Time <= end_timestamp)$co2
  
  # Extract environmental parameters for the interval
  temperature_C <- subset(time_series_df, Time >= start_timestamp & Time <= end_timestamp)$tempC
  pressure_hPa <- subset(time_series_df, Time >= start_timestamp & Time <= end_timestamp)$pressure
  humidity_percent <- subset(time_series_df, Time >= start_timestamp & Time <= end_timestamp)$humidity
  volume_cm3 <- 768 #cm3
  
  # Saturation vapor pressure calculation
  e_star <- 10 * (0.61978 * exp((17.27 * temperature_C) / (temperature_C + 273.3))) # Saturation vapor pressure (hPa)
  
  # Saturation density calculation
  x_s <- 0.62198 * e_star / (pressure_hPa - e_star)
  
  # Humidity ratio calculation
  x <- x_s * humidity_percent / 100
  
  # Dry air density calculation
  rho_d <- (pressure_hPa * 100) / (R_specific * (temperature_C + 273.15))
  
  # Air density with humidity correction
  rho_a <- rho_d * (1 + x) / (1 + 1.609 * x)
  
  # Convert CO2 concentration from ppm to mol/m3
  mol_kg <- 1 / (28.9628 / 1000) # Mass of air in mol/kg (dry air)
  mol_m3 <- mol_kg * rho_a # Mass of moist air in chamber in mol/m3
  mol_gas_m3 <- mol_m3 * (interval_data / 1000000) # CO2 concentration in mol/m3
  
  # Convert molar concentration of CO2 into concentration in kg/m3
  kg_gas_m3 <- mol_gas_m3 * (mol_mass_co2 / 1000) # Convert from g to kg
  
  # Convert to mass in kg
  mass_kg <- kg_gas_m3 * (volume_cm3 * 0.000001) # Convert volume from cm3 to m3
  
  return(mass_kg)
}
