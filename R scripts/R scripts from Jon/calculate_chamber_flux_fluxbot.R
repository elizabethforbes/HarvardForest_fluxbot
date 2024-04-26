# Process the raw output from the Los Gatos Research Greenhouse Gas Analyzer
# or the Picarro GasScouter analyzer into CO2 and CH4 flux
# according to the dates and start times provided.
#
#
# INPUT:
#  * raw_files: vector of raw LGR/Picarro file names 
#  * date_time: data frame with chamber IDs, start times, end times, reps
#  * init: list of flux processing settings (in workflow.R for transparency/access)
# 
# OUTPUT: 
#  * flux_clean: data frame with chamber ID, date info, CO2 fluxes and fit
#
# Requires libraries: dplyr, lubridate

calculate_chamber_flux <- function(raw_files, date_time, init){
  
  # Get # chambers to loop through
  totalreps <- length(unique(date_time$UniqueID))
  
  # System volume to total mol in the system (for ppm to mol conversion)
  # vol_system <- init$vol_system + date_time$rep_vol_L
  vol_system <- 768 #cm3
  surf_area <- 81 #cm2
  init$ntotal <- (739*vol_system)/(62.363577*298.15) # n = (P * vol_system) / (R * T) #LGR
  # init$ntotal <- (raw_files$pressure * vol_system)/(1.2041*raw_files$tempC) # Rho, air density in kg/m^3; fluxbot
  # P = mmHg (739 mmHg = 985.25 hPa)
  # R = gas constant in L*Torr*K^-1*m^-1
  # T = degrees in K
  
  ### Process the GHG analyzer files into CO2/CH4 flux by matching the chamber     
  ### measurements dates & times to the raw file information 
  
  # Create flux output storage
  empty_vector <- rep(NA, length = nrow(date_time))
  flux_data <- data.frame(jday = empty_vector, 
                          UniqueID = empty_vector, 
                          year = empty_vector,
                          CO2_flux = empty_vector, 
                          CO2_r2 = empty_vector, 
                          CO2_SE = empty_vector,
                          # CH4_flux = empty_vector, 
                          # CH4_r2 = empty_vector,
                          # CH4_SE = empty_vector,
                          CO2_p = empty_vector, 
                          # CH4_p = empty_vector)
  
  # Use the appropriate analyzer function to compile dates & files
  if(init$analyzer == "lgr") {
    # Read raw LGR analyzer CO2/CH4 concentration data
    conc_data <- format_LGR_output(init)
    flux_startdelay <- init$startdelay
    flux_end   <- init$fluxend
  } else if(init$analyzer == "picarro"){ 
    # Read raw Picarro analyzer CO2/CH4 concentration data
    conc_data <- format_Picarro_output(raw_files, date_time) 
    flux_startdelay <- init$startdelay
    flux_end   <- init$fluxend
  } else if(init$analyzer == "fluxbot"){ 
    # read raw fluxbot data (CO2 only)
    conc_data <- format_fluxbot_output(raw_files, date_time) 
    flux_startdelay <- init$startdelay
    flux_end   <- init$fluxend
  } 
  
  # Set up replicate & start time simplified table:
  # First use the end time col if available, or use end time length in init 
  if("end_time" %in% colnames(date_time)){
    rep_times <- date_time %>% 
      mutate(start = start_time+lubridate::seconds(flux_startdelay))
    rep_times$end = rep_times$end_time
  } else {
    rep_times <- date_time %>% #filter(date_time, dates == flux_dates[d]) %>%
      mutate(start = startx+lubridate::seconds(flux_startdelay),
             end = startx+lubridate::minutes(flux_end)) %>%
      filter(!is.na(start))
  }
  
  # Loop over unique measurements and calculate CO2 & CH4 flux
  for(c in c(1:nrow(rep_times))){
    
    # Index for storage: rep #
    rep_index = c
    
    # # Replicate & date bookkeeping
    flux_data$jday[rep_index]  <- lubridate::yday(date_time$dates[c])
    flux_data$year[rep_index] <- lubridate::year(date_time$dates[c])
    flux_data$UniqueID[rep_index]    <- rep_times$UniqueID[c]
    
    # If chamber measurement missing
    if(is.na(rep_times$start_time[c])){
      flux_data$CO2_r2[rep_index]   <- flux_data$CH4_r2[rep_index] <- NA
      flux_data$CO2_flux[rep_index] <- flux_data$CH4_flux[rep_index] <- NA
      flux_data$CO2_SE[rep_index]   <- flux_data$CH4_SE[rep_index] <- NA
      
    } else {
      
      # Find replicate start time and match to nearest time in raw concentration file
      conc_rep <- dplyr::filter(conc_data, 
                                times >= rep_times$start[c] & 
                                  times <= rep_times$end[c])
      #conc_rep$times<-as.numeric(round(conc_rep$times-min(conc_rep$times)))
      
      
      conc_rep_extend <- dplyr::filter(conc_data, 
                                times >= rep_times$start[c]-minutes(2) & 
                                  times <= rep_times$end[c]+minutes(2))
      conc_rep_extend$times<-as.numeric(round(conc_rep_extend$times-min(conc_rep_extend$times)))-120
      
      # Get flux period in seconds (so units work out)
      flux_seconds = lubridate::interval(conc_data$times[1],conc_rep$times)/lubridate::seconds(1)
      flux_seconds = round(flux_seconds-min(flux_seconds))
      
      # Fit linear model for x = seconds, y = CH4/CO2 concentration
      lm_CO2 <- lm(conc_rep$CO2 ~ flux_seconds)
      # lm_CH4 <- lm(conc_rep$CH4 ~ flux_seconds)
      CO2_sl <- summary(lm_CO2)$coefficients[2]
      # CH4_sl <- summary(lm_CH4)$coefficients[2]
      flux_data$CO2_r2[rep_index] <- summary(lm_CO2)$r.squared
      # flux_data$CH4_r2[rep_index] <- summary(lm_CH4)$r.squared
      flux_data$CO2_SE[rep_index] <- (coef(summary(lm_CO2))[, "Std. Error"][2]*init$ntotal[c])#/init$surfarea
      # flux_data$CH4_SE[rep_index] <- (coef(summary(lm_CH4))[, "Std. Error"][2]*init$ntotal[c])#/init$surfarea
      flux_data$CO2_p[rep_index] <- summary(lm_CO2)$coefficients[2,4]
      # flux_data$CH4_p[rep_index] <- summary(lm_CH4)$coefficients[2,4]
      
      # Calculate CO2/CH4 chamber flux in umol/(m^2 * s)
      # V_CO2/V_T = n_CO2/n_total, n_CO2 = (ppm*10^-6) * n_total
      flux_data$CO2_flux[rep_index] <- (CO2_sl*init$ntotal[c])/surf_area 
      # flux_data$CH4_flux[rep_index] <- (CH4_sl*init$ntotal[c])/init$surfarea 
      
      # Make plots of CO2_conc/CH4_conc vs time to visually inspect
      # if(init$plotslope == 1){
      #   par(mfrow=c(1,2))
      #   plot(conc_rep_extend$CO2~conc_rep_extend$times,
      #        main=paste(" UniqueID: ",flux_data$UniqueID[c],sep=""))
      #   points(flux_seconds-min(flux_seconds), conc_rep$CO2, col="red")
      #   abline(lm_CO2, col="blue")
        # plot(conc_rep_extend$CH4~conc_rep_extend$times,
             # main=paste(" UniqueID: ",flux_data$UniqueID[c],sep=""))
        # points(flux_seconds-min(flux_seconds), conc_rep$CH4, col="red")
        # abline(lm_CH4, col="blue")
        
        print(c)
        
        
      }
    } # end else.if start time exists
  } #end chamber loop
  
  # Write .csv file?
 # if(init$outputfile == 1){
#    write_csv(flux_data, paste0("output/",init$outfilename))
 # }
  
  # OUTPUT data frame back to workflow
  return(flux_data)
}
