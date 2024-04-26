# Workflow for processing raw LGR/Picarro data and calculating
# chamber flux on different dates at different soil chambers

library(tidyverse)
library(lubridate)
library(data.table)

# Load local functions
#file_sources <- list.files("R/functions", pattern="*.R", full.names = TRUE)
#sapply(file_sources, source, .GlobalEnv)
source("/Users/jongewirtzman/Google Drive/Research/Envirobotics/fluxbot_lgr_test/calculate_chamber_flux_fluxbot.R")
#source("/Users/jongewirtzman/Google Drive/Research/Blueflux/Blueflux Tree Methane/Blueflux - Yale Ground Team - Shared/flux_code/calculate_chamber_flux_lgr2.R")

date_time <- read.csv("/Users/jongewirtzman/Google Drive/Research/Envirobotics/fluxbot_lgr_test/times.csv")
names(date_time)<-c("UniqueID", "start_time", "end_time")

#fluxbot
data_path <-  "/Users/jongewirtzman/Google Drive/Research/Envirobotics/fluxbot_lgr_test/fluxbot20.csv"


####NEED TO UPDATE VOLUME CALC#####
date_time$rep_vol_L<-1

#%>%filter(!is.na(start_time))

# Flux processing settings - change these for your application
init <- list()
init$analyzer <- "fluxbot" # can be "picarro" or "lgr"
init$data_path <- data_path # path to analyzer files
# init$startdelay <- 20 # 20s delay for Picarro
init$fluxend   <- 4 # minutes to include data after start (will ignore if end times are in date_time)
init$surfarea  <- 81  #cm^2
init$vol_system <- 768 #cm^3
init$plotslope <- 1 # make a plot with the slope: 0 = off
# init$outputfile <- 1 # write an output file: 0 = off
# init$outfilename <- "blueflux_lgr2_ch4.csv"


# Calculate soil CO2 & CH4 flux for each measurement date & replicate

flux_data <- calculate_chamber_flux(raw_files, date_time, init)          

# AFTER THIS YOU CAN EDIT TO MERGE WHATEVER OTHER DATA YOU WANT

merged_data<-left_join(date_time, flux_data)
write_csv(merged_data, "lgrfluxestimates_data.csv")
# write_csv(merged_data, "botfluxestimates_data.csv")
