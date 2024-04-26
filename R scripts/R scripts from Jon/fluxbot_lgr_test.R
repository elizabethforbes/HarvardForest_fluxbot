library(ggplot2)

setwd("/Users/jongewirtzman/Google Drive/Research/Envirobotics/fluxbot_lgr_test")
fluxbot<-read.csv("fluxbot_co2 - fluxbot_20.csv")
fluxbot_co2 <- unlist(strsplit(gsub("\\[|\\]", "", fluxbot$co2), ","), use.names = FALSE)
fluxbot_co2 <- as.numeric(fluxbot_co2)
fluxbot_co2[which(fluxbot_co2==65535)]<-NA

fluxbot_time <- unlist(strsplit(gsub("\\[|\\]", "", fluxbot$device.timestamps), ","), use.names = FALSE)

fluxbot_df<-as.data.frame(cbind(fluxbot_time, fluxbot_co2))

fluxbot_df$fluxbot_time<-as.numeric(fluxbot_df$fluxbot_time)
fluxbot_df$fluxbot_co2<-as.numeric(fluxbot_df$fluxbot_co2)
fluxbot_df$fluxbot_time <- lubridate::ymd_hms(as.POSIXct(fluxbot_df$fluxbot_time, origin="1970-01-01", tz="UTC"))

fluxbot_df<-fluxbot_df[which(fluxbot_df$fluxbot_time>lubridate::ymd_hms("2023-10-11 12:00:00")),]

ggplot(aes(y=fluxbot_co2, x=fluxbot_time), data=fluxbot_df)+
  geom_point()+ylim(400,700)

x <- 1  # replace with the number of days you want
# Define the start time
start_time <- as.POSIXct("2023-10-11 13:56:00", tz="UTC")
# Calculate the end time based on x days
end_time <- start_time + (x * 24 * 3600) - 3600  # subtracting 3600 seconds to not exceed x days
# Create a sequence of date-times
start_times <- seq(start_time, end_time, by="1 hour")
print(start_times)
end_times<-start_times+240

times<-data.frame(start_times, end_times)

write.csv(times, "times.csv")
write.csv(fluxbot_df, "fluxbot20.csv")
