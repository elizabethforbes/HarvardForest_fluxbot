# This function reads a raw text output file from a Los Gatos Research (LGR) Greenhouse Gas Analyzer (GGA)
# and exports the measurement times and CO2 and CH4 concentration data. 
#

format_LGR_output <- function(init){

  ## With this chunk of script we can make one data table from a list of many. It reads all files from a folder and their subfolders and merge them.
  # It does not read compressed files
  # LGR sometimes saves files in .zip. The first part of this chunk is for decompress all .zip files from a given folder
  
  # List all .zip files including sub-folders
  list_of_zip_files <- list.files(path = init$data_path, recursive=TRUE, 
                                  pattern="\\.zip$", full.names=TRUE)
  
  # Decompress all .zip files from a given folder and subfolders
  # Make a copy of the uncompressed folder in the same folder as .zip was
  sapply(list_of_zip_files, 
         function(i) unzip(i, exdir=gsub("\\.zip$", "", i))) 
  
  # List all txt files, merge them in one single file and create a variable 
  # with the name of each original file 
  list_of_txt_files <- list.files(path = init$data_path, recursive = TRUE,
                                  pattern = "\\.txt$", full.names = T)  #with full.name=F the function save the name of each file instead of the name of each path. This is useful for the idcol in the next section 
  
  # Read all the files and create a Path column to store filenames
  LGR_data <- rbindlist(sapply(list_of_txt_files, fread, simplify = FALSE),
                      use.names = TRUE, idcol = "Path", fill=T)

  #convert table to dataframe format
  LGR_data <- as.data.frame(LGR_data) 
  
  # remove duplicated columns
  LGR_data <- LGR_data[,!duplicated(colnames(LGR_data))]
  
  # Format LGR time
  LGR_data = LGR_data %>%
    mutate(times = mdy_hms(Time)) %>%
    rename(CH4 = `[CH4]d_ppm`,
           CO2 = `[CO2]d_ppm`) %>%
    select(times, CO2, CH4)
  
  return(LGR_data)
}
