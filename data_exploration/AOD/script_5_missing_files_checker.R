#namanpaul
#file exist checker

#load the packages
library(dplyr)
library(lubridate)

#i/p data path Switch for Terra/ Aqua (MOD/MYD)
#set the working directory to the current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

current_wd <- getwd()

#set satellite names and year
satellite_name <- 'MOD'
year_val <- 2019

#create the date range in julian days
date_range <- format(seq(as.Date(paste(year_val,"-01-01", sep = "")),
                         as.Date(paste(year_val,"-01-31", sep = "")),
                         by=1), "%Y-%m-%d")

#specify the extract path
multiple_extract_path <- paste0(current_wd, '/output/',satellite_name,'_multiple/')


#for loop
for(i in seq_along(date_range)){
  
  
  #list the files for the particular day
  file_name <- list.files(path = multiple_extract_path, pattern = date_range[i])
  
  #we should have atleast 1 file for each day, else we have missing data
  ifelse(length(file_name) >=1,
         print(paste0('File exists:',date_range[i])),
         print(paste0('File does not exist:',date_range[i])))
  
}
