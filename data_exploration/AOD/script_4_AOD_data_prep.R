#namanpaul
#binding the MOD-MYD data extracted

#load the packages
library(dplyr)
library(foreach)

#aod i/p data path Switch for Terra/ Aqua (MOD/MYD)
#set the working directory to the current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

current_wd <- getwd()

#set satellite names and year
#satellite_name <- 'MOD'
year_val <- 2019


# input -------------------------------------------------------------------

#file extract path
MOD_bound_path <- paste0(current_wd, '/output/MOD_bound/')
MYD_bound_path <- paste0(current_wd, '/output/MYD_bound/')

#output path
AOD_data_output_path <- paste0(current_wd, '/output/AOD_data/')


# function-definition -----------------------------------------------------
#function definition
bind_AOD_data <- function(MOD_bound_path, MYD_bound_path, year_val){
  
  #create the date range in julian days (these can be changed for the whole year around)
  date_range <- format(seq(as.Date(paste(year_val,"-01-01", sep = "")),
                           as.Date(paste(year_val,"-01-02", sep = "")),
                           by=1), "%Y-%m-%d")
  
  foreach(i = seq_along(date_range)) %do% {
    
    #printing to keep a check of how many files have been processed
    
    MOD_file <- list.files(path=paste0(MOD_bound_path),
                           pattern=paste0("MOD_data_",date_range[i],".RData"), full.names=TRUE, recursive=TRUE)
    
    MYD_file <- list.files(path=paste0(MYD_bound_path),
                           pattern=paste0("MYD_data_",date_range[i],".RData"), full.names=TRUE, recursive=TRUE)
    
    #MOD dataframe
    if(!purrr::is_empty(MOD_file)){
      print(paste0(date_range[i]," file exists"))
      
      #load the file
      load(MOD_file)
      
      MOD_data <- bound_data %>% 
        distinct()
    }
    else{
      print(paste0(date_range[i]," MOD data doesn't exist"))
    }
    
    
    #MYD dataframe
    if(!purrr::is_empty(MYD_file)){
      print(paste0(date_range[i]," file exists"))
      
      #load the file
      load(MYD_file)
      
      MYD_data <- bound_data %>% 
        distinct()
    }
    else{
      print(paste0(date_range[i]," MYD data doesn't exist"))
    }
    
    
    #bind the dataframes  
    daily_AOD_data <- bind_rows(MOD_data, MYD_data)
    
    #make sure we get unique data rows only
    clean_AOD_data <- daily_AOD_data %>%
      distinct()
    
    #reorganize cols
    clean_AOD_data <- clean_AOD_data %>% 
      dplyr::select(-x,-y) %>% 
      dplyr::select(longitude, latitude, ACQ_DATE, satellite_name, everything())
    
    save(clean_AOD_data,
         file = paste0(AOD_data_output_path,'/','AOD_data_',date_range[i],'.RData'))
  }
}



# call function -----------------------------------------------------------
#execute
for(year_val in 2019) {
  
  bind_AOD_data(MOD_bound_path, MYD_bound_path, year_val)
  
}
  


