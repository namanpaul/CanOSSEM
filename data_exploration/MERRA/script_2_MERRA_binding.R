#namanpaul
#bind MERRA data

#load the packages
library(dplyr)
library(foreach)
library(stringr)

#set the working directory to the current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

current_wd <- getwd()

#MERRA multiple path
MERRA_multiple_path <- paste0(current_wd, '/output/multiple/')

#MERRA bound path
MERRA_bound_path <- paste0(current_wd, '/output/bound/')

#define year
year_val <- 2019

# define function ---------------------------------------------------------
bind_MERRA <- function(MERRA_multiple_path, MERRA_bound_path, year_val){
  
  #create the date range in julian days
  date_range <- format(seq(as.Date(paste(year_val,"-01-01", sep = "")),
                           as.Date(paste(year_val,"-01-02", sep = "")),
                           by=1), "%Y-%m-%d")
  #input files list
  foreach(i = seq_along(date_range)) %do% {
    
    #print the date
    print(date_range[i])
    
    #list the files
    filelist_forday <- 
      list.files(path = MERRA_multiple_path,
                 pattern = paste("*",date_range[i],sep="_"), full.names = T, recursive = T)
    
    #for loop iterating over the days
    for(k in seq_along(filelist_forday)) {
      
      #load the files for the day
      load(filelist_forday[k])
    
      #assigning names to dfs
      assign(paste("dfs", k, sep="_"), singledate)
      
      #list of dataframes
      list_df <- lapply(ls(pattern="dfs+"), function(x) get(x))
      
      #binding the rows of the dfs for the day
      MERRA_bound <- dplyr::bind_rows(list_df)
      
    }
    
    #saving the MERRA data bound
    save(MERRA_bound,
        file = paste0(MERRA_bound_path,'/','MERRA_data_bound_',date_range[i],'.RData'))
  }
    
}


for(year_val in 2019){
  bind_MERRA(MERRA_multiple_path, MERRA_bound_path, year_val)
}

