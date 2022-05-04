#namanpaul
#binding the MOD-MYD 

#load the packages
library(dplyr)
library(foreach)

#aod i/p data path Switch for Terra/ Aqua (MOD/MYD)
#set the working directory to the current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

current_wd <- getwd()

#set satellite names and year
satellite_name <- 'MYD'
year_val <- 2019

#set the input and output paths
input_path <- paste0(current_wd, '/output/',satellite_name,'_multiple/')
output_path <- paste0(current_wd, '/output/',satellite_name,'_bound/')


#defining the function
bind_AOD <- function(input_path, bound_path, year_val){
  #create the date range in julian days
  date_range <- format(seq(as.Date(paste(year_val,"-01-01", sep = "")),
                           as.Date(paste(year_val,"-01-02", sep = "")),
                           by=1), "%Y-%m-%d")
  #input files list
  foreach(i = seq_along(date_range)) %do% {
    
    #print the date
    print(date_range[i])
    
    #get the list of files for the day
    filelist_forday <- 
      list.files(path = input_path, pattern = paste("*",date_range[i],sep="_"), full.names = T, recursive = T)
    
    for(k in seq_along(filelist_forday)) {
      
      #load all the files for the day
      load(filelist_forday[k])
      
      #assign a name
      assign(paste("dfs", k, sep="_"), singledate)
      
      #list of dataframes
      list_df <- lapply(ls(pattern="dfs+"), function(x) get(x))
      
      #binding the rows
      bound_data <- dplyr::bind_rows(list_df)
      
      #distinct rows only
      bound_data <- bound_data %>% 
        distinct()
      
    }
    #save the RData
    save(bound_data,
         file = paste0(output_path,'/',satellite_name,'_data_',date_range[i],'.RData'))
  }
  
}


#running the function
for(year_val in 2019){
  bind_AOD(input_path, bound_path, year_val)
}
