#namanpaul
#script for extracting MERRA FLX and SLV data


#load the packages
library(ncdf4)
library(dplyr)
library(sp)
library(raster)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(foreach)


#set the working directory to the current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

current_wd <- getwd()



# input -------------------------------------------------------------------

#flx i/p path
flx_input_path <- paste0(current_wd, '/input/FLX/')

#slv i/p path
slv_input_path <- paste0(current_wd, '/input/SLV/')

#MERRA o/p files
MERRA_output_path <- paste0(current_wd, '/output/multiple/')


#set year
year_val <- 2019


# function definition -----------------------------------------------------

process_MERRA <- function(flx_input_path, slv_input_path, year_val){


  #create the date range
  date_range <- seq(as.Date(paste(year_val,"-01-01", sep = "")),
                    as.Date(paste(year_val,"-01-02", sep = "")),
                    by=1)
  
  #change the dateformat to match the filenames
  date_range <- format(date_range, "%Y%m%d")
  
  #foreach loop
  foreach(i = seq_along(date_range)) %do% {
    #printing to keep a check of how many files have been processed
    print(date_range[i])
    
    date_char_match <- as.character(date_range[i])
    
    file_name_single_day <- list.files(path = flx_input_path, pattern = date_char_match)
  
    #read in the FLX i/p
    ncin_flx <- nc_open(filename = paste0(flx_input_path,file_name_single_day))
    
    #get the vars
    flx_vars <- ncvar_get(ncin_flx,
                          "PBLH", verbose = F) %>% 
      cbind(ncvar_get(ncin_flx,
                      "SPEED", verbose = F)) %>% 
      cbind(ncvar_get(ncin_flx,
                      "SPEEDMAX", verbose = F)) %>% 
      cbind(ncvar_get(ncin_flx,
                      "TLML", verbose = F)) %>% 
      cbind(ncvar_get(ncin_flx,
                      "ULML", verbose = F)) %>% 
      cbind(ncvar_get(ncin_flx,
                      "VLML", verbose = F)) %>% 
      cbind(ncvar_get(ncin_flx,
                      "QLML", verbose = F))
    
    #convert the matrix to a dataframe and left_join with the lonlat
    flx_df <- as_data_frame(flx_vars)
    
    #set colnames
    flx_colnames <- c("PBLH","SPEED","SPEEDMAX",
                      "TLML","ULML","VLML","QLML")
    
    #set colnames
    colnames(flx_df) <- flx_colnames
    
    #Extract lon, lat, time
    #Longitude
    longitude <- ncvar_get(ncin_flx, "lon", verbose = F)
    
    #Latitude
    latitude <- ncvar_get(ncin_flx, "lat", verbose = F)
    
    #time
    time <- ncvar_get(ncin_flx, "time", verbose = F)
    
    #close
    nc_close(ncin_flx)
    #create a df of the lonlat
    lonlattime <- expand.grid(longitude,latitude,time)
    
    #lonlat_df
    lonlattime_df <- lonlattime %>% 
      dplyr::rename(longitude = Var1,
                    latitude = Var2,
                    time = Var3)
    
    
    #lonlattimedf, change the hours
    lonlattime_df <- lonlattime_df %>% 
      dplyr::mutate(hhmm = 100*floor((time+30)/60)+30,
                    hhmm1 = stringi::stri_pad_left(str=hhmm, 4, pad="0"),
                    ACQ_DATE = date_range[i])
    
    #POSIXct
    TZ_lonlattime <- within(lonlattime_df,
                            { UTC_time = as.POSIXct(strptime(paste(ACQ_DATE, hhmm1),
                                                             "%Y%m%d %H%M")) })
    
    #now we reduce the 6 hours to get CST
    TZ_lonlattime$CST_time <- TZ_lonlattime$UTC_time - hours(6)
    
    #now modify the ACQ_DATE
    TZ_lonlattime$ACQ_DATE <-lubridate::date(TZ_lonlattime$CST_time)
    
    #remove the cols
    TZ_lonlattime <- TZ_lonlattime %>% 
      dplyr::select(longitude, latitude, ACQ_DATE:CST_time)
    
    #binds_cols of the lonlatdf_repeated with slv_df
    flx_data <- dplyr::bind_cols(TZ_lonlattime,
                                 flx_df)
    
    #remove unneeded vars
    flx_data <- flx_data %>% 
      dplyr::select(-UTC_time, -CST_time)
    
    ###################SLV
    
    
    file_name_single_day <- list.files(path = slv_input_path, pattern = date_char_match)
    
    
    #read in the slv
    ncin_slv <- nc_open(filename = paste0(slv_input_path, file_name_single_day))
    
    #get slv vars data
    slv_vars <- ncvar_get(ncin_slv,
                          "U850", verbose = F) %>% 
      cbind(ncvar_get(ncin_slv,
                      "V850", verbose = F)) %>% 
      cbind(ncvar_get(ncin_slv,
                      "U500", verbose = F)) %>% 
      cbind(ncvar_get(ncin_slv,
                      "V500", verbose = F)) %>% 
      cbind(ncvar_get(ncin_slv,
                      "U250", verbose = F)) %>% 
      cbind(ncvar_get(ncin_slv,
                      "V250", verbose = F)) %>% 
      cbind(ncvar_get(ncin_slv,
                      "U50M", verbose = F)) %>% 
      cbind(ncvar_get(ncin_slv,
                      "V50M", verbose = F)) %>% 
      cbind(ncvar_get(ncin_slv,
                      "U10M", verbose = F)) %>% 
      cbind(ncvar_get(ncin_slv,
                      "V10M", verbose = F)) %>% 
      cbind(ncvar_get(ncin_slv,
                      "U2M", verbose = F)) %>% 
      cbind(ncvar_get(ncin_slv,
                      "V2M", verbose = F)) 
    
    
    #convert the matrix to a dataframe and left_join with the lonlat
    slv_df <- as_data_frame(slv_vars)
    
    #set colnames
    slv_colnames <- c("U850","V850","U500","V500","U250","V250",
                      "U50M","V50M","U10M","V10M","U2M","V2M")
    
    #set colnames
    colnames(slv_df) <- slv_colnames
    
    #Extract lon, lat, time
    #Longitude
    longitude <- ncvar_get(ncin_slv, "lon", verbose = F)
    
    #Latitude
    latitude <- ncvar_get(ncin_slv, "lat", verbose = F)
    
    #time
    time <- ncvar_get(ncin_slv, "time", verbose = F)
    
    nc_close(ncin_slv)
    
    #create a df of the lonlat
    lonlattime <- expand.grid(longitude,latitude,time)
    
    #lonlat_df
    lonlattime_df <- lonlattime %>% 
      dplyr::rename(longitude = Var1,
                    latitude = Var2,
                    time = Var3)
    
    
    #lonlattimedf, change the hours
    lonlattime_df <- lonlattime_df %>% 
      dplyr::mutate(hhmm = 100*floor((time+30)/60)+30,
                    hhmm1 = stringi::stri_pad_left(str=hhmm, 4, pad="0"),
                    ACQ_DATE = date_range[i])
    
    #POSIXct
    TZ_lonlattime <- within(lonlattime_df,
                            { UTC_time = as.POSIXct(strptime(paste(ACQ_DATE, hhmm1),
                                                             "%Y%m%d %H%M")) })
    
    #now we reduce the 6 hours to get CST
    TZ_lonlattime$CST_time <- TZ_lonlattime$UTC_time - hours(6)
    
    #now modify the ACQ_DATE
    TZ_lonlattime$ACQ_DATE <-lubridate::date(TZ_lonlattime$CST_time)
    
    #remove the cols
    TZ_lonlattime <- TZ_lonlattime %>% 
      dplyr::select(longitude, latitude, ACQ_DATE:CST_time)
    
    #binds_cols of the lonlatdf_repeated with slv_df
    slv_data <- dplyr::bind_cols(TZ_lonlattime,
                                 slv_df)
    
    #remove unneeded vars
    slv_data <- slv_data %>% 
      dplyr::select(-UTC_time, -CST_time) %>% 
      rename(slv_longitude = longitude,
             slv_latitude = latitude,
             slv_ACQ_date = ACQ_DATE)
    
    #join the flx and slv data
    MERRA_data <- dplyr::bind_cols(flx_data, slv_data)
    
    #remove the duplicate cols
    MERRA_data <- MERRA_data %>% 
      dplyr::select(-slv_longitude, -slv_latitude, -slv_ACQ_date)
    
    #setting up an additional counter
    counter = 0
    
    #a vector of dates
    dates_MERRA <- unique(MERRA_data$ACQ_DATE)
    
    
    foreach(k = dates_MERRA) %do% {
      #create a smaller df to operate on
      singledate <- MERRA_data %>% 
        dplyr::filter(ACQ_DATE == k)
      
        #counter
        counter = counter+1
      
         #save the data
          save(singledate,
               file = paste0(MERRA_output_path,'/','MERRA_data_',counter,"_",k,'.RData'))
          
    }
    
  }
  
}



#execute
for(year_val in 2019){
  
  process_MERRA(flx_input_path, slv_input_path, year_val)
  
}

