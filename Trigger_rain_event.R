rm(list=ls())

libs <- c("dplyr", "tidyr", 
          "ggplot2", "ggpubr",
          "ncdf4", "raster", "rgdal", 
          "lubridate", 
          "rgeos", "smoothr", "sf",
          "reshape")

install.libraries <- function(lib=NULL){
  new <- lib[!(lib %in% installed.packages()[, "Package"])]
  if (length(new)){   
    install.packages(new, dependencies = TRUE)
  }
} 

load.libraries <- function(lib=NULL){
  sapply(libs, require, character.only = TRUE)
}

install.libraries(libs)
load.libraries(libs)




### what triggered these rainfall events?
str(seasonal_break_day_year)
#seasonal_break_day_year[5:52] <- lapply(seasonal_break_day_year[5:52], as.double) 

seasonal_break_day_year_narrow <- gather(seasonal_break_day_year, 
                                         year, day_of_year, 
                                         Year_1971 : Year_2018 )
year_input_rain = 1971
day_of_year_rain = 110

daily_rain <- brick(
  paste("daily_rain/",
        year_input_rain, ".daily_rain.nc", sep = ""),varname = "daily_rain")

#crop to a fix area
daily_rain_crop <- crop(daily_rain, site)

#only use a few days
daily_rain_crop_subset_day <- subset(daily_rain_crop, 61:212) #pull out the 1 March: 30 July 

#Add the moving window avearge of 7 days ? 
seasonal_break_rainfall_MovMean7 <- calc(daily_rain_crop_subset_day, function(x) movingFun(x, 7, sum, "to")) 

seasonal_break_rainfall_tigger <- subset(seasonal_break_rainfall_MovMean7, day_of_year_rain)  

seasonal_break_rainfall_tigger  
seasonal_break_rainfall_tigger_pt <- raster::extract(seasonal_break_rainfall_tigger, 
                                                     site_bound_pts_df_point, method="simple") 

seasonal_break_rainfall_tigger_pt_df <- data.frame(year = year_input_rain,
                                                   day = day_of_year_rain,
                                                   rainfall = seasonal_break_rainfall_tigger_pt)

seasonal_break_rainfall_tigger_pt_df