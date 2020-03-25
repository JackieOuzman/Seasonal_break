#This is to chcek if our function is working and drill down into one site



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




file_save <- ("W:/Pastures/Gridded_seasonal_break") #jackie
#setwd("T:/Pastures/Gridded_seasonal_break") #bonny

setwd("I:/work/silo") #the folder now has curley bracket which is means something in R so the is a work around
getwd()

#------------------------------------------------------------------------------------------------------------------
#bring in my spatial data



site_import <- st_read("W:/Pastures/Gridded_seasonal_break/Boundary_for_analysis/Lamaroo_rectangle.shp")

site_sf <- as(site_import, "Spatial") #convert to a sp object
year_input <- 1972
site_name <- "Lameroo"
site <- site_sf
plot(site)
rolling_avearge_days = 5
#day_of_break = 
#------------------------------------------------------------------------------------------------------------
##1. define the boundary with and use a single layer raster 

   # daily_rain_1 <- brick(
   #   paste("daily_rain/",
   #         "2000", ".daily_rain.nc", sep = ""),varname = "daily_rain")

#crop to a fix area
daily_rain_crop <- crop(daily_rain_1, site)
daily_rain_crop

site_bound_raster <- daily_rain_crop$ X2000.01.01
plot(site_bound_raster)
site_bound_raster

##2. extract points from the raster as a point shapefile
site_bound_pts <- rasterToPoints(site_bound_raster)
names(site_bound_pts) <- c("longitude", "latitude", "value")
site_bound_pts_df <- as.data.frame(site_bound_pts)
site_bound_pts_df <- select(site_bound_pts_df, x, y)
site_bound_pts_df_point <- SpatialPointsDataFrame(site_bound_pts_df[,c("x", "y")], site_bound_pts_df)

head(site_bound_pts_df_point)


### Rainfall and Evaporation
#function_rainfall_evap <- function(year_input, site) {
  ############################################
  ##1. Rainfall
  
 
   daily_rain <- brick(
     paste("daily_rain/",
           year_input, ".daily_rain.nc", sep = ""),varname = "daily_rain")
  
  #crop to a fix area
  daily_rain_crop <- crop(daily_rain, site)
  
  #only use a few days
  daily_rain_crop_subset_day <- subset(daily_rain_crop, 61:213) #pull out the 1stMarch to 31th July leap year
   
  #Add the moving window avearge of 7 days ? should this be sum?
  seasonal_break_rainfall_MovMean7 <- calc(daily_rain_crop_subset_day, function(x) movingFun(x, rolling_avearge_days, sum, "to"))
  #seasonal_break_rainfall_MovMean7 <- calc(daily_rain_crop_subset_day, function(x) movingFun(x, 1, sum, "to"))
  seasonal_break_rainfall_MovMean7
  ############################################
  ##2. Evaporation stuff here similar to above
  
   daily_evap <- brick(
     paste("evap_pan/",
           year_input, ".evap_pan.nc", sep = ""),varname = "evap_pan")
  
  #crop to a fix area
  daily_evap_crop <- crop(daily_evap, site)
  
  #only use a few days
  daily_evap_crop_subset_day <- subset(daily_evap_crop, 61:213) #pull out the 1stMarch to 30th June this is leap year days
  #daily_evap_crop_subset_day <- subset(daily_evap_crop, 61:213) #pull out the 1stMarch to 31th July
  #Add the moving window
  seasonal_break_evap_MovMean7 <- calc(daily_evap_crop_subset_day, function(x) movingFun(x, rolling_avearge_days, sum, "to"))
  #seasonal_break_evap_MovMean7 <- calc(daily_evap_crop_subset_day, function(x) movingFun(x, 1, sum, "to"))
  
  
  #then run the test here Rainfall - evaporation All positive values are the ones I want
  Rain_evap <- seasonal_break_rainfall_MovMean7 - seasonal_break_evap_MovMean7
  #return(Rain_evap)
  
 
  
  
  ###### This is part 2 changing the grids into df
  ### 2a is the Rain_evap grid
  #---------------------------------------------------------------------------------------------------------
  Rain_evap_extract <- raster::extract(Rain_evap, 
                                       site_bound_pts_df_point, method="simple")
  
  Rain_evap_extract_wide <- data.frame(site_bound_pts_df_point$x, 
                                       site_bound_pts_df_point$y, 
                                       Rain_evap_extract)

  
  ##### assign names for all the layers this will days
  names(Rain_evap_extract_wide) <- c("POINT_X", "POINT_Y", 
                                     "61", "62", "63", "64", "65", "66","67","68","69","70",
                                     "71", "72", "73", "74", "75", "76","77","78","79","80",
                                     "81", "82", "83", "84", "85", "86","87","88","89","90",
                                     "91", "92", "93", "94", "95", "96","97","98","99","100",
                                     "101", "102", "103", "104", "105", "106","107","108","109","110",
                                     "111", "112", "113", "114", "115", "116","117","118","119","120",
                                     "121", "122", "123", "124", "125", "126","127","128","129","130",
                                     "131", "132", "133", "134", "135", "136","137","138","139","140",
                                     "141", "142", "143", "144", "145", "146","147","148","149","150",
                                     "151", "152", "153", "154", "155", "156","157","158","159","160",
                                     "161", "162", "163", "164", "165", "166","167","168","169","170",
                                     "171", "172", "173", "174", "175", "176","177","178","179","180",
                                     "181", "182","183", "184", "185", "186", "187", "188" , "189",
                                     "190", "191", "192", "193", "194", "195", "196", "197", "198",
                                     "199", "200", "201", "202", "203", "204", "205", "206", "207", 
                                     "208", "209", "210", "211", "212", "213")
  
  #-----------------------------------------------------------------------------------------------------
  
  #Remove the clm that have no data for  Rain_evap and add the coords
  str(Rain_evap_extract_wide)
  tail(Rain_evap_extract_wide)
Rain_evap_extract_wide <- select(Rain_evap_extract_wide, -"61", -"62", -"63", -"64", -"65", -"66" )
  Rain_evap_extract_wide_x_y <- select(Rain_evap_extract_wide, "POINT_X",  "POINT_Y")
  Rain_evap_extract_wide_values <- select(Rain_evap_extract_wide,"67":"213")
  
  #make a df with cood and values also add a clm that has a unquie id for grid cell
  Rain_evap_extract_df <- cbind(Rain_evap_extract_wide_x_y, Rain_evap_extract_wide_values)
  Rain_evap_extract_df <- mutate(Rain_evap_extract_df, x_y = paste0(POINT_X, "_", POINT_Y))
  
  #make it a narrow data frame
  Rain_evap_extract_df_narrow <- gather(Rain_evap_extract_df, 
                                        key = "day", value = "Rain_evap", `67`:`213` )
  
  
  Rain_evap_extract_df_narrow <- filter(Rain_evap_extract_df_narrow, !is.na(Rain_evap))
  Rain_evap_extract_df_narrow$day_factor <- as.factor(Rain_evap_extract_df_narrow$day)
  Rain_evap_extract_df_narrow$Rain_evap_numb<- as.double(Rain_evap_extract_df_narrow$Rain_evap)
  
  
 
  
    
    ### 2b is the rainfall grid seasonal_break_rainfall_MovMean7
  #  ---------------------------------------------------------------------------------------------------------
  head(seasonal_break_rainfall_MovMean7)
    
  Rain_extract <- raster::extract(seasonal_break_rainfall_MovMean7, 
                                         site_bound_pts_df_point, method="simple")
  
  Rain_extract_wide <- data.frame(site_bound_pts_df_point$x, 
                                       site_bound_pts_df_point$y, 
                                       Rain_extract)
  
  
  ##### assign names for all the layers this will days
  names(Rain_extract_wide) <- c("POINT_X", "POINT_Y", 
                                     "61", "62", "63", "64", "65", "66","67","68","69","70",
                                     "71", "72", "73", "74", "75", "76","77","78","79","80",
                                     "81", "82", "83", "84", "85", "86","87","88","89","90",
                                     "91", "92", "93", "94", "95", "96","97","98","99","100",
                                     "101", "102", "103", "104", "105", "106","107","108","109","110",
                                     "111", "112", "113", "114", "115", "116","117","118","119","120",
                                     "121", "122", "123", "124", "125", "126","127","128","129","130",
                                     "131", "132", "133", "134", "135", "136","137","138","139","140",
                                     "141", "142", "143", "144", "145", "146","147","148","149","150",
                                     "151", "152", "153", "154", "155", "156","157","158","159","160",
                                     "161", "162", "163", "164", "165", "166","167","168","169","170",
                                     "171", "172", "173", "174", "175", "176","177","178","179","180",
                                "181", "182","183", "184", "185", "186", "187", "188" , "189",
                                "190", "191", "192", "193", "194", "195", "196", "197", "198",
                                "199", "200", "201", "202", "203", "204", "205", "206", "207", 
                                "208", "209", "210", "211", "212", "213")
  
 # -----------------------------------------------------------------------------------------------------
    
    #Remove the clm that have no data for  Rainfall and add the coords
  
  Rain_extract_wide <- select(Rain_extract_wide, -"61", -"62", -"63", -"64", -"65", -"66" )
  Rain_extract_wide_x_y <- select(Rain_extract_wide, "POINT_X",  "POINT_Y")
  Rain_extract_wide_values <- select(Rain_extract_wide,"67":"213")
  
  #make a df with cood and values also add a clm that has a unquie id for grid cell
  Rain_extract_df <- cbind(Rain_extract_wide_x_y, Rain_extract_wide_values)
  Rain_extract_df <- mutate(Rain_extract_df, x_y = paste0(POINT_X, "_", POINT_Y))
  
  #make it a narrow data frame
  Rain_extract_df_narrow <- gather(Rain_extract_df, 
                                        key = "day", value = "Rain", `67`:`213` )
  
  
  Rain_extract_df_narrow <- filter(Rain_extract_df_narrow, !is.na(Rain))
  Rain_extract_df_narrow$day_factor <- as.factor(Rain_extract_df_narrow$day)
  Rain_extract_df_narrow$Rain_numb<- as.double(Rain_extract_df_narrow$Rain)
  
  
  head(Rain_extract_df_narrow)
  str(Rain_extract_df_narrow)

    
    ### 2c is the evaopration  grid seasonal_break_evap_MovMean7
  #---------------------------------------------------------------------------------------------------------
    head(seasonal_break_evap_MovMean7)
  
  Evap_extract <- raster::extract(seasonal_break_evap_MovMean7, 
                                  site_bound_pts_df_point, method="simple")
  
  Evap_extract_wide <- data.frame(site_bound_pts_df_point$x, 
                                  site_bound_pts_df_point$y, 
                                  Evap_extract)
  
  
  ##### assign names for all the layers this will days
  names(Evap_extract_wide) <- c("POINT_X", "POINT_Y", 
                                "61", "62", "63", "64", "65", "66","67","68","69","70",
                                "71", "72", "73", "74", "75", "76","77","78","79","80",
                                "81", "82", "83", "84", "85", "86","87","88","89","90",
                                "91", "92", "93", "94", "95", "96","97","98","99","100",
                                "101", "102", "103", "104", "105", "106","107","108","109","110",
                                "111", "112", "113", "114", "115", "116","117","118","119","120",
                                "121", "122", "123", "124", "125", "126","127","128","129","130",
                                "131", "132", "133", "134", "135", "136","137","138","139","140",
                                "141", "142", "143", "144", "145", "146","147","148","149","150",
                                "151", "152", "153", "154", "155", "156","157","158","159","160",
                                "161", "162", "163", "164", "165", "166","167","168","169","170",
                                "171", "172", "173", "174", "175", "176","177","178","179","180",
                                "181", "182","183", "184", "185", "186", "187", "188" , "189",
                                "190", "191", "192", "193", "194", "195", "196", "197", "198",
                                "199", "200", "201", "202", "203", "204", "205", "206", "207", 
                                "208", "209", "210", "211", "212", "213")
  
  #-----------------------------------------------------------------------------------------------------
    
    #Remove the clm that have no data for  evap and add the coords
    
  Evap_extract_wide <- select(Evap_extract_wide, -"61", -"62", -"63", -"64", -"65", -"66" )
  Evap_extract_wide_x_y <- select(Evap_extract_wide, "POINT_X",  "POINT_Y")
  Evap_extract_wide_values <- select(Evap_extract_wide,"67":"213")
  
  #make a df with cood and values also add a clm that has a unquie id for grid cell
  Evap_extract_df <- cbind(Evap_extract_wide_x_y, Evap_extract_wide_values)
  Evap_extract_df <- mutate(Evap_extract_df, x_y = paste0(POINT_X, "_", POINT_Y))
  
  #make it a narrow data frame
  Evap_extract_df_narrow <- gather(Evap_extract_df, 
                                   key = "day", value = "Evap", `67`:`213` )
  
  
  Evap_extract_df_narrow <- filter(Evap_extract_df_narrow, !is.na(Evap))
  Evap_extract_df_narrow$day_factor <- as.factor(Evap_extract_df_narrow$day)
  Evap_extract_df_narrow$Evap_numb<- as.double(Evap_extract_df_narrow$Evap)
  
  #dev.off() #having trouble with mapping this fixes it!
  head(Evap_extract_df_narrow)
  str(Evap_extract_df_narrow)
  #------------------------------------------------------------------------------------------------------ 
    
    str(Rain_extract_df_narrow)
    str(Evap_extract_df_narrow)
    str(Rain_evap_extract_df_narrow)
    #drop some clms so I can join together
    Rain_extract_df_narrow <- select(Rain_extract_df_narrow,
                                     day, Rain)
    Evap_extract_df_narrow <- select(Evap_extract_df_narrow,
                                     day, Evap)
    Rain_evap_extract_df_narrow <- select(Rain_evap_extract_df_narrow,
                                          POINT_X,
                                          POINT_Y,
                                          day,
                                          Rain_evap)
    
   
    
    
     Rain_Evap <- left_join(Rain_extract_df_narrow, Evap_extract_df_narrow)
    
    head(Rain_Evap)
    Rain_Evap_both <- left_join(Rain_evap_extract_df_narrow,Rain_Evap)
    str(Rain_Evap_both)  
      
    Rain_Evap_both$day_numb <- as.double(Rain_Evap_both$day)
    str(Rain_Evap_both)
    dim(Rain_Evap_both)
    
    
    # MAKE THE data set narrow again for easy plotting
    
    Rain_Evap_both_plot <- gather(Rain_Evap_both, variable, value, Rain_evap, Rain, Evap)
    str(Rain_Evap_both_plot)
    
    
      
     
    ### Now a bit of stuffing around to check find out what sites are near Lamaroo and what days our day calu does this make sense?
    #In year 2000 this was around 105 and 106
    # in 1972 we have a mix of NA for this same site
    
    
    ggplot(Rain_Evap_both_plot, aes(day_numb, value, colour = variable))+
             geom_point(alpha = 0.2)+
             geom_line()+
             #geom_smooth(se= FALSE) +
       #geom_vline(xintercept= day_of_break, linetype="dashed", color = "dark green", size = 1)+
             theme_bw()+
      labs(title= paste0("Check data for: ", site_name, ", year ", year_input),
           x ="day of year", 
           y = "",
           subtitle = paste0("Rolling average = ", rolling_avearge_days, ", Window = 61:212"))+
      scale_colour_manual(values = c("red", "blue", "black"),
                          name="Climate data",
                          breaks=c("Evap", "Rain", "Rain_evap"),
                          labels=c("Evaporation", "Rainfall", "Seasonal break"))
      
      
    
    
    #remove everthing except the rasters from silo for specific year RE RUN SAME YEAR
    #rm(list = setdiff(ls(), c("daily_rain", "daily_evap", "daily_rain_1")))
    
   
    #remove everthing except one rasters that set bounds RE RUN NEW YEAR
     rm(list = setdiff(ls(), c("daily_rain_1")))
    
     
    