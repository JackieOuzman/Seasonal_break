
##Aggregate SILO monthly and annual and build map layers
##================================
rm(list=ls())

##Libraries and functions=================================================================
#install.packages("raster")
#install.packages("ncdf4")
#install.packages("RNetCDF")
#install.packages("data.table")
#install.packages("reshape2")
#install.packages("doBy")
#install.packages("maptools")
#install.packages("maps")
#install.packages("rasterVis")
#install.packages("lattice")
#install.packages("mapdata")
#install.packages("RColorBrewer")
#install.packages("lubridate")
#install.packages("spatial.tools")
#install.packages("mapdata")
#install.packages("RSenaps")
#install.packages("settings")
#install.packages("httr")
#install.packages("maps")


library(sp)
library(rgdal)
library(raster)

library(ncdf4)
library(RNetCDF)
library(RColorBrewer)
library(data.table)
library(reshape2)
library(doBy)
library(maptools)
library(maps)
library(lattice)
library(latticeExtra)
library(rasterVis)
library(mapdata)

library(lubridate)
library(spatial.tools)
library(mapdata)
require(RSenaps) #error message for my R version
library(settings)
library(httr)
library(sf)
library(rgeos)
library(tidyverse)

#===========================
# Compute temp variables ------------------------------------------------------




############################################################################################################################
################### Start here ############################################################################################

------------------------------------------------------------------------------------------------------------------
  #set up working directories
file_save <- ("W:/Pastures/Gridded_seasonal_break") #jackie
#setwd("T:/Pastures/Gridded_seasonal_break") #bonny

setwd("I:/work/silo") #the folder now has curley bracket which is means something in R so the is a work around
getwd()

------------------------------------------------------------------------------------------------------------------
#bring in my spatial data
GRDC_bound_mallee <- st_read("W:/Pastures/Gridded_seasonal_break/Boundary_for_analysis/SA_Vic_Mallee.shp")
GRDC_bound_mallee_sf <- as(GRDC_bound_mallee, "Spatial") #convert to a sp object
plot(GRDC_bound_mallee_sf)

GRDC_bound_wimm <- st_read("W:/Pastures/Gridded_seasonal_break/Boundary_for_analysis/SA_Vic_Bordertown-Wimmera.shp")
GRDC_bound_wimm_sf <- as(GRDC_bound_wimm, "Spatial") #convert to a sp object
plot(GRDC_bound_wimm_sf)


GRDC_bound_wheatbelt <- st_read("W:/Pastures/Gridded_seasonal_break/Boundary_for_analysis/GRDC_AgroEcological_zones_boundaries_06_region_jax.shp")
GRDC_bound_wheatbelt_sf <- as(GRDC_bound_wheatbelt, "Spatial") #convert to a sp object
plot(GRDC_bound_wheatbelt_sf)
------------------------------------------------------------------------------------------------------------------
#set the area for running the analysis
site <- GRDC_bound_mallee_sf


------------------------------------------------------------------------------------------------------------------
##1. define the boundary with and use a single layer raster 

daily_rain <- brick(
  paste("daily_rain/",
          2000, ".daily_rain.nc", sep = ""),varname = "daily_rain")

#crop to a fix area
daily_rain_crop <- crop(daily_rain, site)
daily_rain_crop

site_bound_raster <- daily_rain_crop$ X2000.01.01

##2. extract points from the raster as a point shapefile
site_bound_pts <- rasterToPoints(site_bound_raster)
names(site_bound_pts) <- c("longitude", "latitude", "value")
site_bound_pts_df <- as.data.frame(site_bound_pts)
site_bound_pts_df <- select(site_bound_pts_df, x, y)
site_bound_pts_df_point <- SpatialPointsDataFrame(site_bound_pts_df[,c("x", "y")], site_bound_pts_df)

plot(site_bound_pts_df_point)

### list of years ####

jax_list <- as.character(c(2000:2002)) #xx years of data as string
#jax_list <- as.character(c(1970:2018)) #xx years of data as string

#######################################################################################################


### Rainfall and Evaporation
function_rainfall_evap <- function(year_input, site) {
  ############################################
  ##1. Rainfall
  daily_rain <- brick(
    paste("daily_rain/",
          year_input, ".daily_rain.nc", sep = ""),varname = "daily_rain")
  
  #crop to a fix area
  daily_rain_crop <- crop(daily_rain, site)
  
  #only use a few days
  daily_rain_crop_subset_day <- subset(daily_rain_crop, 61:182) #pull out the 1stMarch to 30th June 
  
  #Add the moving window avearge of 7 days ? should this be sum?
  seasonal_break_rainfall_MovMean7 <- calc(daily_rain_crop_subset_day, function(x) movingFun(x, 7, sum, "to"))
  
  
  ############################################
  ##2. Evaporation stuff here similar to above
  daily_evap <- brick(
    paste("evap_pan/",
          year_input, ".evap_pan.nc", sep = ""),varname = "evap_pan")
  #crop to a fix area
  daily_evap_crop <- crop(daily_evap, site)
  
  #only use a few days
  daily_evap_crop_subset_day <- subset(daily_evap_crop, 61:182) #pull out the 1stMarch to 30th June
  
  #Add the moving window
  seasonal_break_evap_MovMean7 <- calc(daily_evap_crop_subset_day, function(x) movingFun(x, 7, sum, "to"))
  
  #then run the test here Rainfall - evaporation All positive values are the ones I want
  Rain_evap <- seasonal_break_rainfall_MovMean7 - seasonal_break_evap_MovMean7
  #return(Rain_evap)
  
  ###### This is part 2 changing thegrids into df
  Rain_evap_extract <- raster::extract(Rain_evap, site_bound_pts_df_point, method="simple")
  
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
                                     "181", "182")
  #Remove the clm that have no data                          
  Rain_evap_extract_wide <- select(Rain_evap_extract_wide, -"61", -"62", -"63", -"64", -"65", -"66" )
  Rain_evap_extract_wide_x_y <- select(Rain_evap_extract_wide, "POINT_X",  "POINT_Y")
  Rain_evap_extract_wide_values <- select(Rain_evap_extract_wide,"67":"182")
  # #replace the values with 0 or 1; 0 = less than 0 and 1 is greater than 0
  
  Rain_evap_extract_wide_values <- Rain_evap_extract_wide_values %>% mutate_all(funs(ifelse(.<=0, 0, .))) #if its less than 0 give it value 0
  Rain_evap_extract_wide_values <- Rain_evap_extract_wide_values %>% mutate_all(funs(ifelse(.>0, 1, .))) #if its greater than 0 give it value 1
  
  first_occurance = names(Rain_evap_extract_wide_values)[apply(Rain_evap_extract_wide_values,1,match,x=1)]
  
  Rain_evap_occurance <- cbind(Rain_evap_extract_wide_x_y, first_occurance)
  Rain_evap_occurance <- mutate(Rain_evap_occurance, x_y = paste0(POINT_X, "_", POINT_Y))
  colnames(Rain_evap_occurance) <- c("POINT_X", "POINT_Y", year_input, "x_y")
  return(Rain_evap_occurance)
  
}


# for (i in jax_list) {
#   assign(paste0("Rain_evap", i), function_rainfall_evap(i))
# }
#?? Just add the other variable for the rainfall_evap function
for (i in jax_list) {
  assign(paste0("Rain_evap", i), function_rainfall_evap(i, site))
}

plot(Rain_evap2000)
head(Rain_evap2000)
###################### UP TO HERE ##########################################################################

### join the df together
Rain_evap2000_2001 <- left_join(Rain_evap2000, Rain_evap2001)
Rain_evap2000_2002 <- left_join(Rain_evap2000_2001, Rain_evap2002)

head(Rain_evap2000_2002)


write.csv(Rain_evap2000_2002, "W:/Pastures/Gridded_seasonal_break/Rain_evap2000_2002.csv")


test<- select(Rain_evap2000_2002, "2000", "2001", "2002")

head(test)

test_narrow <- gather(Rain_evap2000_2002, key = "year", value = "Day_of_break", `2000`:`2002` )

head(test_narrow)

test_narrow$year_factor <- as.factor(test_narrow$year)
test_narrow$Day_of_break_numb<- as.double(test_narrow$Day_of_break)

test_narrow <- filter(test_narrow, !is.na(Day_of_break_numb))

str(test_narrow)

ggplot(test_narrow, aes(year_factor, Day_of_break_numb))+
  geom_point()+
  #geom_boxplot()
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
         plot.caption = element_text(hjust = 0))+
   labs(x = "Year",
        y = "Day of break",
        title = "Seasonal break for Aust area",
        caption = "This the first day of break defined as: when sum 7 days rainfall is greater than sum 7 days evaopration")

























