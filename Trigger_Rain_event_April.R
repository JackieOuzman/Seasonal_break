
libs <- c("dplyr", "tidyr", 
          "ggplot2", "ggpubr",
          "ncdf4", "raster", "rgdal", 
          "lubridate", 
          "rgeos", "smoothr", "sf",
          "reshape",
          "tidyverse")

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

#### Bring in site data
lameroo <- read.csv("W:/Pastures/Gridded_seasonal_break/Check_code_selected_sites/Lameroo_seasonal_break_yrs.csv")

lameroo_look_up <- gather(lameroo,
                      year, day_of_year,
                      Year_1971:Year_2018)
#change year clm to number and remove the Year_ prefix
lameroo_look_up <- separate(lameroo_look_up, 
                        year, 
                        into = c("junk", "year_numb"), 
                        sep = "Year_")
head(lameroo_look_up)
lameroo_look_up <- dplyr::select(lameroo_look_up,  year_numb, day_of_year)

#This the table that will need to be populated with rainfall add clm that looks up what was the rainfall that triggered this


##################################################################################################################

################### Start here ############################################################################################

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
 
#------------------------------------------------------------------------------------------------------------
##1. define the boundary with and use a single layer raster 

 daily_rain_1 <- brick(
   paste("daily_rain/",
        "2000", ".daily_rain.nc", sep = ""),varname = "daily_rain")

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
site_bound_pts_df <- dplyr::select(site_bound_pts_df, x, y)
site_bound_pts_df_point <- SpatialPointsDataFrame(site_bound_pts_df[,c("x", "y")], site_bound_pts_df)

head(site_bound_pts_df_point)


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

#---------------------------------------------------------------------------------------------------------
Rain <- seasonal_break_rainfall_MovMean7 
Rain_extract <- raster::extract(Rain, 
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
#Remove the clm that have no data for  Rain_evap and add the coords
str(Rain_extract_wide)
tail(Rain_extract_wide)
Rain_extract_wide <- dplyr::select(Rain_extract_wide, -"61", -"62", -"63", -"64", -"65", -"66" )
Rain_extract_wide_x_y <- dplyr::select(Rain_extract_wide, "POINT_X",  "POINT_Y")
Rain_extract_wide_values <- dplyr::select(Rain_extract_wide,"67":"213")
#make a df with cood and values also add a clm that has a unquie id for grid cell
Rain_extract_df <- cbind(Rain_extract_wide_x_y, Rain_extract_wide_values)
Rain_extract_df <- mutate(Rain_extract_df, x_y = paste0(POINT_X, "_", POINT_Y))
Rain_extract_df_narrow <- gather(Rain_extract_df, 
                                      key = "day", value = "Rain", `67`:`213` )
head(Rain_extract_df_narrow) # this is only for one year and one site

#for the day clm I want to look up the rain value
head(Rain_extract_df_narrow) #1972
head(lameroo_look_up )
year_I_want <- filter(lameroo_look_up, year_numb == "1972")
year_I_want <- year_I_want[1,2]
year_I_want
rain_in_year_I_want <- filter(Rain_extract_df_narrow, day == year_I_want)
rain_in_year_I_want
