##Aggregate SILO monthly and annual and build map layers
##================================
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



############################################################################################################################
################### Start here ############################################################################################

#------------------------------------------------------------------------------------------------------------------
#set up working directories
file_save <- "W:/Pastures/Gridded_seasonal_break" #jackie
#setwd("T:/Pastures/Gridded_seasonal_break") #bonny

#ensure that temp rasters are writting to the correct directory
#rasterOptions(tmpdir ='//osm-03-mel.it.csiro.au/OSM_MEL_CES_Ag_Systems_MSA_work/Users/')
rasterOptions(tmpdir ='//138.194.104.21/OSM_MEL_CES_SpatioTemp_scratch')

setwd("Y:/work/silo") #the folder now has curley bracket which is means something in R so the is a work around
getwd()

#------------------------------------------------------------------------------------------------------------------
#set the area for running the analysis
#site_import <- st_read("W:/Pastures/Gridded_seasonal_break/Boundary_for_analysis/Lamaroo_rectangle.shp")
site_import <- st_read("W:/Pastures/Gridded_seasonal_break/Boundary_for_analysis/GRDC_AgroEcological_zones_boundaries_06_region_jax.shp")

site_sf <- as(site_import, "Spatial") #convert to a sp object
site_name <- "Aust"
site <- site_sf
plot(site)


#------------------------------------------------------------------------------------------------------------------

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
site_bound_pts_df <- dplyr::select(site_bound_pts_df, x, y)
site_bound_pts_df_point <- SpatialPointsDataFrame(site_bound_pts_df[,c("x", "y")], site_bound_pts_df)

plot(site_bound_pts_df_point)

### list of years ####

jax_list <- as.character(c(1971:1973)) #xx years of data as string
#jax_list <- as.character(c(1971:1975)) #xx years of data as string
#year_input <- 1971
#######################################################################################################

### Rainfall and Evaporation
function_rainfall <- function(year_input, site) {
  ############################################
  ##1. Rainfall
  daily_rain <- brick(
    paste("daily_rain/",
          year_input, ".daily_rain.nc", sep = ""),varname = "daily_rain")
  
  #crop to a fix area
  daily_rain_crop <- crop(daily_rain, site)
  
  #only use a few days
  daily_rain_crop_subset_day <- subset(daily_rain_crop, 61:212) #pull out the 1 March: 30 July 
 
  #Add the moving window avearge of 7 days ? should this be sum?
  seasonal_break_rainfall_MovMean7 <- calc(daily_rain_crop_subset_day, function(x) movingFun(x, 7, sum, "to"))
  #plot(seasonal_break_rainfall_MovMean7)
  
  ############################################
    #then run the test here Rainfall - evaporation All positive values are the ones I want
  Rain <- seasonal_break_rainfall_MovMean7
  #return(Rain_evap)
  
  
  ###### This is part 2 changing the grids into df
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
                                     "181", "182", "183", "184", "185", "186","187","188","189","190",
                                     "191", "192", "193", "194", "195", "196","197","198","199","200",
                                     "201", "202", "203", "204", "205", "206","207","208","209","210", 
                                     "211", "212")
  
  #-----------------------------------------------------------------------------------------------------
  
  
  #Remove the clm that have no data for  Rain_evap and add the coords
  
  Rain_extract_wide <- dplyr::select(Rain_extract_wide, -"61", -"62", -"63", -"64", -"65", -"66" )
  #Rain_extract_wide_x_y <- dplyr::select(Rain_extract_wide, "POINT_X",  "POINT_Y")
  #head(Rain_extract_wide_x_y)
  
  Rain <- Rain_extract_wide
  #return(Rain)
     Rain <- dplyr::mutate(Rain, x_y = paste0(POINT_X, "_", POINT_Y))
   # Rain <- gather(Rain, key = "day", value = "rain", `67`: `212`)
    Rain <- dplyr::mutate(Rain, year = year_input)
   # colnames(Rain) <- c("POINT_X", "POINT_Y", "x_y", "day", "rain", "year")
    return(Rain)

}

for (i in jax_list) {
  assign(paste0("Rain_", i), function_rainfall(i, site))
}

Rain_1971 <- dplyr::mutate(Rain_1971, x_y = paste0(POINT_X, "_", POINT_Y))
Rain_1972 <- dplyr::mutate(Rain_1972, x_y = paste0(POINT_X, "_", POINT_Y))
Rain_1973 <- dplyr::mutate(Rain_1973, x_y = paste0(POINT_X, "_", POINT_Y))

head(Rain_1971,2)
head(Rain_1973, 2)
# I need to change df so its tidy..



#make a list of the grids 
df_list = mget(ls(pattern = "Rain_[0-9]")) # this is not what I want...
#df_list = ls(pattern = "Rain_[0-9]")

big_data = dplyr::bind_rows(df_list)
head(big_data, 3)
tail(big_data, 3)
#Add an iD clm
big_data$ID <- seq.int(nrow(big_data))
 head(big_data)

tail(str(big_data),2)

big_data$year <- as.numeric(big_data$year)
big_data$day <-  as.numeric(big_data$day)

head(big_data, 2)
tail(big_data, 2)

rain_fall_rolling_av <- big_data
readr::write_csv(rain_fall_rolling_av, 
                 "W:/Pastures/Gridded_seasonal_break/Check_code_selected_sites/rain_fall_rolling_av.csv")
head(rain_fall_rolling_av)

#########################################################################################################

#clean up workspace
#rm(list = ls()[!(ls() %in% c("rain_fall_rolling_av"))])

########################################################################################################


#### what is the site I want to look at 

site_coord_ref <-Lamaroo
site_coord_ref
# bring in the day of break data
seasonal_break_output <-read.csv("W:/Pastures/Gridded_seasonal_break/Check_code_selected_sites/GRDC_zone_seasonal_break_yrs_v3_join_study_sites.csv")
head(seasonal_break_output)
str(seasonal_break_output$x_y)
seasonal_break_output$x_y <- as.character(seasonal_break_output$x_y)

subset_seasonal_break_output <- filter(seasonal_break_output, x_y == "140.45_-35.25")

head(subset_seasonal_break_output, 2)

subset_seasonal_break_output <- gather(subset_seasonal_break_output, 
                                       key = "Year", value = "day", 'Year_1971':'Year_2018' )

head(subset_seasonal_break_output)

subset_seasonal_break_output <- separate(subset_seasonal_break_output,Year, c("junk", "year"), "_" )
subset_seasonal_break_output <- dplyr::select(subset_seasonal_break_output,  x_y,year, day)
head(subset_seasonal_break_output)
###################################################################################################

#subset the rainfall data by site
head(rain_fall_rolling_av)
str(rain_fall_rolling_av$x_y)

subset_rain_fall_rolling_av <- filter(rain_fall_rolling_av, x_y == "140.45_-35.25")
head(subset_rain_fall_rolling_av)
head(subset_seasonal_break_output) #this is my base data that I want to add to

unique(subset_rain_fall_rolling_av$x_y)
unique(subset_seasonal_break_output$x_y) #? not sure why this output as it is - looks fine to me??



subset_seasonal_break_output$year <- as.numeric(subset_seasonal_break_output$year)
subset_seasonal_break_output$day <- as.numeric(subset_seasonal_break_output$day)

subset_rain_fall_rolling_av$year <- as.numeric(subset_rain_fall_rolling_av$year)
subset_rain_fall_rolling_av$day <- as.numeric(subset_rain_fall_rolling_av$day)

str(subset_seasonal_break_output) #this is narrow
str(rain_fall_rolling_av)#thi is wide - needs to be narrow
str(rain_fall_rolling_av$year)
head(rain_fall_rolling_av, 2)


subset_rain_fall_rolling_av <- gather(subset_rain_fall_rolling_av, 
                                       key = "day", value = "rainfall", '67':'212' )

str(subset_rain_fall_rolling_av$day)
str(subset_seasonal_break_output$day)

subset_day_break_with_rainfall <-  left_join(subset_seasonal_break_output, 
                    subset_rain_fall_rolling_av, by = c("year","day", "x_y"))



####### list of coordinates that I should be using  

#site_coord_ref <-"146.1_-30.7"
sites <-read.csv("W:/Pastures/Gridded_seasonal_break/Check_code_selected_sites/GRDC_zone_seasonal_break_yrs_v3_join_study_sites_only.csv")
str(sites)
sites <- dplyr::select(sites, POINT_X, POINT_Y, x_y ,site_name:CENTROID_Y)

#I have 4 point but should I just use one?
sites_unique <- distinct(sites, site_name, .keep_all = TRUE)
head(sites_unique, 20)

Lamaroo <- filter(sites_unique, site_name == "Lameroo") %>% 
  dplyr::select(x_y)
Waikerie <- filter(sites_unique, site_name == "Waikerie") %>% 
  dplyr::select(x_y)
Roseworty <- filter(sites_unique, site_name == "Roseworty") %>% 
  dplyr::select(x_y)
Minnipa <- filter(sites_unique, site_name == "Minnipa") %>% 
  dplyr::select(x_y)
Kikoira <- filter(sites_unique, site_name == "Kikoira") %>% 
  dplyr::select(x_y)
Uranquinty <- filter(sites_unique, site_name == "Uranquinty") %>% 
  dplyr::select(x_y)
Wagga_Wagga <- filter(sites_unique, site_name == "Wagga Wagga") %>% 
  dplyr::select(x_y)
Eurongilly <- filter(sites_unique, site_name == "Eurongilly") %>% 
  dplyr::select(x_y)
Piangal <- filter(sites_unique, site_name == "Piangal") %>% 
  dplyr::select(x_y)
Ardath <- filter(sites_unique, site_name == "Ardath") %>% 
  dplyr::select(x_y)
Mingenew <- filter(sites_unique, site_name == "Mingenew") %>% 
  dplyr::select(x_y)

