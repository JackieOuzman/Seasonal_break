
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

jax_list <- as.character(c(1971:2018)) #xx years of data as string


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
  daily_rain_crop_subset_day <- subset(daily_rain_crop, 61:212) #pull out the 1 March: 30 July 
  
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
  daily_evap_crop_subset_day <- subset(daily_evap_crop, 61:212) #pull out the 1 March: 30 July 
  
  #Add the moving window
  seasonal_break_evap_MovMean7 <- calc(daily_evap_crop_subset_day, function(x) movingFun(x, 7, sum, "to"))
  
  #then run the test here Rainfall - evaporation All positive values are the ones I want
  Rain_evap <- seasonal_break_rainfall_MovMean7 - seasonal_break_evap_MovMean7
  #return(Rain_evap)
  
  
  
  ###### This is part 2 changing the grids into df
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
                                     "181", "182", "183", "184", "185", "186","187","188","189","190",
                                     "191", "192", "193", "194", "195", "196","197","198","199","200",
                                     "201", "202", "203", "204", "205", "206","207","208","209","210", 
                                     "211", "212")
  
  #-----------------------------------------------------------------------------------------------------
  
  
  #Remove the clm that have no data for  Rain_evap and add the coords
  
  Rain_evap_extract_wide <- dplyr::select(Rain_evap_extract_wide, -"61", -"62", -"63", -"64", -"65", -"66" )
  Rain_evap_extract_wide_x_y <- dplyr::select(Rain_evap_extract_wide, "POINT_X",  "POINT_Y")
  Rain_evap_extract_wide_values <- dplyr::select(Rain_evap_extract_wide,"67":"212")
  
  #replace the values with 0 or 1; 0 = less than 0 and 1 is greater than 0
  Rain_evap_extract_wide_values <- Rain_evap_extract_wide_values %>% mutate_all(funs(ifelse(.<=0, 0, .))) #if its less than 0 give it value 0
  Rain_evap_extract_wide_values <- Rain_evap_extract_wide_values %>% mutate_all(funs(ifelse(.>0, 1, .))) #if its greater than 0 give it value 1
  
  first_occurance = names(Rain_evap_extract_wide_values)[apply(Rain_evap_extract_wide_values,1,match,x=1)]
  
  Rain_evap_occurance <- cbind(Rain_evap_extract_wide_x_y, first_occurance)
  Rain_evap_occurance <- dplyr::mutate(Rain_evap_occurance, x_y = paste0(POINT_X, "_", POINT_Y))
  colnames(Rain_evap_occurance) <- c("POINT_X", "POINT_Y", year_input, "x_y")
  
  return(Rain_evap_occurance)
  
}

for (i in jax_list) {
  assign(paste0("Rain_evap", i), function_rainfall_evap(i, site))
}


#make a list of the grids 
df_list = mget(ls(pattern = "Rain_evap[0-9]"))
big_data = dplyr::bind_cols(df_list)
#Add an iD clm
big_data$ID <- seq.int(nrow(big_data))


#split the data into years only selecting clms with years and ID
big_data1 <- dplyr::select(big_data, -starts_with("POINT"),
                    -starts_with("x_y"))
big_data1
#Now fix up the clm heading names
colnames(big_data1) <- paste("Year", colnames(big_data1), sep = "_")

#split the data into coords only selecting clms coords and ID
big_data2 <- dplyr::select(big_data,POINT_X,POINT_Y, x_y, ID) 
big_data2

#join the two datasets together
seasonal_break_day_year <- left_join(big_data2, big_data1, by = c("ID" = "Year_ID"))
head(seasonal_break_day_year)
seasonal_break_day_year <- dplyr::select(seasonal_break_day_year,ID, everything()) 


#make data number not factor change to character first and then to number


seasonal_break_day_year[5:52] <- lapply(seasonal_break_day_year[5:52], as.character) 
seasonal_break_day_year[5:52] <- lapply(seasonal_break_day_year[5:52], as.numeric)
#recode the na to zero
seasonal_break_day_year[is.na(seasonal_break_day_year)] <- 0
seasonal_break_day_year
head(seasonal_break_day_year)
readr::write_csv(seasonal_break_day_year, 
          "W:/Pastures/Gridded_seasonal_break/Check_code_selected_sites/Aust_seasonal_break_yrs.csv")

##############################################################################################################
#clean up work space
rm(list=(ls(pattern = "Rain_evap[0-9]")))

#### Play around with graphs
#need to remove the pts that are GRDC growing zones


head(seasonal_break_day_year)
temp <- seasonal_break_day_year
#recode my NA to zero.
str(temp)
#change to number not factor
temp[5:52] <- lapply(temp[5:52], as.double) 
#recode na to 0
temp[is.na(temp)] <- 0
head(temp)


#make data set tidy this will only work with small datasets I dont think I can do it with regions
temp_plot <- gather(temp, 
                                       year, day_of_year, 
                                       Year_1971:Year_2018)
str(temp_plot)

###graphing options for each year####
#This is too much data!
 # ggplot(temp_plot, aes(ID, day_of_year, colour = year))+
 #   geom_point(alpha = 0.2)+
 #   geom_line()+
 #   #geom_smooth(se= FALSE) +
 #   theme_bw()+
 #   labs(title= paste0("GRDC - seasonal break days"),
 #        x ="site", 
 #        y = "day of year")

#temp_plot %>%  filter(between(day_of_year, Year_1971, Year_1975))
#cant do between using charters need to convert my years back to numbers
        

temp_plot %>%  filter(day_of_year == "Year_1971") %>% 
  ggplot( aes(temp_plot$day_of_year)) + 
  #geom_histogram()+
  geom_density(col=2) + 
  labs(title="Seasonal break") +
  labs(x="site", y="Count") #+
  #geom_vline(aes(yintercept=mean(day_of_year)),
   #           color="blue", linetype="dashed", size=1)


##### IF I had GRDC zones or case study sites 
temp_plot %>%  filter(day_of_year == "Year_1971") %>% 
  ggplot( aes(temp_plot$day_of_year), colour = GRDC_zones) + 
  #geom_histogram()+
  geom_density(col=2) + 
  labs(title="Seasonal break") +
  labs(x="day_of_year", y="Count")#+
  #geom_vline(aes(yintercept=mean(day_of_year)),
            # color="blue", linetype="dashed", size=1)







##################    EXTRA NOTES   ##########################

 ##### what sites/ years are still producing NA 
 seasonal_break_day_year_NA <- seasonal_break_day_year
 
 seasonal_break_day_year_NA[4:52] <- lapply(seasonal_break_day_year_NA[4:52], as.numeric) 
 seasonal_break_day_year_NA[is.na(seasonal_break_day_year_NA)] <- -9999
 
 #move ID cl to the front
 seasonal_break_day_year_NA <- select(seasonal_break_day_year_NA,ID, everything()) 
 head(seasonal_break_day_year_NA)
 tail(seasonal_break_day_year_NA)
 
 seasonal_break_day_year_Lameroo <- seasonal_break_day_year_NA %>% 
   filter(between(ID, 93953, 93954)|
            between(ID, 94175, 94176)) 
 
 head(seasonal_break_day_year_Lameroo)












