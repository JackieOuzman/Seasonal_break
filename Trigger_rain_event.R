rm(list=ls())

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
#lameroo <- read.csv("W:/Pastures/Gridded_seasonal_break/Check_code_selected_sites/Lameroo_seasonal_break_yrs.csv")
study_sites <- read.csv("W:/Pastures/Gridded_seasonal_break/Check_code_selected_sites/GRDC_zone_seasonal_break_yrs_v3_join_study_sites.csv")

str(study_sites)
## just keep point with site info..

study_sites$site_name <- as.character(study_sites$site_name)

unique(study_sites$site_name)
study_sites <- filter(study_sites,
                           site_name != " ")

#keep only a few clms I have 4 points for everysite I only want to keep one
study_sites <- study_sites %>% distinct(site_name, .keep_all = TRUE)

study_sites <- dplyr::select(study_sites,
                      site_name,
                      ID,
                      Year_1971:Year_2018)

str(study_sites)

study_sites <- gather(study_sites,
                         year, day_of_year,
                         Year_1971:Year_2018)

#change year clm to number and remove the Year_ prefix
study_sites <- separate(study_sites, 
                             year, 
                             into = c("junk", "year_numb"), 
                             sep = "Year_")
study_sites <- dplyr::select(study_sites, - junk)

###################################################################

# this will be the dataframe that the function will use...
#now I need to pull out the 
#? not sure what now pull out info for each site?
site = "Lameroo"
str(study_sites)
one_study_sites <- filter(study_sites, site_name == site)
site_import <- st_read("W:/Pastures/Gridded_seasonal_break/Boundary_for_analysis/Lamaroo_rectangle.shp")

site_sf <- as(site_import, "Spatial") #convert to a sp object
site_name <- "Lameroo"
site <- site_sf
plot(site)
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

#add ID clm that the function will use
one_study_sites$ID_function <- seq.int(nrow(one_study_sites))


### i in my loop will be the ID_function
#so say this i = 1.
#ID_function_list <- 1
#df <- one_study_sites


list_year <- as.character(c(1:48)) #xx years of data as string
################################################
function_rain_on_break <- function(list_year, df, site) {

year_input_rain <- filter(df, ID_function == list_year) 
year_input_rain <- year_input_rain[,3] 
year_input_rain
#might need to chnage this to numeric I think its a character

day_input_rain <- filter(df, ID_function == list_year) 
day_input_rain <- as.character(day_input_rain[,4] )
day_input_rain

daily_rain <- brick(
  paste("daily_rain/",
        year_input_rain, ".daily_rain.nc", sep = ""),varname = "daily_rain")

#crop to a fix area
daily_rain_crop <- crop(daily_rain, site)

#only use a few days
daily_rain_crop_subset_day <- subset(daily_rain_crop, 61:212) #pull out the 1 March: 30 July 

#Add the moving window avearge of 7 days ? 
seasonal_break_rainfall_MovMean7 <- calc(daily_rain_crop_subset_day, function(x) movingFun(x, 7, sum, "to")) 
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
                                   "181", "182", "183", "184", "185", "186","187","188","189","190",
                                   "191", "192", "193", "194", "195", "196","197","198","199","200",
                                   "201", "202", "203", "204", "205", "206","207","208","209","210", 
                                   "211", "212")

#-----------------------------------------------------------------------------------------------------


#Remove the clm that have no data for  Rain_evap and add the coords

Rain_extract_wide <- dplyr::select(Rain_extract_wide, -"61", -"62", -"63", -"64", -"65", -"66" )
Rain_extract_wide_x_y <- dplyr::select(Rain_extract_wide, "POINT_X",  "POINT_Y")

return(Rain_extract_wide_values)
}

for (i in list_year) {
  assign(paste0("Rain", i), function_rain_on_break(list_year, df, site))
}
##################################################################################
#Up to here the below line is not working as part of function
Rain_extract_wide_values <- dplyr::select(Rain_extract_wide, day_input_rain) #this is the day I want results for


Rain_on_break <- cbind(Rain_extract_wide_x_y, Rain_extract_wide_values)
Rain_on_break <- dplyr::mutate(Rain_on_break, 
                               x_y = paste0(POINT_X, "_", POINT_Y))
colnames(Rain_on_break) <- c("POINT_X", "POINT_Y", year_input_rain, "x_y")






####Hopefully this works an I will get value for df for each year


#make a list of the df 
df_list = mget(ls(pattern = "Rain[0-9]"))
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
rain_on_break <- left_join(big_data2, big_data1, by = c("ID" = "Year_ID"))
head(rain_on_break)
rain_on_break <- dplyr::select(rain_on_break,ID, everything()) 


#make data number not factor change to character first and then to number


rain_on_break[5:52] <- lapply(rain_on_break[5:52], as.character) 
rain_on_break[5:52] <- lapply(rain_on_break[5:52], as.numeric)
#recode the na to zero
rain_on_break[is.na(rain_on_break)] <- 0
readr::write_csv(rain_on_break, 
                 "W:/Pastures/Gridded_seasonal_break/Check_code_selected_sites/site_rain_on_break.csv")