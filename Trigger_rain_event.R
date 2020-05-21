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
lameroo <- read.csv("W:/Pastures/Gridded_seasonal_break/Check_code_selected_sites/Lameroo_seasonal_break_yrs.csv")
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
df <- one_study_sites
df <- mutate(df, day_of_year1 = paste0("Day", day_of_year))
str(df)

list_year <- as.character(c(1:48)) #xx years of data as string
################################################
function_rain_on_break <- function(list_year, df, site) {


#df <- mutate(df, day_of_year1 = paste0("Day", day_of_year))
#df$day_of_year <- as.character(df$day_of_year)
year_input_rain <- filter(df, ID_function == list_year) 
year_input_rain <- year_input_rain[,3] 
year_input_rain
#might need to chnage this to numeric I think its a character

day_input_rain <- filter(df, ID_function == list_year) 
day_input_rain <- filter(df, ID_function == list_year)
day_input_rain <- as.character(day_input_rain[,6] )
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
                                   "Day61", "Day62", "Day63", "Day64", "Day65", "Day66","Day67","Day68","Day69","Day70",
                                   "Day71", "Day72", "Day73", "Day74", "Day75", "Day76","Day77","Day78","Day79","Day80",
                                   "Day81", "Day82", "Day83", "Day84", "Day85", "Day86","Day87","Day88","Day89","Day90",
                                   "Day91", "Day92", "Day93", "Day94", "Day95", "Day96","Day97","Day98","Day99","Day100",
                                   "Day101", "Day102", "Day103", "Day104", "Day105", "Day106","Day107","Day108","Day109","Day110",
                                   "Day111", "Day112", "Day113", "Day114", "Day115", "Day116","Day117","Day118","Day119","Day120",
                                   "Day121", "Day122", "Day123", "Day124", "Day125", "Day126","Day127","Day128","Day129","Day130",
                                   "Day131", "Day132", "Day133", "Day134", "Day135", "Day136","Day137","Day138","Day139","Day140",
                                   "Day141", "Day142", "Day143", "Day144", "Day145", "Day146","Day147","Day148","Day149","Day150",
                                   "Day151", "Day152", "Day153", "Day154", "Day155", "Day156","Day157","Day158","Day159","Day160",
                                   "Day161", "Day162", "Day163", "Day164", "Day165", "Day166","Day167","Day168","Day169","Day170",
                                   "Day171", "Day172", "Day173", "Day174", "Day175", "Day176","Day177","Day178","Day179","Day180",
                                   "Day181", "Day182", "Day183", "Day184", "Day185", "Day186","Day187","Day188","Day189","Day190",
                                   "Day191", "Day192", "Day193", "Day194", "Day195", "Day196","Day197","Day198","Day199","Day200",
                                   "Day201", "Day202", "Day203", "Day204", "Day205", "Day206","Day207","Day208","Day209","Day210", 
                                   "Day211", "Day212")

#-----------------------------------------------------------------------------------------------------


#Remove the clm that have no data for  Rain_evap and add the coords

Rain_extract_wide <- dplyr::select(Rain_extract_wide, -"Day61", -"Day62", -"Day63", -"Day64", -"Day65", -"Day66" )
Rain_extract_wide_x_y <- dplyr::select(Rain_extract_wide, "POINT_X",  "POINT_Y")
#this won't work for select???
#Rain_extract_wide_values <- dplyr::select(Rain_extract_wide, day_input_rain) #this is the day I want results for
#Rain_on_break <- cbind(Rain_extract_wide_x_y, Rain_extract_wide_values)

Rain_on_break <- dplyr::mutate(Rain_extract_wide, 
                               x_y = paste0(POINT_X, "_", POINT_Y), 
                               site_name = site_name)
#colnames(Rain_on_break) <- c("POINT_X", "POINT_Y", year_input_rain, "x_y", 
#"site")

return(Rain_on_break)
}

for (i in list_year) {
  assign(paste0("Rain", i), function_rain_on_break(list_year, df, site))
}


####Hopefully this works an I will get value for df for each year -NOPE
### super not working
#join the dataset together
Rain_site <- rbind(Rain1, Rain2, Rain3, Rain4)
head(Rain_site)
view(Rain_site)
