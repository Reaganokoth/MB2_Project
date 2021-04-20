##################################################################################################################################
# THIS PROJECT USES FRACTION OF ABSORBED PHOTOSYNTHETICALLY ACTIVE RADIATION (FAPAR) TO DETECT DROUGHT YEARS IN KENYA ###########
##################################################################################################################################

install.packages("rasterVis")
install.packages("RColorBrewer")
install.packages("ggspatial")
install.packages("xts")
install.packages("dygraphs")
install.packages("hrbrthemes")
library(rvest)
library(xml2)
library(stringr)
library(RColorBrewer)
library(rasterVis)
library(foreach)
library(ggplot2)
library(ggspatial)
library(dplyr)
library(sf)
library(tibble)
library(lubridate)
library(xts) 
#library(dygraphs)
library(tidyr)
library(gganimate)
library(hrbrthemes)
library(patchwork)
setwd("/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/MB12/projects/R_project/")

#############################################################
#GETTING WARD LEVEL (5) ADMINISTRATIVE BOUNDARY FOR KENYA ###
#############################################################

#load admin shapefile and select Homa-bay as the area of interest.
# the file was downloaded from the following link 

#download.file(url = "https://data.humdata.org/dataset/e8d06ae7-740b-4491-8749-43f81700cf41/resource/858129b2-7197-4ffe-b34f-c2091b307b2c/download/kenya_wards.zip",destfile = paste0(getwd(),"KenyaAdminLevel5.zip"))

kenya_admin5 <- sf::st_read("/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/MB12/projects/R_project/Kenya_Wards/kenya_wards.shp")
ROI <-  ROI <- kenya_admin5[kenya_admin5$county == "Homa Bay", ]
plot(st_geometry(ROI))

##########################################################
#GETTING CLIMATE DATA temperature, precipitation
##########################################################

#The following function downloads data from worldclim.org 
# inputs: category , product, product_resolution, period, SSP, GCM
#input Details: 
# category: 
# 1. HCD (historical climate data) downloads 12 geotifs 1 for each month as.zip
#   product types: tmin, tmax,tavg, prec,srad,wind,vapr
#   requires the user to define resolution
#   possibilities are 10m, 5m,2.5m and 30s
# 2. HMWD (historical monthly climate data) data since 1960-2018(min/max temperature and precipitation)
#    product types available: tmin, tmax,prec.
#    requires user to define period of interest. 
#    available periods are: 1960-1969, 1970-1979,1980-1989, 1990-1999, 2000-2009, 2010-2018
# 3. FCD (future climate data) download downscaled monthly future climate data from CMIP6 for 9 GCMs and 4 SSPs
#    available GCMs: BCC-CSM2-MR, CNRM-CM6-1, CNRM-ESM2-1, CanESM5, GFDL-ESM4, IPSL-CM6A-LR, MIROC-ES2L, MIROC6, MRI-ESM2-0
#    available SSPs: 126, 245, 370 and 585.
#    available periods: (2021-2040, 241-2060, 2061-2080, 2081-2100)
#    available resolution: 10 minutes, 5 minutes, 2.5 minutes.
getWC_data <- function(category=NULL, product=NULL, Product_resolution=NULL, period=NULL, SSP=NULL, GCM=NULL ){
  worldclimlink <- "https://www.worldclim.org"
  page <- read_html(worldclimlink)
  #get data page url
  dataPageUrl <- page %>%
    html_nodes("a") %>%
    html_attr("href")
  dataPageUrl2 <-  str_split(string = dataPageUrl, pattern = " ")[[1]] %>% 
    str_sub(start = 2, end = -1) %>% 
    str_c(paste0(worldclimlink), .)
  #navigate to the page
  dataPage <- read_html(dataPageUrl2) %>% 
    html_nodes("a") %>%
    html_attr("href")
  #extract importantlinks
  patternList <- c("worldclim21", "monthlywth", "cmip6climate")
  # HC_data <- grep(pattern = patternList[1], x = dataPage, value = T)[[1]]
  # gsub(pattern = "index", HC_data,dataPageUrl2)
  
  urls <- lapply(patternList, function(eachPattern){
    gsub(pattern = "index", replacement = eachPattern,dataPageUrl2)
  })
  list_of_urls <- unlist(urls) 
  if(category=="HCD"){
    pattern <- paste0("_",Product_resolution,"m","_",product)
    download_list <- read_html(list_of_urls[[1]]) %>% 
      html_nodes("a") %>%
      html_attr("href") %>%
      str_subset(paste0(pattern, ".zip")) #%>%
    lapply(download_list,function(each){
      options(timeout=3600)
      download.file(url = each, destfile = paste0(getwd(),"/", pattern, ".zip"))
    })
    #return(download_list)
  }else {
    if(category=="HMWD"){
      pattern <- paste0("_",product,"_", period)
      download_list <- read_html(list_of_urls[[2]]) %>% 
        html_nodes("a") %>%
        html_attr("href") %>%
        str_subset(paste0(pattern, ".zip")) #%>%
      lapply(download_list,function(each){
        options(timeout=3600)
        download.file(url = each, destfile = paste0(getwd(),"/", pattern,".zip"), method = "auto", mode = "wb")
      })
    }else{
      pattern <- paste0(product,"_", GCM,"_",SSP,"_",period)
      download_list <- read_html(gsub(pattern ="climate",replacement = paste0("/cmip6_clim",Product_resolution,"m"),x = list_of_urls[[3]]))%>% 
        html_nodes("a") %>%
        html_attr("href") %>%
        str_subset(paste0(pattern, ".zip")) #%>%
      lapply(download_list,function(each){
        options(timeout=36000)
        download.file(url = each, destfile = paste0(getwd(),"/",pattern, ".zip", ))
      })
    }
  }
}


getWC_data(category = "HMWD", product = "tmin", period = "2000-2009")
getWC_data(category = "HMWD", product = "tmin", period = "2010-2018")

getWC_data(category = "HMWD", product = "tmax", period = "2000-2009")
getWC_data(category = "HMWD", product = "tmax", period = "2010-2018")

getWC_data(category = "HMWD", product = "prec", period = "2000-2009")
getWC_data(category = "HMWD", product = "prec", period = "2010-2018")

#############################################################
# Handling climate data
##############################################################
# set the path to download folder or 
# working directory containing the downloaded files
path <- "/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/MB12/projects/R_project/_tmax_2010-2018/"
tif_list <- list.files(path = "/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/MB12/projects/R_project/_tmax_2010-2018", pattern = ".tif")

# set pattern to isolate files based on months (contained in the file name). 
# This is to group the files into 2 rainy seasons in Kenya
# March April May and June (MAMJ) corresponds to long rains and 
# September October November December (SOND) the short rains 
pattern_MAMJ <- "([-][0]{1}[3-6]{1})"
pattern_SOND <-  "([-][1-2]{2})|([-][1][0])|([-][0]{1}[9]{1})"

# these functions organize the files into folder 
# based on season and return the desired period (MAMJ or SOND)
# then returns the files to the original folder

fileSorter <- function(path, year, period){
  tif_list <- list.files(path = path, pattern = ".tif")
  MAMJ <- grep(pattern = "([-][0]{1}[3-6]{1})",x = tif_list,value = T)
  SOND <- grep(pattern = "([-][1-2]{2})|([-][1][0])|([-][0]{1}[9]{1})",x = tif_list,value = T)
  years_MAMJ <- str_extract(string = MAMJ,pattern = "([0-9]{4})")
  years_SOND <- str_extract(string = SOND,pattern = "([0-9]{4})")
  
  if(period=="MAMJ"){
    foreach(i = MAMJ, y = years_MAMJ ) %do% {
      sapply(paste0(path, y), dir.create)
      if (isTRUE(as.numeric(str_extract(string = i, pattern = "([0-9]{4})")) == y)){
        
        library(filesstrings)
        file.move(files = paste0(path, i), destinations = paste0(path, y,"/"), overwrite = T)
        #file.copy(from =paste0(path, i),to =paste0(path, y,"/"),overwrite = T  )
      } 
      else {
        print("No Match")
      }
    }
  } else {
    foreach(i = SOND, y = years_SOND ) %do% {
      sapply(paste0(path, y), dir.create)
      if (isTRUE(as.numeric(str_extract(string = i, pattern = "([0-9]{4})")) == y)){
        
        library(filesstrings)
        file.move(files = paste0(path, i), destinations = paste0(path, y,"/"), overwrite = T)
        
      } 
      else {
        print("No Match")
      }
    }
  }
  files <- list.files(path = paste0(path, year,"/"), pattern = ".tif")
  if(period=="MAMJ"){
    foreach(i = MAMJ, y = years_MAMJ ) %do% {
      
      file.move(files = paste0(path, y,"/", i), destinations = paste0(path), overwrite = T)
    }
    foreach(i = MAMJ, y = years_MAMJ ) %do%{
      dir.remove(paste0(path,y))
    }
  } else {
    foreach(i = SOND, y = years_SOND ) %do% {
      
      file.move(files = paste0(path, y,"/", i), destinations = paste0(path), overwrite = T)
    }
    foreach(i = SOND, y = years_SOND ) %do%{
      dir.remove(paste0(path,y))
    }
  }
  #file_unsort(path = path,period = period,file_list = tif_list)
  return(files)
  break
}

# getAnualmean function computes annual mean for each period MAMJ and 
# SOND since 2000-2018 for temperature and Precipitation 
# and 2000-2010 for FAPAR data for each polygon in the area of interest
# returns a dataframe with mean values from each polygon in ROI

getAnualmean <- function(path, season, ROI, df=NULL){
  setwd(path)
  list_files <- list.files(path = path, pattern = ".tif")
  years <- unique(str_extract(string = list_files,pattern = "([0-9]{4})"))
  df <- data.frame(matrix(0, nrow = nrow(ROI), ncol = length(years)))
  
  for(i in 1:ncol(df)){
    
    if(season=="MAMJ"){
      file_list <- grep(pattern = pattern_MAMJ,x = list_files, value = T) %>% 
        grep(pattern = years[[i]], value = T)
      columnames <- unlist(lapply(years, function(each){
        paste0("MAMJ_mean_",each)
      }))
      names(df) <- columnames
      my_raster <- stack(file_list)
      my_raster <- crop(my_raster,ROI)  
      my_raster <- overlay(my_raster,fun=mean)
      names(my_raster) <- "mean"
      values <- raster::extract(my_raster, ROI,df=T, fun=mean)
      df[[i]] <- values[, "mean"]
    } else{
      file_list <- grep(pattern = pattern_SOND,x = list_files, value = T) %>% 
        grep(pattern = years[[i]], value = T)
      columnames <- unlist(lapply(years, function(each){
        paste0("SOND_mean_",each)
      }))
      names(df) <- columnames
      my_raster <- stack(file_list)
      my_raster <- crop(my_raster,ROI)  
      my_raster <- overlay(my_raster,fun=mean)
      names(my_raster) <- "mean"
      values <- raster::extract(my_raster, ROI,df=T, fun=mean)
      df[[i]] <- values[, "mean"]
    }
  }
  return(df)
}

# renames columns of a dataframe to desired names
colRename <- function(df,product, period){
  years <- str_extract(string = names(df),pattern = "([0-9]{4})")
  layernames <- paste0(period,"_",product,"_mean_",years[1]:years[length(years)])
  names(df) <- layernames
  return(df)
}


# Annual mean maximum temperature 2000-2018
path_tmax <- "/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/MB12/projects/R_project/data/climate_data/Temperature/tmax_2000-2018"
MAMJ_tmax_mean_2000_2018 <- getAnualmean(path = path_tmax,season = "MAMJ",ROI = ROI) %>% 
  colRename(product = "tmax",period = "MAMJ")

SOND_tmax_mean_2000_2018 <- getAnualmean(path = path_tmax,season = "SOND",ROI = ROI) %>% 
  colRename(product = "tmax",period = "SOND")

# Annual mean minimum temperature 2000-2018
path_tmin <- "/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/MB12/projects/R_project/data/climate_data/Temperature/tmin_2000-2018"
MAMJ_tmin_2000_2018_mean <- getAnualmean(path = path_tmin,season = "MAMJ",ROI = ROI) %>% 
  colRename(product = "tmin",period = "MAMJ")

SOND_tmin_2000_2018_mean <- getAnualmean(path = path_tmin,season = "SOND",ROI = ROI) %>% 
  colRename(product = "tmin",period = "SOND")

# Annual mean precipitation 2000-2018
path_prec <- "/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/MB12/projects/R_project/data/climate_data/Precipitation"

MAMJ_prec_2000_2018_mean <- getAnualmean(path = path_prec,season = "MAMJ",ROI = ROI) %>% 
  colRename(product = "prec",period = "MAMJ")

SOND_prec_2000_2018_mean <- getAnualmean(path = path_prec,season = "SOND",ROI = ROI) %>% 
  colRename(product = "prec",period = "SOND")

################################################################################################################
# Calculating the overall mean for the entire period (2000-2018) per season and for each climate parameter ####
###############################################################################################################

# compositeRaster function  crops the images to ROI and assigns projection then 
# composites a list of raster images to a single image. 
# It computes the mean of all the pixels within each polygon in the ROI
# returns a raster or sf multipolygon object.

compositeRaster <- function(path, bbox, period, df, epsg){
  setwd(path)
  if(period=="MAMJ"){
    list_files <- list.files(path = path, pattern = pattern_MAMJ)
    x <- stack(list_files)
    x <- crop(x,ROI)
    x <- overlay(x,fun=mean)
  } else{
    list_files <- list.files(path = path, pattern = pattern_SOND)
    x <- stack(list_files)
    x <- crop(x,ROI)
    x <- overlay(x,fun=mean)
  }
  if(df==T){
    #st_as_sf(x)
    return(st_as_sf(raster::extract(x, bbox, df=T, fun=mean, sp=1)[,1:9]) %>% 
             st_transform(st_crs(epsg)))
  }else{
    return(x)
  }
  
}

# Tmax 2000-2018 
MAMJ_tmax_Overal_mean_2000_2018 <- compositeRaster(path = path_tmax,period = "MAMJ",bbox = ROI,df = T,epsg = 21097)
SOND_tmax_Overal_mean_2000_2018 <- compositeRaster(path = path_tmax,period = "SOND",bbox = ROI,df = T,epsg = 21097)

# Tmin 2000-2018 

MAMJ_tmin_Overal_mean_2000_2018 <- compositeRaster(path = path_tmin,period = "MAMJ",bbox = ROI,df = T,epsg = 21097)
SOND_tmin_Overal_mean_2000_2018 <- compositeRaster(path = path_tmin,period = "SOND",bbox = ROI,df = T,epsg = 21097)

# Prec 2000-2018

MAMJ_prec_Overal_mean_2000_2018 <- compositeRaster(path = path_prec,period = "MAMJ",bbox = ROI,df = T,epsg = 21097)
SOND_prec_Overal_mean_2000_2018 <- compositeRaster(path = path_prec,period = "SOND",bbox = ROI,df = T,epsg = 21097)

# Extract  the overall mean i.e the entire 2000-2018 period (layer column) for all the climate parameters for each season into a data-frame, 
# Then join the seasonal data-frames by row to get single data-frame with the overal means for the entire period
MAMJ_Overal <- data.frame(cbind(MAMJ_tmin_Overal_mean_2000_2018$layer,MAMJ_tmax_Overal_mean_2000_2018$layer,MAMJ_prec_Overal_mean_2000_2018$layer)) 
MAMJ_Overal_names <- c("MAMJ_tmin_Overal_mean_2000_2018","MAMJ_tmax_Overal_mean_2000_2018","MAMJ_prec_Overal_mean_2000_2018")
names(MAMJ_Overal) <- MAMJ_Overal_names

SOND_Overal <- data.frame(cbind(SOND_tmin_Overal_mean_2000_2018$layer,SOND_tmax_Overal_mean_2000_2018$layer,SOND_prec_Overal_mean_2000_2018$layer)) 
SOND_Overal_names <- c("SOND_tmin_Overal_mean_2000_2018","SOND_tmax_Overal_mean_2000_2018","SOND_prec_Overal_mean_2000_2018")
names(SOND_Overal) <- SOND_Overal_names

# Combine the annual means for each parameter with the overall period mean into a single data-frame for each season
# Then combine the seasons data into a single master dataframe.
MAMJ_OvralDF <- cbind(MAMJ_tmin_2000_2018_mean,MAMJ_tmax_mean_2000_2018,MAMJ_prec_2000_2018_mean,MAMJ_Overal)
SOND_OvralDF <- cbind(SOND_tmin_2000_2018_mean,SOND_tmax_mean_2000_2018,SOND_prec_2000_2018_mean,SOND_Overal)
names(SOND_OvralDF)

masterDF <- cbind(MAMJ_OvralDF,SOND_OvralDF)
names(masterDF)


#################################
##    VEGETATION DATA        ###
################################

#FAPAR DATA

nc_path <- "/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/MB12/projects/R_project/data/FAPAR2000_2010"


# netCDF_process function lists netcdf (.nc) files present in the nc_path(netcdf file directory) and create a raster stack. 
# It crops each file to an area of interest based on a polygon extent (ROI)
# It can return a composite of all the netcdf files (mean composite) as multi-polygon data-frame which is a sp object
# or a mean value per each polygon in the ROI for each .nc file 
# or a raster stack with nlayers= length of the .nc files supplied.
netCDF_process <- function(path, period, ROI, overlay, sp, epsg=NULL){
  setwd(path)
  nc_files <- list.files(path = path, pattern = ".nc")
  nc_files <- list.files(path = nc_path, pattern = ".nc")
  MAMJ_FAPAR_pattern <- "([3]{2}[1]{1})|([0]{1}[4]{1}[3])|([0]{1}[4]{1}[2])"
  MAMJ_FAPAR_files <-grep(x = nc_files, pattern = MAMJ_FAPAR_pattern, value = T)
  # -11 to remove double entry in 2009
  SOND_FAPAR_files <- setdiff(x =nc_files,y = MAMJ_FAPAR_files ) [-10]
  if(period=="MAMJ"){
    files <-MAMJ_FAPAR_files
    years <- stringr::str_extract(string = files,pattern = "([0-9]{4})")
    layernames <- unlist(lapply(years, function(each){
      paste0("MAMJ_FAPAR_", each)
    }))
    my_raster <- stack(files)
    my_raster <- crop(x = my_raster,y = extent(ROI))
    names(my_raster) <- layernames
    
  } else{
    files <-SOND_FAPAR_files
    years <- stringr::str_extract(string = files,pattern = "([0-9]{4})")
    layernames <- unlist(lapply(years, function(each){
      paste0("SOND_FAPAR_", each)
    }))
    my_raster <- stack(files)
    my_raster <- crop(x = my_raster,y = extent(ROI))
    names(my_raster) <- layernames
  }
  if(sp==1& overlay==F){
    sp_object <- extract(my_raster, ROI, df=T,sp=1,fun=mean) 
    sp_object <- sf::st_as_sf( sp_object) 
    sp_object <- sf::st_transform(sp_object,sf::st_crs(epsg))
    return(sp_object)
  }else{
    if(overlay==T & sp==1){
      sp_object <- overlay(my_raster, fun=mean)
      names(sp_object) <- "Overal_FAPAR_mean"
      sp_object <- extract(sp_object, ROI, df=T,sp=1,fun=mean)
      sp_object <- sf::st_as_sf(sp_object)
      return(sf::st_transform(sp_object,sf::st_crs(epsg)))
    } else{
      return(my_raster)
    }
  }
  
}

# Compute mean FAPAR per polygon for each year and each season for period 2000-2010

# MAMJ period
MAMJ_FAPAR_anual_mean <- netCDF_process(path = nc_path,period = "MAMJ",ROI = ROI,sp = 1,epsg = 21097,overlay = F)

# SOND period
SOND_FAPAR_anual_mean <- netCDF_process(path = nc_path,period = "SOND",ROI = ROI,sp = 1,epsg = 21097, overlay = F)


# Compute overal mean FAPAR per ward (polygon) per season for the period 2000-2010

# MAMJ period
MAMJ_FAPAR_overal_mean <- netCDF_process(path = nc_path,period = "MAMJ",ROI = ROI,sp = 1,epsg = 21097,overlay = T)

#SOND period
SOND_FAPAR_overal_mean <- netCDF_process(path = nc_path,period = "SOND",ROI = ROI,sp = 1,epsg = 21097,overlay = T)

##################################
# Anomaly analysis           ###
##################################

# Calculating Relative (R) anomaly for each variable per season
# R(i,k) = [P(i,k)-P(i)mean]/P(i)mean where P=variable(i.e T FAPAR, prec) i= season and k= year

# AnomalyCal function takes in a dataframe or sf object 
# and computes annual anomaly based on the above formula
# It requires two datasets, x = annual means and y = overall period mean for the season.

AnomalyCal <- function(x,y, type, period, variable){
  colNames <- names(x[9:ncol(x)])
  colNames <- colNames[-length(colNames)]
  years <- unlist(lapply(colNames, function(each){
    stringr::str_extract(string = each,pattern = "([0-9]{4})")
  }))
  anomalyColNames <- unlist(lapply(years, function(each){
    paste0("MAMJ_R_FAPAR_",each)
  }))
  #df <- data.frame(matrix(1, nrow = nrow(x), ncol = length(colNames)))
  
  if(variable=="FAPAR" & period=="MAMJ" & type==stringr::str_to_title("r")){
    anomalyColNames <- unlist(lapply(years, function(each){
      paste0("MAMJ_R_FAPAR_",each)
    }))
    data <-x[9:ncol(x)]
    data <- as.data.frame(data)
    data <- data[-length(data)]
    overal_mean <- as.data.frame(y[9])[-2]
    
  }else{
    if(variable=="FAPAR" & period=="SOND" & type==stringr::str_to_title("r")){
      anomalyColNames <- unlist(lapply(years, function(each){
        paste0("SOND_R_FAPAR_",each)
      }))
      data <-x[9:ncol(x)]
      data <- as.data.frame(data)
      data <- data[-length(data)]
      overal_mean <- as.data.frame(y[9])[-2]
    }else{
      if(variable=="prec" & period=="MAMJ" & type==stringr::str_to_title("r")){
        colNames <- names(x)
        #colNames <- colNames[-length(colNames)]
        years <- unlist(lapply(colNames, function(each){
          stringr::str_extract(string = each,pattern = "([0-9]{4})")
        }))
        anomalyColNames <- unlist(lapply(years, function(each){
          paste0("MAMJ_R_Prec_",each)
        }))
        data <-x
        # data <- as.data.frame(data)
        # data <- data[-length(data)]
        overal_mean <- as.data.frame(y[9])[-2]
      }else {
        if (variable=="prec" & period=="SOND" & type==stringr::str_to_title("r")){
          colNames <- names(x)
          #colNames <- colNames[-length(colNames)]
          years <- unlist(lapply(colNames, function(each){
            stringr::str_extract(string = each,pattern = "([0-9]{4})")
          }))
          anomalyColNames <- unlist(lapply(years, function(each){
            paste0("SOND_R_Prec_",each)
          }))
          data <-x
          #data <- as.data.frame(data)
          #data <- data[-length(data)]
          overal_mean <- as.data.frame(y[9])[-2]
        }else{
          if(variable=="tmin" & period=="MAMJ" & type==stringr::str_to_title("r")){
            colNames <- names(x)
            #colNames <- colNames[-length(colNames)]
            years <- unlist(lapply(colNames, function(each){
              stringr::str_extract(string = each,pattern = "([0-9]{4})")
            }))
            anomalyColNames <- unlist(lapply(years, function(each){
              paste0("MAMJ_R_tmin_",each)
            }))
            data <-x
            #data <- as.data.frame(data)
            #data <- data[-length(data)]
            overal_mean <- as.data.frame(y[9])[-2]
          }else{
            if(variable=="tmin" & period=="SOND" & type==stringr::str_to_title("r")){
              colNames <- names(x)
              #colNames <- colNames[-length(colNames)]
              years <- unlist(lapply(colNames, function(each){
                stringr::str_extract(string = each,pattern = "([0-9]{4})")
              }))
              anomalyColNames <- unlist(lapply(years, function(each){
                paste0("SOND_R_tmin_",each)
              }))
              data <-x
              #data <- as.data.frame(data)
              #data <- data[-length(data)]
              overal_mean <- as.data.frame(y[9])[-2]
            }else{
              if(variable=="tmax" & period=="MAMJ" & type==stringr::str_to_title("r")){
                colNames <- names(x)
                #colNames <- colNames[-length(colNames)]
                years <- unlist(lapply(colNames, function(each){
                  stringr::str_extract(string = each,pattern = "([0-9]{4})")
                }))
                anomalyColNames <- unlist(lapply(years, function(each){
                  paste0("MAMJ_R_tmax_",each)
                }))
                data <-x
                #data <- as.data.frame(data)
                #data <- data[-length(data)]
                overal_mean <- as.data.frame(y[9])[-2]
              } else{
                colNames <- names(x)
                #colNames <- colNames[-length(colNames)]
                years <- unlist(lapply(colNames, function(each){
                  stringr::str_extract(string = each,pattern = "([0-9]{4})")
                }))
                anomalyColNames <- unlist(lapply(years, function(each){
                  paste0("SOND_R_tmax_",each)
                }))
                data <-x
                #data <- as.data.frame(data)
                #data <- data[-length(data)]
                overal_mean <- as.data.frame(y[9])[-2]
              }
            }
          }
          
        }
      }
      
    }
  }
  df.new = as.data.frame(lapply(data, function(each) {
    (each-overal_mean)/overal_mean
  }))
  names(df.new) <- anomalyColNames
  return(df.new)
}

#Tmax Relative Anomaly
MAMJ_Tmax_anual_Relative_Anomaly <- AnomalyCal(x =MAMJ_tmax_mean_2000_2018,
                                                y=MAMJ_tmax_Overal_mean_2000_2018,
                                                type = "R",
                                                period = "MAMJ",
                                                variable = "tmax")

SOND_Tmax_anual_Relative_Anomaly <- AnomalyCal(x =SOND_tmax_mean_2000_2018,
                                                y=SOND_tmax_Overal_mean_2000_2018,
                                                type = "R",
                                                period = "SOND",
                                                variable = "tmax")


#Tmin Relative Anomaly
MAMJ_Tmin_anual_Relative_Anomaly <- AnomalyCal(x =MAMJ_tmin_2000_2018_mean,
                                               y=MAMJ_tmin_Overal_mean_2000_2018,
                                               type = "R",
                                               period = "SOND",
                                               variable = "tmin")

SOND_Tmin_anual_Relative_Anomaly <- AnomalyCal(x =SOND_tmin_2000_2018_mean,
                                               y=SOND_tmin_Overal_mean_2000_2018,
                                               type = "R",
                                               period = "SOND",
                                               variable = "tmin")

#Precipitation Relative Anomaly
MAMJ_Prec_anual_Relative_Anomaly <- AnomalyCal(x =MAMJ_prec_2000_2018_mean,
                                               y=MAMJ_prec_Overal_mean_2000_2018,
                                               type = "R",
                                               period = "MAMJ",
                                               variable = "prec")

SOND_Prec_anual_Relative_Anomaly <- AnomalyCal(x =SOND_prec_2000_2018_mean,
                                               y=SOND_prec_Overal_mean_2000_2018,
                                               type = "R",
                                               period = "SOND",
                                               variable = "prec")

#FAPAR relative Anomaly
MAMJ_FAPAR_anual_Relative_Anomaly <- AnomalyCal(x =MAMJ_FAPAR_anual_mean,
                                           y=MAMJ_FAPAR_overal_mean,
                                           type = "R",
                                           period = "MAMJ",
                                           variable = "FAPAR")


SOND_FAPAR_anual_Relative_Anomaly <- AnomalyCal(x =SOND_FAPAR_anual_mean,
                                                y=SOND_FAPAR_overal_mean,
                                                type = "R",
                                                period = "SOND",
                                                variable = "FAPAR")




#compute overall mean for all the parameters for each season and store into a 
# single data-frame including FAPAR
colname <- c("Prec_mean","Tmax_mean", "Tmin_mean", "FAPAR_mean")
MAMJ_season_mean <- as.data.frame(cbind(MAMJ_prec_Overal_mean_2000_2018[, 9:10],
                                       MAMJ_tmax_Overal_mean_2000_2018[[9]],
                                       MAMJ_tmin_Overal_mean_2000_2018[[9]],
                                       MAMJ_FAPAR_overal_mean[[9]]))

names(MAMJ_season_mean) <- c("Prec_mean","Tmax_mean", "Tmin_mean", "FAPAR_mean", "geometry")
MAMJ_season_mean$Season <- "MAMJ"



SOND_season_Mean <- as.data.frame(cbind(SOND_prec_Overal_mean_2000_2018[, 9:10],
                                        SOND_tmax_Overal_mean_2000_2018[[9]],
                                        SOND_tmin_Overal_mean_2000_2018[[9]],
                                        SOND_FAPAR_overal_mean[[9]]))


SOND_season_Mean$Season <- "SOND"
names(SOND_season_Mean) <- names(MAMJ_season_mean)


Overall_mean_df <- rbind(MAMJ_season_mean,SOND_season_Mean) 

spatialPolygonDF <- st_as_sf(Overall_mean_df) %>% 
  st_transform(crs = st_crs(21097))


wideTolongDF <- function(x,variable){
  if(variable=="FAPAR"){
    x <- as.data.frame(x[9:19])
    x <- x[-length(x)]
    names <- names(x)
    endYear <- str_extract(string = names[length(names)],pattern = "[0-9]{4}")
    names <- names(x)[-length(x)]
    df <- unlist(lapply(as.data.frame(x), function(each){
      rbind(each)
    }))
    df <- data.frame(df)
    Year <- rep(2000:endYear, each=40)
    df1 <- df[[1]]
   
    df3 <- data.frame(cbind(df1,Year))
  }else{
    if(variable=="FAPAR_anomaly"){
      x <- as.data.frame(x)
      x <- x[-length(x)]
      names <- names(x)
      endYear <- as.double(str_extract(string = names[length(x)],pattern = "[0-9]{4}"))
      df <- unlist(lapply(x, function(each){
        rbind(each)
      }))
      df <- data.frame(df)
      Year <- rep(2000:endYear, each=40)
      df1 <- df[[1]]
      df3 <- data.frame(cbind(df1,Year))
    }else{
      x <- as.data.frame(x)
      #x <- x[-length(x)]
      names <- names(x)
      endYear <- str_extract(string = names[length(x)],pattern = "[0-9]{4}")
      df <- unlist(lapply(x, function(each){
        rbind(each)
      }))
      df <- data.frame(df)
      Year <- rep(2000:endYear, each=40)
      df1 <- df[[1]]
      df3 <- data.frame(cbind(df1,Year))
    }
    
  }
  return(df3)
}

#converting climate data from wide to long datasets per season
#and then creating a single master dataframe

MAMJ_FAPAR_Long <- wideTolongDF(x = MAMJ_FAPAR_anual_mean, "FAPAR")
MAMJ_tmax_long <- wideTolongDF(x = MAMJ_tmax_mean_2000_2018,variable = "tmax")
MAMJ_tmin_long <- wideTolongDF(x = MAMJ_tmin_2000_2018_mean,variable = "tmin")
MAMJ_prec_long <- wideTolongDF(x = MAMJ_prec_2000_2018_mean,"prec")

MAMJ_Anual_mean_Long <- cbind(MAMJ_tmin_long[1],MAMJ_tmax_long[1],MAMJ_prec_long)
MAMJ_Anual_mean_Long$Season <- "MAMJ"


SOND_FAPAR_Long <- wideTolongDF(x = SOND_FAPAR_anual_mean, "FAPAR")
SOND_tmax_long <- wideTolongDF(x = SOND_tmax_mean_2000_2018,variable = "tmax")
SOND_tmin_long <- wideTolongDF(x = SOND_tmin_2000_2018_mean,variable = "tmin")
SOND_prec_long <- wideTolongDF(x = SOND_prec_2000_2018_mean,"prec")

SOND_Anual_mean_Long <- cbind(SOND_tmin_long[1],SOND_tmax_long[1],SOND_prec_long)
SOND_Anual_mean_Long$Season <- "SOND"

FAPAR_anual_mean_Long <- rbind(MAMJ_FAPAR_Long,SOND_FAPAR_Long)

#combine the MAMJ and SOND to make a single dataframe (long version) 2000-2018
master_ANNUAL_meanDF_Long <- rbind(MAMJ_Anual_mean_Long,SOND_Anual_mean_Long)
names <- c("Tmin", "Tmax", "Prec", "Year", "Season")
names(master_ANNUAL_meanDF_Long) <- names
head(master_ANNUAL_meanDF_Long)
tail(master_ANNUAL_meanDF_Long)

# seubset the master_ANNUAL_meanDF_Long data to retain only 2000-2010
# Since FAPAR data is till this time frame and add FAPAR data to the df

master_ANNUAL_meanDF_Long <- master_ANNUAL_meanDF_Long[master_ANNUAL_meanDF_Long$Year < 2011, ] %>% 
  add_column(
    FAPAR_anual_mean_Long[1],
    .after = "Prec",
  ) 
new_names <- c("Tmin", "Tmax", "Prec","FAPAR", "Year", "Season")
names(master_ANNUAL_meanDF_Long) <- new_names


############################################
#convert anomaly from wide to long df #####
###########################################

#Tmax anomaly to long
MAMJ_Tmax_anual_Anomaly_Long <- wideTolongDF(x = MAMJ_Tmax_anual_Relative_Anomaly,variable = "tmax")
MAMJ_Tmax_anual_Anomaly_Long$Season <- "MAMJ"

SOND_Tmax_anual_Anomaly_Long <- wideTolongDF(x = SOND_Tmax_anual_Relative_Anomaly,variable = "tmax")
SOND_Tmax_anual_Anomaly_Long$Season <- "SOND"

Tmax_anual_Anomaly_Long <- rbind(MAMJ_Tmax_anual_Anomaly_Long,SOND_Tmax_anual_Anomaly_Long)
#subset to retain values for 2000-2010
Tmax_anual_Anomaly_Long2000_2010_DF <- Tmax_anual_Anomaly_Long[Tmax_anual_Anomaly_Long$Year < 2011, ]
#Prec anomaly to long
MAMJ_Prec_anual_Anomaly_Long <- wideTolongDF(x = MAMJ_Prec_anual_Relative_Anomaly,variable = "tmax")
MAMJ_Prec_anual_Anomaly_Long$Season <- "MAMJ"

SOND_Prec_anual_Anomaly_Long <- wideTolongDF(x = SOND_Prec_anual_Relative_Anomaly,variable = "tmax")
SOND_Prec_anual_Anomaly_Long$Season <- "SOND"


Prec_anual_Anomaly_Long <- rbind(MAMJ_Prec_anual_Anomaly_Long,SOND_Prec_anual_Anomaly_Long)
#subset to retain values for 2000-2010
Prec_anual_Anomaly_Long2000_2010_DF <- Prec_anual_Anomaly_Long[Prec_anual_Anomaly_Long$Year < 2011, ]

#FAPAR Anomaly to long
MAMJ_FAPAR_anual_Anomaly_Long <- wideTolongDF(x = MAMJ_FAPAR_anual_Relative_Anomaly,variable = "anomaly")
MAMJ_FAPAR_anual_Anomaly_Long$Season <- "MAMJ"

SOND_FAPAR_anual_Anomaly_Long <- wideTolongDF(x = SOND_FAPAR_anual_Relative_Anomaly,variable = "anomaly")
SOND_FAPAR_anual_Anomaly_Long$Season <- "SOND"

FAPAR_anual_Anomaly_Long <- rbind(MAMJ_FAPAR_anual_Anomaly_Long,SOND_FAPAR_anual_Anomaly_Long)


#Create a master relative anual anomaly for all the variables
anomaly_DF_2000_2010_Long <- cbind(Tmax_anual_Anomaly_Long2000_2010_DF[1],
                                    Prec_anual_Anomaly_Long2000_2010_DF[1],
                                    FAPAR_anual_Anomaly_Long[1:3])

colNames <-c("Tmax_anomaly", "Prec_anomaly","FAPAR_anomaly", "Year", "Season") 

names(anomaly_DF_2000_2010_Long) <- colNames
head(anomaly_DF_2000_2010_Long)
tail(anomaly_DF_2000_2010_Long)

#Make a single df with 2000-2010 means and anomalies 2000-2010
Master_mean_AND_anomaly_DF <- master_ANNUAL_meanDF_Long %>% 
  add_column(
    anomaly_DF_2000_2010_Long[1:3],
    .after = "FAPAR",
  ) 

MastercolumnNames <- c("Tmin_mean",
                       "Tmax_mean" ,
                       "Prec_mean",
                       "FAPAR_mean",
                       "Tmax_anomaly", 
                       "Prec_anomaly",
                       "FAPAR_anomaly",
                       "Year",   
                       "Season")

names(Master_mean_AND_anomaly_DF) <- MastercolumnNames

# agregate the data by year to get mean values for each variable in each year for visualization
# exclude season column because it is categorical

Master_mean_AND_anomaly_DF_agg <- aggregate(x = Master_mean_AND_anomaly_DF[-ncol(Master_mean_AND_anomaly_DF)],                
                                          by =list(Master_mean_AND_anomaly_DF$Year) ,              
                                          FUN = mean)

# convert the years column to a date object 
# a rondom month and date will be assigned but i am interested in just years part

years <- c(Master_mean_AND_anomaly_DF_agg$Year)
years <- as.Date(as.character(years), format = "%Y")
str(years)

##############
# PLOTTING ###
##############

# animation showing FAPAR trend through 2000-2010
# in the animation, major droughts years in Kenya are visible
# the worst drought of 1999- early 2001, 2004-2006 and 2008-2009 droughts
p1 <- ggplot(
  Master_mean_AND_anomaly_DF_agg,
  aes(years, FAPAR_mean)
) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Years", y = "Fraction of Absorbed \n Photosynthetically Active Radiation") +
  theme(legend.position = "top")+
  scale_x_date(date_labels = "%Y",date_breaks = "1 year")
p1 + transition_reveal(years) +
  theme_ipsum()


# visualizing FAPAR behavior with temperature

p2 <- ggplot(MAMJ_FAPAR_overal_mean) + 
  geom_sf(data = ROI,
          aes(fill = MAMJ_FAPAR_overal_mean$Overal_FAPAR_mean), 
          alpha = 0.8)+
  theme(
    panel.background = element_rect(colour = "black",
                                    size = 0.5, linetype = "solid"))+
  scale_fill_gradientn(colors = rev(viridis::inferno(8)[5:7]), guide = 
                         guide_colorbar(
                           title = "FAPAR MAMJ",
                           title.position = "top",
                           title.hjust = 0.5,
                           barwidth = unit(4, "cm"),
                           barheight = unit(0.2, "cm"),
                           direction = "horizontal",
                           #frame.colour = NULL 
                         )) +
  annotation_scale(location = "bl")+
  annotation_north_arrow(which_north = "grid",
                         location = "tl",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_x = unit(0.5, "cm"),
                         pad_y = unit(0.8, "cm"))+
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("A")+
  theme(
    panel.background = element_rect( fill = "grey90" ,
                                     colour = "black",
                                     size = 0.5, 
                                     linetype = "solid"), 
    legend.position = c(.79, .20),
    plot.title = element_text(hjust = 0))
p2


p3 <- ggplot(SOND_FAPAR_overal_mean) + 
  geom_sf(data = ROI,
          aes(fill = SOND_FAPAR_overal_mean$Overal_FAPAR_mean), 
          alpha = 0.8)+
  theme(
    panel.background = element_rect(colour = "black",
                                    size = 0.5, linetype = "solid"))+
  scale_fill_gradientn(colors = rev(viridis::inferno(8)[5:7]), guide = 
                         guide_colorbar(
                           title = "FAPAR SOND",
                           title.position = "top",
                           title.hjust = 0.5,
                           barwidth = unit(4, "cm"),
                           barheight = unit(0.2, "cm"),
                           direction = "horizontal",
                           #frame.colour = NULL 
                         )) +
  annotation_scale(location = "bl")+
  annotation_north_arrow(which_north = "grid",
                         location = "tl",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_x = unit(0.5, "cm"),
                         pad_y = unit(0.8, "cm"))+
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("B")+
  theme(
    panel.background = element_rect( fill = "grey90" ,
                                     colour = "black",
                                     size = 0.5, 
                                     linetype = "solid"), 
    legend.position = c(.79, .20),
    plot.title = element_text(hjust = 0))
p3

#temeprature plot
p4 <- ggplot(MAMJ_tmax_Overal_mean_2000_2018) + 
  geom_sf(data = ROI,
          aes(fill = MAMJ_tmax_Overal_mean_2000_2018$layer), 
          alpha = 0.8)+
  theme(
    panel.background = element_rect(colour = "black",
                                    size = 0.5, linetype = "solid"))+
  scale_fill_gradientn(colors = rev(viridis::inferno(8)[5:7]), guide = 
                         guide_colorbar(
                           title = "Temperature MAMJ",
                           title.position = "top",
                           title.hjust = 0.5,
                           barwidth = unit(4, "cm"),
                           barheight = unit(0.2, "cm"),
                           direction = "horizontal",
                           #frame.colour = NULL 
                         )) +
  annotation_scale(location = "bl")+
  annotation_north_arrow(which_north = "grid",
                         location = "tl",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_x = unit(0.5, "cm"),
                         pad_y = unit(0.8, "cm"))+
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("C")+
  theme(
    panel.background = element_rect( fill = "grey90" ,
                                     colour = "black",
                                     size = 0.5, 
                                     linetype = "solid"), 
    legend.position = c(.79, .20),
    plot.title = element_text(hjust = 0))



p5 <- ggplot(SOND_tmax_Overal_mean_2000_2018) + 
  geom_sf(data = ROI,
          aes(fill = SOND_tmax_Overal_mean_2000_2018$layer), 
          alpha = 0.8)+
  theme(
    panel.background = element_rect(colour = "black",
                                    size = 0.5, linetype = "solid"))+
  scale_fill_gradientn(colors = rev(viridis::inferno(8)[5:7]), guide = 
                         guide_colorbar(
                           title = "Temperature SOND",
                           title.position = "top",
                           title.hjust = 0.5,
                           barwidth = unit(4, "cm"),
                           barheight = unit(0.2, "cm"),
                           direction = "horizontal",
                           #frame.colour = NULL 
                         )) +
  annotation_scale(location = "bl")+
  annotation_north_arrow(which_north = "grid",
                         location = "tl",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_x = unit(0.5, "cm"),
                         pad_y = unit(0.8, "cm"))+
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("D")+
  theme(
    panel.background = element_rect( fill = "grey90" ,
                                     colour = "black",
                                     size = 0.5, 
                                     linetype = "solid"), 
    legend.position = c(.79, .20),
    plot.title = element_text(hjust = 0))

# Grd the plots and set plot annotation using patchwork library

Tplots <- (p2 + p3 + p4 + p5) 

library(patchwork)

#png(filename = "TEMP_FAPAR_Plot.png",width = 1200, height = 800)

Tplots + plot_annotation(
  title = 'Variation of Fraction of Absorbed Photosynthetically Active Radiation (FAPAR) \nwith Temperature during Long and Short Rain Seasons in Homa-Bay County Kenya',
  theme = theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), 
                plot.caption = element_text(hjust = 0)),
  subtitle = 'Long Rains:March-June (MAMJ) , Short Rains: September-December (SOND)',
  caption = 'A: Mean Temperature during Long Rains, \nB: Mean Temperature during Short Rains \nC: Mean FAPAR during Long Rains \nD: Mean FAPAR during Short Rains'
)

#dev.off()



# Plot 2 Temperature anomaly vs FAPAR anomaly
# there is a positive FAPAR anomaly (FAPAR exceeding the decade (2000-2010) average) 
# when the temperature anomly is -ve
# positive temperature anomalies correcpond to the drought years

#set some constants
FAPARColor <- "green"
temperatureColor <- "red"

p6 <- ggplot(Master_mean_AND_anomaly_DF_agg, aes(x=years)) +
  geom_line( aes(y=Tmax_anomaly), size=0.5, color=temperatureColor) + # Divide by 10 to get the same range than the temperature
  geom_line( aes(y=FAPAR_anomaly/10),size=0.5, color=FAPARColor) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "FAPAR Anomaly",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*10, name="Temperature (ºC)")
  ) +
  scale_x_date(date_labels = "%Y",date_breaks = "1 year")+
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = FAPARColor, size=13),
    axis.title.y.right = element_text(color = temperatureColor, size=13),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(hjust = 0.5)
    
  ) +
  
  ggtitle("Temperature Anomly Vs FAPAR Anomaly 2000-2010 in Homa-Bay County Kenya") +
  xlab(label = "Year")
p6

############################### END #######################################




