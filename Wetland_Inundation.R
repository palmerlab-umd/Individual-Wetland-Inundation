###################################################################################
#Name: Individual Wetland Inundation
#Coder: C. Nathan Jones
#Date: 12/6/2017
#Purpose: Delineate Individual Wetland and Estimate Volume-Stage Relationships
###################################################################################

####################################################################################
#Step 1: Setup Workspace------------------------------------------------------------
####################################################################################
#Set Run Priority
library(tools)
psnice(pid=Sys.getpid(),value=15)

#Clear Memory
rm(list=ls(all=TRUE))

#Set working directory
setwd("/nfs/njones-data/Research Projects/Individual-Wetland-Inundation/")

#Add appropriate libararies
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(magrittr)

#Gather Required Data
dem.grd<-raster("/nfs/palmer-group-data/Choptank/Data/dem/dem")
catchment.shp<-readOGR("/nfs/palmer-group-data/Choptank/Data/wetland_basin/basins.shp")
pnt<-matrix(c(-75.83447, 39.054562), nrow=1)

####################################################################################
#Step 2: Inundate Wetland-----------------------------------------------------------
####################################################################################
#Convert point to spatialpoint
pnt.shp<-SpatialPoints(coords = pnt, proj4string = dem.grd@crs)

#Select previously defined catchment and crop DEM
catchment.shp<-catchment.shp[pnt.shp,]
dem.grd<-crop(dem.grd, catchment.shp)
dem.grd<-mask(dem.grd, catchment.shp)

#Create Minimum Raster
temp_min.grd<-dem.grd*0+minValue(dem.grd)
  
#Create function to return conditional raster 
Con<-function(condition, trueValue, falseValue){
  return(condition * trueValue + (!condition)*falseValue)
}
  
#Create function to calculate inundation area/volume
inundate<-function(z){
  area<-Con(dem.grd>(temp_min.grd+z),0,1)
  volume<-(((z+dem.grd)-temp_min.grd)*area)*res(area)[1]*res(area)[2]
  outflow<-cellStats(area*boundaries(temp_min.grd, type="inner"), 'sum')
  c(cellStats(area, 'sum')*res(area)[1]*res(area)[2], #area (m^2)
    cellStats(volume, 'sum'),                         #volume (m^3)
    outflow                                           #Outflow length (3 m increments)
  )
}

#Run function
df<-lapply(seq(0.1,2,0.1), inundate)
df<-data.frame(seq(0.1,2,0.1),do.call(rbind, df))
colnames(df)<-c("z","area","volume","spill_boundary_length")
  #Note, units will be in the units of m*[L^2], where L is the units from DEM projection

#Plot inundated area
z<-df$z[df$spill_boundary_length>0][1]
inundation<-Con(dem.grd>(temp_min.grd+z),0,1)
plot(inundation)

#Kelly, I chose QB to highlight limitation of this approach.  You will want to vary
#inundation depth until you get to the actual spill point (known from field data)
plot(Con(dem.grd>(temp_min.grd+0.85),0,1))

