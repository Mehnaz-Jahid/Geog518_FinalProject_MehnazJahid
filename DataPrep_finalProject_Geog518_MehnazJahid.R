#Libraries
library(spgwr)
library(spatstat)
library(tmap)
library(gstat)
library(sf)
library(raster)
library(rgdal)
library(e1071)
library(spdep)

#Set working directory
dir <- "C:/Users/mehnazjahid/Desktop/UVic/GEO518/finalProject/Census"
setwd(dir)

#Reading in particulate matter dataset
#Read in PM2.5 data:
pm2.5 <- readOGR("Pm25Sample.shp") 
pm2.5 <- spTransform(pm2.5, CRS("+init=epsg:26910"))
View(pm2.5@data)

#Read in census income data:
income <- read.csv("Income.csv")  
#Select only ID and Income columns:
colnames(income) <- c("DAUID", "Income") 

#Read in dissemination tract shapefile:
census.tracts <- readOGR("BC_DA.shp") 
census.tracts26910<- spTransform(census.tracts, CRS("+init=epsg:26910"))

#Merge income and dissemination data:
income.tracts <- merge(census.tracts,income, by = "DAUID") 

#Determine the number of columns in the dataframe:
nrow(income.tracts)

#Remove NA values:
income.tracts <- income.tracts[!is.na(income.tracts$Income),]

#Reproject the data:
income.tracts <- spTransform(income.tracts, CRS("+init=epsg:26910"))
View(income.tracts@data)

#Create choropleth map of income:
map_Income <- tm_shape(income.tracts) +
  tm_polygons(col = "Income",
              title = "Median Income",
              style = "jenks",
              palette = "-viridis", n = 6) +
  tm_legend(legend.position = c("LEFT", "BOTTOM"), legend.text.size=.7, legend.title.size= .9)

map_Income

#Create a grid called grd to use in your interpolation
# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(pm2.5, "regular", n=5000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
# Create SpatialPixel object:
gridded(grd)     <- TRUE  
# Create SpatialGrid object:
fullgrid(grd)    <- TRUE  
#Reproject the grid:
proj4string(grd) <- proj4string(income.tracts)

### descriptive stats of pm2.5
pm25<- pm2.5$PM25
hist(pm25, xlab="PM2.5", main="Histogram of PM2.5")
min(pm25)
max(pm25)
mean(pm25)
median(pm25)
sd(pm25)
shapiro.test(pm25)$p.value

### descriptive stats of income
Income<- income.tracts$Income
hist(Income, xlab="Income", main="Histogram of income")
min(Income)
max(Income)
mean(Income)
median(Income)
sd(Income)
shapiro.test(Income)$p.value

