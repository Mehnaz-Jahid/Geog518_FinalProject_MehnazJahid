
### spatial interpolation by kriging
# Create an empty grid where n is the total number of cells
f.0 <- as.formula(PM25 ~ 1)
#Create variogram
var.pm <- variogram(f.0, pm2.5, cloud = FALSE) 

dat.fit.exp  <- fit.variogram(var.pm, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=0.015, model="Exp", range=8000, nugget=0)) # this one works best
dat.fit.gau  <- fit.variogram(var.pm, fit.ranges = FALSE, fit.sills = FALSE,
                              vgm(psill=0.015, model="Gau", range=8000, nugget=0))
dat.fit.sph  <- fit.variogram(var.pm, fit.ranges = FALSE, fit.sills = FALSE,
                              vgm(psill=0.015, model="Sph", range=8000, nugget=0))

par(mfrow=c(1,3))
plot(var.pm, dat.fit.exp, main= "Exponential")
plot(var.pm, dat.fit.gau, main= "Gaussian")
plot(var.pm, dat.fit.sph, main= "Spherical")
par(mfrow=c(1,1))
dat.krg <- krige(f.0, pm2.5, grd, dat.fit.exp)
# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m <- mask(r, income.tracts)
# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="-RdBu",  
            title="Predicted PM2.5", midpoint = NA) +
  tm_shape(pm2.5) + tm_dots(size=0.06)+
tm_legend(legend.outside=TRUE)

#Extract average pm2.5 for each polygon
income.tracts$Pm2.5 <- round(extract(r, income.tracts, fun = mean)[,1], 5)
View(income.tracts@data)

