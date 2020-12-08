
### Linear Regression
#Let's say your dataset with both PM2.5 and Income 
#are stored in a dataset called income.tracts.
#Plot income and PM2.5 from the income.tracts dataset you created
plot(income.tracts$Pm2.5~income.tracts$Income)

#Notice that there are a lot of 0's in this dataset. If you decide to remove them, use the following line:
income.tracts.no0 <-  income.tracts[which(income.tracts$Pm2.5 > 0), ]

#Now plot the data again
plot(income.tracts.no0$Pm2.5~income.tracts.no0$Income)

#Perform a linear regression on the two variables. You should decide which one is dependent.
lm.model <- lm(income.tracts.no0$Income~income.tracts.no0$Pm2.5)
#Add the regression model to the plot you created
plot(income.tracts.no0$Income~income.tracts.no0$Pm2.5)
abline(lm.model, col = "red")
#Get the summary of the results
summary(lm.model)

#add the fitted values to your spatialpolygon dataframe
income.tracts.no0$predictlm <- lm.model$fitted.values

#You want to determine if the model residuals are spatially clustered. 
#add the residuals to your spatialpolygon dataframe
income.tracts.no0$residuals <- residuals.lm(lm.model)

#Observe the result to make sure it looks correct
head(income.tracts.no0)

#Now, create choropleth map of residuals
map_resid <- tm_shape(income.tracts.no0) +
  tm_polygons(col = "residuals",
              title = "Residuals",
              style = "jenks",
              palette = "Oranges", n = 6)+
  tm_legend(legend.outside=TRUE)
map_resid

## Check whether there is any spatial autocorrelation in residuals
# global Moran's I on residuals
resid<- income.tracts.no0[!is.na(income.tracts.no0@data$residuals), ]
resid.nb <- poly2nb(resid) # constructing neighbors list from polygon list using Queen's case
resid.net <- nb2lines(resid.nb, coords=coordinates(income.tracts.no0)) # creating a spatial line data frame
crs(resid.net) <- crs(income.tracts.no0) # setting the CRS as the data

resid.nb2 <- poly2nb(income.tracts.no0, queen = FALSE) #Rook's case
resid.net2 <- nb2lines(resid.nb2, coords=coordinates(income.tracts.no0))
crs(resid.net2) <- crs(income.tracts.no0)

resid.lw <- nb2listw(resid.nb, zero.policy = TRUE, style = "W") # weight matrix of Queen's case
print.listw(resid.lw, zero.policy = TRUE) # printing weight matrix
resid.lw2 <- nb2listw(resid.nb2, zero.policy = TRUE, style = "W") # weight matrix of Rook's case
print.listw(resid.lw2, zero.policy = TRUE) # printing weight matrix

milwresid <- moran.test(income.tracts.no0$residuals, resid.lw, zero.policy = TRUE) # queen's
milw2resid <- moran.test(income.tracts.no0$residuals, resid.lw2, zero.policy = TRUE) # Rook's

zlwresid <- (milwresid$estimate[[1]]-milwresid$estimate[[2]])/sqrt(milwresid$estimate[[3]]) # calculating the test statistic of Queen's case
zlw2resid <- (milw2resid$estimate[[1]]-milw2resid$estimate[[2]])/sqrt(milw2resid$estimate[[3]]) # calculating the test statistic of Rook's case

