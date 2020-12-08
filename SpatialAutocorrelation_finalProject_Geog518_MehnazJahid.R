### Spatial autocorrelation
## Global Moran's I
incm<- income.tracts[!is.na(income.tracts@data$Income), ]
incm.nb <- poly2nb(incm) # constructing neighbors list from polygon list using Queen's case
incm.net <- nb2lines(incm.nb, coords=coordinates(income.tracts)) # creating a spatial line data frame
crs(incm.net) <- crs(income.tracts) # setting the CRS as the data

incm.nb2 <- poly2nb(income.tracts, queen = FALSE) #Rook's case
incm.net2 <- nb2lines(incm.nb2, coords=coordinates(income.tracts))
crs(incm.net2) <- crs(income.tracts)

incm.lw2 <- nb2listw(incm.nb2, zero.policy = TRUE, style = "W") # weight matrix of Rook's case
print.listw(incm.lw2, zero.policy = TRUE) # printing weight matrix
incm.lw <- nb2listw(incm.nb, zero.policy = TRUE, style = "W") # weight matrix of Queen's case
print.listw(incm.lw, zero.policy = TRUE) # printing weight matrix

milw <- moran.test(income.tracts$Income, incm.lw, zero.policy = TRUE) # queen's
milw2 <- moran.test(income.tracts$Income, incm.lw2, zero.policy = TRUE) # Rook's

mIlw <- milw$estimate[[1]] # extracting the global Moran's I value of queen's case
mIlw2 <- milw2$estimate[[1]] # extracting the global Moran's I value of rook's case
eIlw <- milw$estimate[[2]]# extracting the expected Moran's I value of Queen's case
eIlw2 <- milw2$estimate[[2]]# extracting the expected Moran's I value of Rook's case
varlw <- milw$estimate[[3]] # extracting the variance of Queen's case
varlw2 <- milw2$estimate[[3]] # extracting the variance of Rook's case
zlw <- (mIlw-eIlw)/sqrt(varlw) # calculating the test statistic of Queen's case
zlw2 <- (mIlw2-eIlw2)/sqrt(varlw2) # calculating the test statistic of Rook's case

## local Moran's I

lisa.test <- localmoran(income.tracts$Income, incm.lw, zero.policy = T) # calculating Local Moran's I Queen's
lisa.test2 <- localmoran(income.tracts$Income, incm.lw2, zero.policy = T) # calculating Local Moran's I Rook's

income.tracts$IiQueen <- lisa.test[,1] # storing local Moran's I value for each polygon in the dataset as a variable called Ii for Queens
income.tracts$E.IiQueen<- lisa.test[,2] # storing the expected value of local Moran's I.
income.tracts$Var.IiQueen<- lisa.test[,3] # storing the variance 
income.tracts$Z.IiQueen<- lisa.test[,4] # storing the z-value
income.tracts$P.Queen<- lisa.test[,5] # storing the p-value
income.tracts$IiRook <- lisa.test2[,1] # storing local Moran's I value for each polygon in the dataset as a variable called Ii for Rooks
income.tracts$E.IiRook<- lisa.test2[,2] # storing the expected value of local Moran's I. for Rooks
income.tracts$Var.IiRook<- lisa.test2[,3] # storing the variance Rooks
income.tracts$Z.IiRook<- lisa.test2[,4] # storing the z-value Rooks
income.tracts$P.Rook<- lisa.test2[,5] # storing the p-value
income.tracts$sigQueen<- "not significant"
income.tracts$sigRook<- "not significant"
income.tracts$sigQueen[income.tracts$Z.IiQueen <= -1.96 & income.tracts$IiQueen<income.tracts$E.IiQueen| income.tracts$Z.IiQueen >= 1.96 & income.tracts$IiQueen<income.tracts$E.IiQueen] = "-ve autocorrelation"
income.tracts$sigQueen[income.tracts$Z.IiQueen <= -1.96 & income.tracts$IiQueen>income.tracts$E.IiQueen| income.tracts$Z.IiQueen >= 1.96 & income.tracts$IiQueen>income.tracts$E.IiQueen] = "+ve autocorrelation" 
income.tracts$sigRook[income.tracts$Z.IiRook <= -1.96 & income.tracts$IiRook<income.tracts$E.IiRook| income.tracts$Z.IiRook >= 1.96 & income.tracts$IiRook<income.tracts$E.IiRook] = "+ve autocorrelation"
income.tracts$sigRook[income.tracts$Z.IiRook <= -1.96 & income.tracts$IiRook>income.tracts$E.IiRook| income.tracts$Z.IiRook >= 1.96 & income.tracts$IiRook>income.tracts$E.IiRook] = "+ve autocorrelation" 
View(income.tracts@data)

count(income.tracts$sigQueen=="+ve autocorrelation")/length(income.tracts$sigQueen)
count(income.tracts$sigQueen=="-ve autocorrelation")/length(income.tracts$sigQueen)
count(income.tracts$sigQueen=="not significant")/length(income.tracts$sigQueen)

count(income.tracts$sigRook=="+ve autocorrelation")/length(income.tracts$sigRook)
count(income.tracts$sigRook=="-ve autocorrelation")/length(income.tracts$sigRook)
count(income.tracts$sigRook=="not significant")/length(income.tracts$sigRook)

########################
map_LISA.Queen <- tm_shape(income.tracts) + # mapping the local Moran's I
  tm_polygons(col = "sigQueen", 
              title = "Local Moran's I (Queen's)", 
              style = "cat", 
              palette = "viridis"
              ) +
  tm_legend(legend.position = c("LEFT", "BOTTOM"), legend.text.size=.6, legend.title.size= .9)
map_LISA.Queen

map_LISA.Rook <- tm_shape(income.tracts) + # mapping the local Moran's I
  tm_polygons(col = "sigRook", 
              title = "Local Moran's I (Rook's)", 
              style = "cat", 
              palette = "viridis")+
  tm_legend(legend.position = c("LEFT", "BOTTOM"), legend.text.size=.6, legend.title.size= .9)
map_LISA.Rook

