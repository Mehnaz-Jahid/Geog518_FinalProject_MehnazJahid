
### point pattern analysis of PM2.5
##QUADRAT ANALYSIS
coordsPM25<- as.data.frame(geom(pm2.5)[,c(2,3)]) # coordinates of PM2.5 dataset
pm25.ext <- as.matrix(extent(pm2.5)) # creating extent
window <- as.owin(list(xrange = pm25.ext[1,], yrange = pm25.ext[2,])) # observation window
pm25.ppp <- ppp(x = coordsPM25$x, y = coordsPM25$y, window = window) # creating ppp object
quads <- 8 # determine the number of qusdrats 
qcount <- quadratcount(pm25.ppp, nx = quads, ny = quads)

plot(pm25.ppp, pch = "+", cex = 0.5)
plot(qcount, add = T, col = "red")

qcount.df <- as.data.frame(qcount)
View(qcount.df)

qcount.df <- plyr::count(qcount.df,'Freq') ##Second, count the number of quadrats with a distinct number of points.
View(qcount.df)
colnames(qcount.df) <- c("x","f") ##Change the column names so that x=number of points and f=frequency of quadrats with x point.

sum.f.x2 <- sum((qcount.df$f)*(qcount.df$x)^2)
M <- quads*quads
N <- sum(qcount.df$f*qcount.df$x)
sum.fx.2 <- (sum(qcount.df$f*qcount.df$x))^2
VAR <- (sum.f.x2 -(sum.fx.2/M))/(M-1)
MEAN <- N/M
VMR <- VAR/ MEAN
chi.square = VMR*(M-1)
p = 1 - pchisq(chi.square, (M - 1))

