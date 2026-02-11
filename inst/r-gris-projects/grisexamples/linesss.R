library(raster)

## for example
#r <- getData('SRTM', lon=147, lat=-42)
# or 
r <- raster("C:/temp/srtm_66_21.tif")

## make a line
lin <- structure(c(147.278215113875, 147.22345324458, 147.08654857134, 
                   146.9496438981, 146.840120159509, 146.689525018945, 146.580001280354, 
                   146.56631081303, 146.525239411058, 146.49785847641, -43.0914896421773, 
                   -43.0006466005827, -42.869428873835, -42.7886795035286, -42.6776491193575, 
                   -42.5464313926097, -42.3849326519971, -42.1628718836547, -42.0013731430421, 
                   -41.8499680737177), .Dim = c(10L, 2L), .Dimnames = list(NULL, 
                                                                           c("coords.x1", "coords.x2")))

## 
sp <- spLines(lin, crs = projection(r))

## buffer the line
library(rgeos)
lbuf <- gBuffer(sp, width = 0.05)
lbuf <- SpatialPolygonsDataFrame(lbuf, data.frame(name = "buffer"), match.ID = FALSE)

## triangulate the buffer and add a vertex to record distance from the start
library(gris)
gobj <- gris(lbuf)  ## relational table version of polygonal topology
tri <- gris::triangulate(gobj, a = 0.002)

tri$v$l <- spDistsN1(as.matrix(tri$v[,c("x", "y")]), lin[1,,drop = FALSE])
tri$tXv$rasterval <- 0
for (i in seq(nrow(tri$oXt))) {
  tri$tXv$rasterval[i] <- extract(r, spPolygons(tri$v[as.matrix(tri$tXv[i,  c(".vx1", ".vx2", ".vx3")]), ] %>% select(x, y) %>% as.matrix), fun = mean)
  
}
vv <- apply(matrix(tri$v$l[t(as.matrix(tri$tXv[, c(".vx1", ".vx2", ".vx3")]))], nrow = 3), 2, mean)
plot(cbind(vv, tri$tXv$rasterval)[order(vv), ], type = "l")




library(sp)
library(rgeos)

data(meuse.grid)
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE
proj4string(meuse.grid) <- CRS("+init=epsg:28992")

library(raster)
r <- raster(meuse.grid)
##	Slice width in meters:
slice_width <- 500

## line
x1 <- 181000
y1 <- 331500
x2 <- 180000
y2 <- 330500


lin <- spLines(rbind(c(x1, y1), c(x2, y2)), attr = data.frame(x = 1), crs = projection(meuse.grid))
lbuf <- gBuffer(lin, width = slice_width / 2)
