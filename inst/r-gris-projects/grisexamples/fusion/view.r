library(manifoldr)
mapfile2 <- system.file("extdata", "Montara_20m.map", package= "manifoldr")
gg <- Surface(mapfile2, "Montara")
decimate <- function(x, fact = 10) {
  r <- raster::raster(x); raster::res(r) <- raster::res(x) * fact
  raster::setValues(r, raster::extract(x, coordinates(r)))
}

raster2tri <- function(x, fact = 4) {
  triangulate(gris(raster::rasterToPolygons(decimate(x, fact = fact))))
}
library(gris)
rt <- raster2tri(gg, fact = 28)
rt$v$z <- raster::extract(gg, rt$v %>% dplyr::select(x, y) %>% as.matrix)

gris:::plot3d.gris(rt, globe = FALSE, verts = c("x", "y", "z"))
pt <- cbind(543000, 4166500,   10)
rgl::points3d(pt)
rt$v$z[is.na(rt$v$z)] <- jitter(rep(0, sum(is.na(rt$v$z))))



phi <- atan(abs(rt$v$y - pt[2])/ abs(rt$v$x - pt[1] ))
theta <- atan(abs(rt$v$z - pt[3] )/  abs(rt$v$y - pt[2]))

rt2 <- rt
rt2$v$x <- phi
rt2$v$y <- theta

rt3 <- triangulate(rt2)



xy <- locator(4)
p <- RTriangle::triangulate(RTriangle::pslg(do.call(cbind, xy), S = matrix(c(1, 2, 2, 3, 3, 4, 4, 1), ncol = 2, byrow = TRUE)))
plot(p)


