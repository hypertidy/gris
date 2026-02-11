## wtf
#' Combine a list of PolygonDataFrame objects into a single object. 
#' 
#' Objects are combined by modifying sequential IDs to increase. 
c_poly <- function(x) {
  poly <- x[[1]]
  if (length(x) == 1L) {
    warning("input of length one, returning first element")
    return(poly)
  }
  for (i in seq_along(x)[-1]) {
    x0 <- spChFIDs(x[[i]], as.character(seq(nrow(x[[i]])) + max(as.integer(row.names(poly)))))
    poly <- maptools::spRbind(poly, x0)
  }
  poly
}


library(maptools)
data(wrld_simpl)
library(raster)
library(gris)
library(dplyr)
library(rworldxtra)
data(countriesHigh)
map1 <- subset(countriesHigh, SOVEREIGNT == "Australia")
map1$NAME <- map1$SOVEREIGNT
map1 <- map1["NAME"]
#map1 <- subset(wrld_simpl, NAME == "Australia", select = "NAME")
ras <- raster(map1, nrow = 150, ncol = 250)
#ras <- raster(map1, nrow = 65, ncol = 85)
pgrid <- disaggregate(as(ras, "SpatialPolygonsDataFrame"))
pgrid$NAME <- sprintf(sprintf("g%%0%ii", ceiling(log10(ncell(ras) + 1))),  seq(ncell(ras)))
pgrid$layer <- NULL



## something like this
pbind <- c_poly(list(pgrid, map1))
pmesh0 <- RTriangle::triangulate(mkpslg(gris(pbind)))
pmesh <- gris:::as.gris.triangulation(pmesh0, type = "poly")
pmesh$oXt <- pmesh$b %>% transmute(.tr0 = .br0, .ob0 = .ob0)
pmesh$tXv <- with(pmesh, data_frame(.vx1 = bXv$.vx0[seq(1, nrow(bXv), by = 3)], 
                     .vx2 = bXv$.vx0[seq(2, nrow(bXv), by = 3)], 
                     .vx3 = bXv$.vx0[seq(3, nrow(bXv), by = 3)], 
                     .tr0 = oXt$.tr0))


centroids <- gris:::tricentroids(pmesh)


 mLink <-extract(as(map1, "SpatialPolygons"), centroids %>% dplyr::select(x, y) %>% as.matrix)$poly.ID
# # pLink <- extract(as(pbind, "SpatialPolygons"), centroids %>% dplyr::select(x, y) %>% as.matrix)$poly.ID
# # str(mLink)
# # str(pLink)
# 
 pmesh$o$name <- c("gr", "oz")[is.na(mLink) + 1]
 topo <- raster("E:\\DATA\\Etopo\\Etopo1Ice\\Etopo1.tif")
 pmesh$v$z <- extract(topo, pmesh$v %>% select(x, y) %>% as.matrix) * 25
# gris:::plot3d.gris(pmesh, verts = c("x", "y", "z"), objname = "name")


library(dismo)
gm <- gmap(map1, scale = 2, type = "satellite")

img <- as(gris:::rasterPal2RGB(readAll(gm)), "SpatialGridDataFrame")
texfile <- sprintf("%s.png", tempfile())

writeGDAL(img, texfile, drivername = "PNG", type = "Byte", mvFlag = 0)

texcoords <- xyFromCell(setExtent(gm, extent(0, 1, 0, 1)), cellFromXY(gm, project(pmesh$v %>% select(x, y) %>% as.matrix(), projection(img))))
texcoords[texcoords > 1 | texcoords < 0] <- NA_real_

texmesh <- gris:::grisTri2rgl(pmesh, globe = TRUE, verts = c("x", "y", "z"))
library(rgl)
texmesh$material <- list()
shade3d(texmesh, col = "white", texcoords = texcoords[texmesh$it, ], texture = texfile)
