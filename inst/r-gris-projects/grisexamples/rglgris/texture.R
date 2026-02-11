library(raster)
library(rgl)
library(rglgris)
## fast grid reduction
smashimate <- function(x, smash) {dim(x) <- dim(x)/smash; x}

## 1.2 Mb raster topography
EtopoFile <- "http://staff.acecrc.org.au/~mdsumner/grid/Etopo20.Rdata"
if (!file.exists(basename(EtopoFile))) download.file(EtopoFile, basename(EtopoFile), mode = "wb")
load(basename(EtopoFile))
## 14 Mb  RGB blue marble image
bmfile <- "http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73884/world.topo.bathy.200411.3x5400x2700.png"
if (!file.exists(basename(bmfile))) download.file(bmfile, basename(bmfile), mode = "wb")
bm <- setExtent(brick(basename(bmfile), crs = "+proj=longlat +ellps=WGS84"), extent(-180, 180, -90, 90))
sm <-smashimate(bm,10)
## Raster brick with RGB at reduce resolution
rs <- setValues(sm, extract(bm, coordinates(sm), method = "simple"))

## build rgl object from our etopo/colour map, with Z's populated
## (homogenous coordinates in $vb, index in $ib - essentially t(xyzw))
ro <- bgl(rs, z = Etopo20)  ## use Etopo20 for z
##ro$texcoords <- t(xyFromCell(setExtent(bm, extent(0, 1, 0, 1)), cellFromXY(bm, t(ro$vb)[,1:2])))


bo <- bgl(raster(volcano), z = raster(volcano)/500)
scl <- function(x) (x - min(x)) /diff(range(x))

##bo$texcoords <- NULL; ##rbind(scl(bo$vb[1,]), scl(bo$vb[2,]))
## dunno shade3d(bo, texcoords = cbind(scl(bo$vb[1,]), scl(bo$vb[2,]), 0, 1), texture = "logo.png", override = TRUE)

## not transpose for texture . . .
rgl.quads(bo$vb[1,bo$ib], bo$vb[2,bo$ib], bo$vb[3, bo$ib], texcoords =cbind(bo$vb[1,bo$ib], bo$vb[2,bo$ib]), texture = "logo.png")
##ro$texcoords[1:2,] <-
##shade3d(ro, texture = "world.topo.bathy.200411.3x5400x2700.png")

rgl.quads(ro$vb[1,ro$ib], ro$vb[2,ro$ib], ro$vb[3, ro$ib], texcoords =cbind(scl(ro$vb[1,ro$ib]), scl(ro$vb[2,ro$ib])), texture = "world.topo.bathy.200411.3x5400x2700.png")

roxyz <- ro; roxyz$vb[1:3, ] <- t(llh2xyz(t(roxyz$vb[1:3, ]), exag = 100))
rgl.quads(roxyz$vb[1,roxyz$ib], roxyz$vb[2,roxyz$ib], roxyz$vb[3, roxyz$ib],
          texcoords = xyFromCell(setExtent(bm, extent(0, 1, 0, 1)), cellFromXY(bm, t(ro$vb[1:2,]))), texture = "world.topo.bathy.200411.3x5400x2700.png")


