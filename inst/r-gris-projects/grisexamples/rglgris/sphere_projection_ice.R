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
##plotRGB(bm)

## ice files
## 4 is reasonable, 10 very fast, 1 is no reduction
sm <-smashimate(bm,2)
## Raster brick with RGB at reduce resolution
rs <- setValues(sm, extract(bm, coordinates(sm), method = "simple"))
## colour map in rgl's index
bmcols <- brick2col(rs)

## build rgl object from our etopo/colour map, with Z's populated 
## (homogenous coordinates in $vb, index in $ib - essentially t(xyzw))
ro <- bgl(rs, z = Etopo20)  ## use Etopo20 for z
proj <- "+proj=omerc +lonc=165 +lat_0=-22 +alpha=23 +gamma=23"

  library(graticule)
  l <- graticule(proj = proj)


## check mesh for long segments
mshlen <- function(vb, ib) {
  len <- ncol(vb) 
  lens <- numeric(len)
  xms <-  matrix(vb[1, ib], nrow = 4)
  yms <- matrix(vb[2, ib], nrow = 4)
  xsd <- ( colSums(xms * xms) - (colSums(xms) ^ 2)/4) / 3
  ysd <- ( colSums(yms * yms) - (colSums(yms) ^ 2)/4) / 3
  pmax(xsd, ysd)
}


plan <- TRUE  ## projected plane or sphere?
if (plan) {
    library(rgdal)
	roxyz <- ro; roxyz$vb[1:2,] <- t(project(t(ro$vb[1:2,]), proj))
ll <- mshlen(roxyz$vb, roxyz$ib) 
bad0 <- ll > quantile(ll, 0.99907)

	roxyz$vb[3,] <- roxyz$vb[3,]  * 50  ## arbitrary exaggeration
roxyz$ib <- roxyz$ib[,!bad0]
} else {
   roxyz <- ro; roxyz$vb[1:3, ] <- t(llh2xyz(t(roxyz$vb[1:3, ]), exag = 50))
}

shade3d(roxyz, col = rep(bmcols[!bad0], each = 4), lit = FALSE)
#for (i in seq(nrow(l))) {xy <- coordinates(as(l[i,], "SpatialPoints")); rgl.lines(cbind(xy, 1000), col = "white")}

sfiles <- c("ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/south/daily/2014/nt_20140601_f17_v01_s.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/south/daily/2014/nt_20140701_f17_v01_s.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/south/daily/2014/nt_20140801_f17_v01_s.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/south/daily/2014/nt_20140901_f17_v01_s.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/south/daily/2014/nt_20141001_f17_v01_s.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/south/daily/2014/nt_20141101_f17_v01_s.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/south/daily/2014/nt_20141201_f17_v01_s.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/south/nt_20150101_f17_nrt_s.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/south/nt_20150201_f17_nrt_s.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/south/nt_20150301_f17_nrt_s.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/south/nt_20150401_f17_nrt_s.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/south/nt_20150501_f17_nrt_s.bin" 
)

nfiles <- c("ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/north/daily/2014/nt_20140601_f17_v01_n.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/north/daily/2014/nt_20140701_f17_v01_n.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/north/daily/2014/nt_20140801_f17_v01_n.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/north/daily/2014/nt_20140901_f17_v01_n.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/north/daily/2014/nt_20141001_f17_v01_n.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/north/daily/2014/nt_20141101_f17_v01_n.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/north/daily/2014/nt_20141201_f17_v01_n.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/north/nt_20150101_f17_nrt_n.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/north/nt_20150201_f17_nrt_n.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/north/nt_20150301_f17_nrt_n.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/north/nt_20150401_f17_nrt_n.bin", 
"ftp://sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/north/nt_20150501_f17_nrt_n.bin"
)

for (i in seq_along(sfiles)) {
   if (!file.exists(basename(sfiles[i]))) download.file(sfiles[i], basename(sfiles[i]), mode = "wb")
    if (!file.exists(basename(nfiles[i]))) download.file(nfiles[i], basename(nfiles[i]), mode = "wb")
}

sf <- basename(sfiles)
nf <- basename(nfiles)

s_ <- stack(sf)
n_ <- stack(nf)
i <- 9
#for (i in 1:1000) {
 ii <-  ((i - 1) %% 12) + 1
vn <- values(n_[[ii]])
vs <- values(s_[[ii]])

nbad <- is.na(vn) | !(vn > 0)
nicecols <- grey(vn[!nbad]/100)
ngl <- bgl(raster(n_[[1]]))
#nqverts <- llh2xyz(cbind(project(t(ngl$vb[1:2, ]), projection(n_), inv = TRUE), 0))
nqverts <- cbind(project(project(t(ngl$vb[1:2, ]), projection(n_), inv = TRUE), proj), 0)
ngl$vb[1:3, ] <- t(nqverts)
ngl$ib <- ngl$ib[,!nbad]

sbad <- is.na(vs) | !(vs > 0)
sicecols <- grey(vs[!sbad]/100)
sgl <- bgl(raster(s_[[1]]))
#sqverts <- llh2xyz(cbind(project(t(sgl$vb[1:2, ]), projection(s_), inv = TRUE), 0))
sqverts <- cbind(project(project(t(sgl$vb[1:2, ]), projection(s_), inv = TRUE), proj), 0)
sgl$vb[1:3, ] <- t(sqverts)
sgl$ib <- sgl$ib[,!sbad]
#rgl.clear()
#shade3d(roxyz, col = rep(bmcols, each = 4), lit = FALSE)
shade3d(ngl, col = rep(nicecols, each = 4))
shade3d(sgl, col = rep(sicecols, each = 4))


  library(graticule)
  l <- graticule(proj = proj)
 for (i in seq(nrow(l))) {xy <- coordinates(as(l[i,], "SpatialPoints")); rgl.lines(cbind(xy, 1000), col = "white")}
}
