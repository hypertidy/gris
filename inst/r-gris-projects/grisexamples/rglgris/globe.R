library(rglgris)
library(rgl)
library(rgdal)
library(raster)
load("E:\\DATA\\d3d\\nice.RData")
load("E:\\DATA\\d3d\\xice.RData")
load("E:\\DATA\\d3d\\xsst.RData")

sst <- bgl(xsst[[1]])
sice <- bgl(xice[[1]])
n_ice <- bgl(nice[[1]])

projvb <- function(x, proj, ...) {
    x0 <- t(x[1:2,])
    t(project(x0, proj, ...))

}
xyzvb <- function(x) {
    t(llh2xyz(t(x[1:3,])))
}
sst$vb[3,] <- -1e5
sst$vb[1:3,] <- xyzvb(sst$vb)

sice$vb[1:2,] <- projvb(sice$vb, projection(xice), inv = TRUE)
sice$vb[1:3,] <- xyzvb(sice$vb)

n_ice$vb[1:2,] <- projvb(n_ice$vb, projection(nice), inv = TRUE)
n_ice$vb[1:3,] <- xyzvb(n_ice$vb)

svals <- values(xice[[1]])
sok <- svals > 0 & !is.na(svals)
nvals <- values(nice[[1]])
nok <- nvals > 0 & !is.na(nvals)

sstvals <- values(xsst[[1]])
sstok <- !is.na(sstvals)

icePal <- function(x) {
  grey(seq(0.01, 1, length = 100))[x]
}
rglgris:::pquads(sice, col = rep(icePal(svals[sok]), each = 4), subset = sok, alpha = 0.8)
rglgris:::pquads(n_ice, col = rep(icePal(nvals[nok]), each = 4), subset = nok, alpha = 0.8)

rglgris:::pquads(sst, col = rep(sstPal(sstvals[sstok]), each = 4), subset = sstok)


