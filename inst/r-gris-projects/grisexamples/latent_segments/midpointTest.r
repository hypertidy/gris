
#cds <- locator()
cds <- do.call(cbind, structure(list(x = c(146.904396934248, 125.339249697459, 110.962484872933,
                                           94.6252521177894, 68.4856797095603, -119.065752319484, -89.0052440500202
), y = c(-75.7294520547945, -51.4038461538462, -50.1235511064278,
         -48.8432560590095, -50.1235511064278, -3.71285563751317, 19.0123814541623
)), .Names = c("x", "y")))


class(cds) <- c("midpt", "matrix")
prj <- "+proj=laea +ellps=WGS84"
pts <- rgdal::project(x, prj)
calc.midpt <- function(x, pj) {
  pts <- project(x, prj)
  seg <- suppressWarnings(head(matrix(seq(nrow(x)), nrow = nrow(x) + 1, ncol = 2), -2))

#  for (i in seq(nrow(seg))) {
 #   lines(project(geosphere::gcIntermediate(x[seg[i,1], ], x[seg[i, 2], ]), prj), lty = 2)
  #}
  mll <- project(midptLL(x), prj)
  pll <- midpt(pts)

}



distMid <- function(p) {
  geosphere::distGeo(p[1,], p[2, ])
}
getXY <- function(verts, v0_1) {
  rbind(midptLL(cds[v0_1, ]), midpt(rgdal::project(pts[v0_1, ], prj, inv = TRUE)))
}


midptLL <- function(p, fold = TRUE) {
  n <- nrow(p)
  rad <- pi/180
  p <- rad * p
  dlon <- diff(p[, 1L])
  lon1 <- p[-n, 1L]
  lat1 <- p[-n, 2L]
  lat2 <- p[-1L, 2L]
  bx <- cos(lat2) * cos(dlon)
  by <- cos(lat2) * sin(dlon)
  lat <- atan2(sin(lat1) + sin(lat2), sqrt((cos(lat1) + bx)^2 +
                                             by^2))/rad
  lon <- (lon1 + atan2(by, cos(lat1) + bx))/rad
  if (fold)
    lon <- (lon + 180)%%360 - 180
  cbind(lon, lat)
}
midpt <- function(p) {
  tail(p, -1) + (head(p, -1) - tail(p, -1))/2
}

x <- cds
seg <- suppressWarnings(head(matrix(seq(nrow(x)), nrow = nrow(x) + 1, ncol = 2), -2))

distOffset <- function() {
  apply(seg, 1, function(x) distMid(getXY(cds, x)))
}

dg <- geosphere::distGeo(head(x, -1), tail(x, -1))
ds <- distOffset()


