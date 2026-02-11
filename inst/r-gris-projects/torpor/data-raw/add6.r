
vpal <- function(n) {
  if (!is.null(dim(n))) {
    n <- nrow(n)
  }
  viridis::viridis(n)
}
library(sf)
add6 <- st_read("data-raw/add6", "Sub-antarctic_coastline_low_res_polygon_to30S", stringsAsFactors = FALSE)
bad <- !st_is_valid(add6)
invalid <- add6[which(bad)[1], ]
plot(invalid)

library(dplyr)
library(rangl)
ri <- rangl(torpor:::sf_as_sp_l(invalid))
si <- torpor:::rg_as_sf(ri)
plot(si, col = vpal(si))

## these ones are the offenders in the first bad object
nodal <- st_as_sf(rgeos::gNode(as(si[c(82, 84), ], "Spatial")))
plot(nodal, asp = 1.2)
sp::plot(as(invalid, "Spatial"), add = TRUE, col = "grey", rule = "evenodd")
points(as(as(nodal, "Spatial"), "SpatialMultiPoints"))
text(ri$v$x_, ri$v$y_, lab = ri$v$vertex_, cex = 0.8, pos = 2, xpd = NA)
