f <- file.path(getOption("default.datadir"), "data/listdata.thelist.tas.gov.au/opendata/data/", 
               "list_2d_building_polys_glenorchy.tab")


library(sf)
d <- sf::st_read(f)
# Reading layer `list_2d_building_polys_glenorchy' from data source `listdata.thelist.tas.gov.au/opendata/data/list_2d_building_polys_glenorchy.tab' using driver `MapInfo File'
# Simple feature collection with 2608 features and 10 fields
# geometry type:  POLYGON
# dimension:      XY
# bbox:           xmin: 512484.9 ymin: 5254640 xmax: 526356.6 ymax: 5266692
# epsg (SRID):    NA
# proj4string:    +proj=utm +zone=55 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs

#devtools::install_github("environmentalinformatics-marburg/mapview", ref = "develop")
library(mapview)
mapview(d)

library(aceecostats)
mapview(aes_region)
aes_region

example(st_read)
mapview(nc)

library(geojsonio)
library(help = "geojsonio")


P_crs <- st_crs(28992)
data("meuse", package = "sp")
meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = P_crs)
mapview(meuse)

proj4string(meuse) <- CRS(unclass(P_crs$proj4string))
meuse.4326 <- spTransform(meuse, proj4def.4326)

## [1] "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs"
proj4def_LL <- st_crs(4326)$proj4string
minZoom = 0
maxZoom = 13

resolutions <- 0.42*(2^(maxZoom:minZoom))

crs <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = sprintf("EPSG:%i", P_crs$epsg),
  proj4def = P_crs$proj4string,
  resolutions = resolutions)

leaflet(options = leafletOptions(crs = crs,
                                 minZoom = minZoom,
                                 maxZoom = maxZoom)) %>%  addTiles()

%>%
  addCircleMarkers(data = meuse, popup = popupTable(meuse))




meuse.4326 <- st_transform(meuse, 4326)

crs.28992.forTiles <- leafletCRS(
  crsClass = "L.Proj.CRS.TMS",
  code = 'EPSG:28992',
  proj4def = P_crs$proj4string,
  resolutions = resolutions,
  projectedBounds = c(-285401.92, 22598.08, 595401.9199999999, 903401.9199999999))

leaflet(options = leafletOptions(crs = crs.28992.forTiles,
                                 minZoom = minZoom,
                                 maxZoom = maxZoom)) %>%
  addCircleMarkers(data = meuse.4326, popup = popupTable(meuse)) %>%
  setView(5.734745, 50.964112, zoom = 9) %>%
  addTiles('http://geodata.nationaalgeoregister.nl/tms/1.0.0/brtachtergrondkaart/{z}/{x}/{y}.png', options = tileOptions(tms=TRUE)) %>%
  htmlwidgets::onRender("function(el,t){ var myMap=this; debugger; }")




