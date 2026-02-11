# fix_overlapping_area <- function(pdata, spatial = TRUE, keep_old = TRUE) {
#   if (is.character(pdata)) pdata <- st_read(pdata, quiet = TRUE)
#   print("checking for warnings ...")
#   bad <-  !st_is_valid(pdata)
#   force(1)
#   sp_pdata <- as(pdata, "Spatial")
#   #ri <- rangl(sp_pdata)
#   ##$l <- vector("list", nrow(ri$o))
#   g <- st_geometry(pdata)
#   index <- which(bad)
#   print(sprintf("checking %i of %i invalid geometries for self-overlap", length(index), length(bad)))
#   for (i_obj in which(bad)) {
#     ri <- rangl(sp_pdata[i_obj, ])
#     tab <- ri$o %>% inner_join(ri$t, "object_") %>%
#       inner_join(ri$tXv, "triangle_") %>% inner_join(ri$v, "vertex_") %>% dplyr::select(triangle_, x_, y_)
#     centroids <- tab %>% group_by(triangle_) %>% summarize(x = mean(x_), y = mean(y_))
#     sf_centroids <- do.call(st_sfc, lapply(split(centroids %>% dplyr::select(x, y), centroids$triangle_), function(x) st_point(as.matrix(x))))
#     ##centroids$pip <- unlist(lapply(st_covered_by(sf_centroids, pdata), length)) > 0
#     centroids$pip <- !is.na(over(SpatialPoints(as.matrix(centroids[, c("x", "y")]), proj4string = CRS(st_crs(pdata)$proj4string)), geometry(sp_pdata)))
#
#     tab <- centroids %>% filter(pip) %>% dplyr::select(triangle_)  %>% inner_join(tab, "triangle_")
#     g[[i_obj]] <- st_union(lapply(split(tab[, c("x_", "y_")], tab$triangle_)[unique(tab$triangle_)],
#                                   function(x) st_polygon(list(as.matrix(x[c(1, 2, 3, 1), ])))) %>% st_sfc(crs = st_crs(pdata)))
#
#   }
#
#   if (keep_old) pdata[["geometry_old"]] <- st_geometry(pdata)
#   if (length(g) == 1) g <- g[[1]]
#   pdata[["geometry"]] <- g ##do.call(st_sfc, l)
#   if (spatial) pdata <- as(pdata, "Spatial")
#   pdata
# }


rg_as_sf <- function(x) {
  mkmatline <- function(a, v) {
    st_linestring(a %>% inner_join(v, "vertex_") %>% select(x_, y_) %>% as.matrix())
  }
  ## expect line segments
  stopifnot(all(c("lXv", "l") %in% names(x) ))
  g <- st_sfc(lapply(split(x$lXv, ordered(x$lXv$segment_, unique(x$lXv$segment_))), mkmatline, x$v), crs = x$meta$proj[1])
  d <- data.frame( id = seq_len(length(g)))
  d[["geometry"]] <- g
  st_as_sf(d)
}

sf_as_sp_l <- function(x) {
  as(as(x, "Spatial"), "SpatialLinesDataFrame")
}

sf_as_sp_p <- function(x) {
  as(as(x, "Spatial"), "SpatialPolygonsDataFrame")
}
