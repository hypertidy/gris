

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
#' tx <- "{\"type\":\"Topology\",\"objects\":{\"collection\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"Polygon\",\"properties\":{\"a\":1},\"arcs\":[[0,1],[2]]},{\"type\":\"Polygon\",\"properties\":{\"a\":2},\"arcs\":[[-1,3]]}]}},\"arcs\":[[[6503,5999],[-894,-5999]],[[5609,0],[-5609,0],[0,9999],[6097,0],[2032,-2000],[-4064,-1000],[2438,-1000]],[[1626,2000],[2439,0],[0,2000],[-1626,1999],[-813,-1999],[0,-2000]],[[6503,5999],[2439,300],[1057,-3299],[-4390,-3000]]],\"transform\":{\"scale\":[0.00012301230123012302,0.00010001000100010001],\"translate\":[0,0]},\"bbox\":[0,0,1.23,1]}"
#'
#'jsonlite::prettify(tx)
#'
#'library(tibble)
#'library(dplyr)
#'## it is easiest to parse twice, once for the coords
#'## once for the objects, but better parsing will fix
#'#tj_coords <- jsonlite::fromJSON(tx, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = TRUE)
#'#tj_objects <- jsonlite::fromJSON(tx, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
#'
#'obj <- rjson::fromJSON(tx)
#'v <- tj_coord(obj)
#'ns_obj <- tj_object(tx)

#'## convert to simple features
#'rapply(ns_obj, classes = "data.frame", f = function(x) inner_join(x, v))
tj_transform <- function(x, sc = 1, tr = 0) x * sc + tr

tj_coord <- function(x) {
  stopifnot(is.list(x))
  ##stopifnot(is.character(x))  ## must be topojson string
  #x <- jsonlite::fromJSON(tx, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = TRUE
  tj_scale <- x$transform$scale
  tj_translate <- x$transform$translate
  ## extract all arcs as paths, keep the unscaled coordinates next to the real-world ones
  
  #dplyr::bind_rows(lapply(x[["arcs"]], function(a) as_tibble(setNames(a, c("ix_", "iy_")))), .id = "i_arc_") %>%
  dplyr::bind_rows(lapply(x[["arcs"]], function(a) setNames(as_tibble(do.call(rbind, a)), c("ix_", "iy_"))), .id = "arc") %>% 
    mutate(x_ = tj_transform(ix_, tj_scale[1], tj_translate[1]),
           y_ = tj_transform(iy_, tj_scale[2], tj_translate[2]))
}

tj_arc <- function(x) {
  ## x must be a list of arcs, within a geometry
  ## e.g. jsonlite::fromJSON(tx, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)[[1]]$geometries[[i]]$arcs
  tibble::tibble(orientation = sign(x), i_arc_ = as.character(abs(x) + 1))  ## convert from 0-based
}

tj_geometry <- function(x) {
  ## x must be a list of geometries, within an object
  ## e.g. jsonlite::fromJSON(tx, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)[[1]]$geometries
  
  rapply(lapply(x[["geometries"]], "[", "arcs"), classes = c("integer", "numeric"), f = tj_arc, how = "replace")
}
tj_object <- function(x) {
  stopifnot(is.list(x))
  #stopifnot(is.character(x))  ## must be topojson string
  ## all arcs are 1D index vectors, nested like simple features
  ## x <- jsonlite::fromJSON(tx, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  lapply(x[["objects"]], tj_geometry)
}



