#' Fix overlapping areas error.
#'
#' The object is decomposed to its segments, and triangulated. This
#' inserts new vertices where ever the segments overlap (invalidly)
#' and returns component triangles. The final fix is to union those triangles, which automatically
#' rebuilds the right paths.
#'
#' @param x simple features object
#' @param ... arguments passed to methods
#'
#' @return simple features object
#' @export
#'
#' @examples
fix_overlapping_area <- function(x, ...) {
  UseMethod("fix_overlapping_area")
}

#' @importFrom sf st_union
#' @importFrom sc PRIMITIVE
#' @importFrom sfdct ct_triangulate
#' @name fix_overlapping_area
#' @export
fix_overlapping_area.sfc <- function(x, ...) {
  tris <- lapply(x, sfdct::ct_triangulate(x))
  ## union the triangles
  tris <- st_sfc(lapply(x, st_union))
  tris
}
#' @name fix_overlapping_area
#' @export
fix_overlapping_area.sf <- function(x, ...) {
  ## triangulate including edges
  #trisG <- lapply(x, sfdct::ct_triangulate)
  #x[["geometry"]] <- fix_overlapping_area(st_sfc(trisG))
  st_union(sfdct::ct_triangulate(x))
}
