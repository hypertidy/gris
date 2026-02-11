chars <- function() {
  numbers <- as.character(0L:9L)
  sample(c(letters, numbers, LETTERS))
}
makeindex <- function(n = 1L, len = 3) {
  do.call(paste0, c(lapply(seq(len), function(x) head(chars(), n))))
}

verts <- round(bind_cols(structure(list(x = c(108, 104, 98, 89, 78, 72, 59, 37, 23, 8,
                                              -5, -25, 1, 21, 38, 51, 58, 62, 62, 57, 51, 51, 42, 34, 22, 19,
                                              20, 25, 30, 33, 30, 24), y = c(-56, -61, -68, -78, -80, -83,
                                                                             -84, -84, -82, -80, -77, -75, -56, -33, -17, 2, 21, 39, 52, 63,
                                                                             66, 66, 66, 66, 64, 60, 57, 51, 46, 37, 29, 25)), .Names = c("x",
                                                                                                                                          "y"), row.names = c(NA, -32L), class = c("tbl_df", "tbl", "data.frame"
                                                                                                                                          )))
) %>% mutate(.ix0 = makeindex(n()))



#' Segment pairs
#'
#' Create segment pairs from a path of indexes.
#'
#' @param x indexes values
#'
#' @return data frame
#' @export
#'
#' @examples
#' path2segs(1:9)
#' ## doesn't have to be numbers
#' path2segs(letters)
path2segs <- function(x) {
  warn <- options(warn = -1)
  on.exit(options(warn))
  setNames(as_tibble(head(matrix(x, ncol = 2L, nrow = length(x) + 1), -2)), c("s0", "s1"))
}

p_n <- function(x, s, i = "s0") {
  s  %>% transmute_(.ix0 = i)  %>% inner_join(x)  %>% dplyr::select(x, y)
}
segdist <- function(x, v) {
  geosphere::distGeo(p_n(v, x, "s0"), p_n(v, x, "s1"))
}

#segs <- path2segs(verts$.ix0)
#segs$m <- segdist(segs, verts)


nverts <- function(nr2, n = 2) {
  if (n > 1) {
    xa <- geosphere::gcIntermediate(nr2[1,], nr2[2,], n = n, addStartEnd = TRUE)
    return(tibble(x = xa[,1], y = xa[,2]))
  }
  nr2
}

aa <- disaggregate(subset(wrld_simpl, NAME == "Antarctica"))[1,]
g <- spbabel:::mtable.Spatial(aa)
#verts <-  g$bXv  %>% inner_join(g$v)
verts <- g$v %>% transmute(x = x_, y = y_, .ix0 = vertex_)
segs <- path2segs(g$bXv$vertex_[g$bXv$order_])
segs$m <- segdist(segs, verts)
x <- list(v = verts, s = segs)
absprop <- function(x, y) {
  a <- x/y
  a[is.na(a)] <- 0
  a
}

modprop <- function(x) {
  a <- 10 * (1 %/% x)
  a[!is.finite(a)] <- 0
  a
}
plotx <- function(x, crs = "") {
  p <- as.matrix(x$v[, c("x", "y")])
  v <- x$v
  s <- x$s
  if (nchar(crs) > 0) {
    p <- rgdal::project(p, crs)
  }

  s$projdist <- sp::spDists(p, segments = TRUE, longlat = nchar(crs) == 0)
  s <- s  %>% mutate(rat = absprop(m, projdist))

  allnr2 <- lapply(split(s, seq(nrow(s))),
         function(x) inner_join(tibble(.ix0 = unlist(x)), v)[, c("x", "y")])
  npoints <- modprop(s$rat)
  latverts <- bind_rows(lapply(seq_along(npoints), function(x) nverts(allnr2[[x]], npoints[x])), .id = ".sx0")
  #plot(abs(s$m - s$projdist))
  plot(p, asp = 1)
   if (nchar(crs) > 0) {
    newpoints <- rgdal::project(as.matrix(latverts[, c("x", "y")]), crs)
    points(newpoints, pch = ".", cex = 0.5)
  }


  invisible(latverts)
}

prj <- "+proj=stere +lat_0=-60"
aa <- plotx(x, prj)
rgdal::llgridlines(sp::spTransform(sp::SpatialPoints(as.matrix(aa[, c("x", "y")]), proj4string = sp::CRS("+init=EPSG:4326")), prj))
