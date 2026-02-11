database <- "world2" 
dbname <- paste(database, "MapEnv", sep = "")
gon <- maps:::mapname(database, patterns = ".", exact = FALSE)
mapbase <- paste(Sys.getenv(get(dbname)), database, sep = "")
as.polygon <- FALSE
xlim <- c(-1e+30, 1e+30)
ylim <- c(-1e+30, 1e+30)
line <- maps:::mapgetg(database, gon, as.polygon, xlim, ylim)
# str(line)
# List of 3
# $ number: int [1:2366] 1 2 3 4 5 6 7 8 9 10 ...
# $ size  : int [1:1639] 1 6 4 3 1 5 1 1 1 2 ...
# $ name  : chr [1:1639] "Aruba" "Afghanistan" "Angola" "Angola:Cabinda" ...
lines <- gon
nline <- as.integer(length(lines))
fill <- FALSE
z <- .C("mapgetl", PACKAGE = "maps", as.character(mapbase), 
        linesize = as.integer(lines), error = as.integer(nline), 
        as.integer(0), as.double(0), as.double(0), as.double(c(xlim, ylim)), 
        as.integer(fill))[c("linesize", "error")]                                                      



ok <- z$linesize != 0
lines <- lines[ok]
nline <- length(lines)
if (nline == 0) 
  return(integer(0))
linesize <- z$linesize[ok]
N <- sum(linesize) + nline - 1
coord <- .C("mapgetl", PACKAGE = "maps", as.character(mapbase), 
         as.integer(lines), as.integer(nline), as.integer(1), 
         x = double(N), y = double(N), range = double(4), as.integer(fill))[c("x", 
                                                                              "y", "range")]
library(dplyr)
v <- tibble::tibble(x_ = coord$x, y_ = coord$y) %>% mutate(vertex_ = row_number())

arc <- tibble(vertex_ = v$vertex_, arc = rep(seq_along(line$number), line$size))

makepoly takes coord, z$linesize
# gon <- mapname(database, regions, exact)
# n <- length(gon)
# if (n == 0) 
#   stop("no recognized region names")
# if (is.null(xlim)) 
#   xlim <- c(-1e+30, 1e+30)
# if (is.null(ylim)) 
#   ylim <- c(-1e+30, 1e+30)
# line <- mapgetg(database, gon, as.polygon, xlim, ylim)
# if (length(line$number) == 0) 
#   stop("nothing to draw: all regions out of bounds")
# if (as.polygon) {
#   coord <- mapgetl(database, line$number, xlim, ylim, 
#                    fill)
#   gonsize <- line$size
#   keep <- rep(TRUE, length(gonsize))
#   coord[c("x", "y")] <- makepoly(coord, gonsize, keep)
# }
# else {
#   l <- abs(line$number)
#   if (boundary && interior) 
#     l <- unique(l)
#   else if (boundary) 
#     l <- l[!match(l, l[duplicated(l)], FALSE)]
#   else l <- l[duplicated(l)]
#   coord <- mapgetl(database, l, xlim, ylim, fill)
#   if (length(coord) == 0) 
#     stop("all data out of bounds")
# }
