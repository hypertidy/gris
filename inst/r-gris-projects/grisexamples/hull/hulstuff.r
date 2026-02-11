---
title: "hull"
author: "Michael Sumner"
date: "9 February 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r}
library(gris)
library(maptools)
data(wrld_simpl)
library(dplyr)
g <- triangulate(gris(subset(wrld_simpl, NAME == "Australia")))
g <- triangulate(gris(wrld_simpl))

ta <- function(ptri) data_frame(a = abs(sum(ptri$x * (magic::shift(ptri$y, -1) - magic::shift(ptri$y, 1))))/2)
g$tXv$area <- (bind_rows(g$tXv  %>% inner_join(g$v, c(".vx1" = ".vx0")),
g$tXv  %>% inner_join(g$v, c(".vx2" = ".vx0")),
g$tXv  %>% inner_join(g$v, c(".vx3" = ".vx0"))) %>% group_by(.tr0) %>% select(x, y) %>% do(ta(.)))$a

g1 <- g
g1$tXv <- g1$tXv %>% filter(area > 10)

plot(g)
plot(g1, add = TRUE, triangles = TRUE)

w <- disaggregate(subset(wrld_simpl, NAME == "Australia"))
plot(w[1,])

pts <- raster::geom(w[1,])[, c("x", "y")]

```
```{r}
x <- read.csv("data.csv")
ind <- chull(x)
plot(x)
polypath(x[ind, ])

# ## convert to Spatial?
# library(raster)
# ch <- spPolygons(as.matrix(x[ind, ]), attr = data.frame(name = "hull", stringsAsFactors = FALSE))

library(geometry)
del <- delaunayn(as.matrix(x))
library(raster)
raw <- lapply(apply(del, 1, function(xx) list(as.matrix(x[c(xx, xx[1]), ]))), "[[", 1)
sp <- do.call(spPolygons, raw)
sp1 <- SpatialPolygonsDataFrame(sp, data = data.frame(x = seq(length(raw))))

library(rgeos)
# sp1$area <- gArea(sp1, byid = TRUE)
# ## see those big triangles?
# plot(x)
# plot(sp1, add = TRUE)
# ## no good
# plot(x)
# plot(subset(sp1, area < 0.15), add = TRUE)

sp1$length <- gLength(sp1, byid = TRUE)
#plot(sort(sp1$length))

plot(x, asp = 1)
plot(subset(sp1, length < 2.5), add = TRUE)


## finally 
plot(x, asp = 1)
plot(gUnionCascaded(subset(sp1, length < 2.5)), add = TRUE, col = "grey")
