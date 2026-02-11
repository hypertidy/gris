library(rlas)
library(nabor)
f <- c("E:\\DATA\\Tassie\\LiDAR\\Mt Wellington\\LAS12\\5205247.las",
"E:\\DATA\\Tassie\\LiDAR\\Mt Wellington\\LAS12\\5215247.las")

xp <- do.call(rbind, lapply(f, readLAS))

library(raster)
g2 <- raster(extent(xp[,1:2]), res = 2)
g5 <- raster(g2); res(g5) <- 5
g10 <- raster(g5); res(g10) <- 10


pts <- data_frame(x = xp[,1], y = xp[,2], z = xp[,3], 
                  c2 = cellFromXY(g2, xp[,1:2]),
                  c5 = cellFromXY(g5, xp[,1:2]), 
                  c10 = cellFromXY(g10, xp[,1:2]))


agg <- pts %>% group_by(c5) %>% summarize(min = min(z))
g5min <- setValues(g5, NA_real_)
g5min[agg$c5] <- agg$min

shade3d(bgl(g5min, g5min), col = "grey")


tz <- extract(g5min, xp[,1:2], method = "bilinear")

ok <- abs(xp[,3] - tz) < 0.5
points3d(xp[ok, ])
pp <- xp[ok, 1:2]
pp <- pp[!is.na(pp[,1]),  ]
pp <- pp[!duplicated(pp), ]
tri <- RTriangle::triangulate(pslg(pp))


