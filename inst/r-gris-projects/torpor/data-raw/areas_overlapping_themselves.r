library(sf)
library(dplyr)
library(rangl)
vpal <- function(n) {
  if (!is.null(dim(n))) {
    n <- nrow(n)
  }
  viridis::viridis(n)
}
p_self <- st_sfc(st_polygon(list(cbind(c(0, 0, 1, -0.1, -0.1, 1.5, 0), c(0, 1, 1, 0.2, 1.2, 1.2, 0)))))

pd <- data.frame(a = 1)
pd[["geometry"]] <- p_self
pd <- st_as_sf(pd)


ri <- rangl(torpor:::sf_as_sp_l(pd))
pi <- rangl(torpor:::sf_as_sp_p(pd))
si <- torpor:::rg_as_sf(ri)
plot(si, col = vpal(si))

nodal <- rangl(rgeos::gNode(as(si, "Spatial")))
nodal$v  %>% anti_join(ri$v, c("x_", "y_")) %>% inner_join(nodal$lXv)

## pick out the component segments
nodal$v  %>% anti_join(ri$v, c("x_", "y_")) %>%
  inner_join(nodal$lXv) %>%
  select(-x_, -y_, -vertex_) %>%
  inner_join(nodal$lXv) %>%
  inner_join(nodal$v) #%>%
  #select(x_, y_) %>% points()


## re-compose the component segments to replace the original
##...
