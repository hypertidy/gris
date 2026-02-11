context("overlapping-areas")

library(sf)
p_self <- st_sfc(st_polygon(list(cbind(c(0, 0, 1, -0.1, -0.1, 1.5, 0),
                                       c(0, 1, 1, 0.2, 1.2, 1.2, 0)))))
plot(p_self)
p_valid <- st_sfc(st_polygon(list(cbind(c(0, 0, 1, 0,          0,  -0.1, -0.1, 1.5, 0),
                                        c(0, 0.272727272727272, 1, 1, 0.272727272727272,  0.2, 1.2, 1.2, 0)))))



st_sfc(p_self, p_valid)

test_that("the triangulate and rebuild fix works", {
  expect_equal(fix_overlapping_area(p_self), p_valid)
})
