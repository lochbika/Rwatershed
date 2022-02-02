# a simple demonstration

# if not already done, install devtools and then the Rwatershed package
require("devtools")
install_github("lochbika/Rwatershed",ref="main")

library("Rwatershed")

# load data pyramids
data("pyramids")

# plot data
image(pyramids)

# run and plot the watershed segmentation with the R implementation
image(watershed(pyramids, method = "R", periodic = TRUE))
# run and plot the watershed segmentation with the fortran implementation
image(watershed(pyramids, method = "fortran", periodic = TRUE))

# load data sine2d
data("sine2d")

# tile it to a bigger size
sine2d <- matrix(rep(sine2d,4), ncol = 400, byrow = TRUE)

# plot data
image(sine2d)

## performance tests

# run the watershed segmentation with the R implementation
system.time(
  watershed(sine2d, method = "R", periodic = TRUE)
)
# run the watershed segmentation with the fortran implementation
system.time(
  watershed(sine2d, method = "fortran", periodic = TRUE)
)

