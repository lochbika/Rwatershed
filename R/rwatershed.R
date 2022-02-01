#' @title An R implementation of a watershed segmentation
#' @description This function should not be called directly. Instead, use watershed.
#' @param x input matrix
#' @param periodic Use periodioc boundary conditions? Default: TRUE.
#' @return Returns a matrix of the same dimensions as x.
#' @source This function is part of the following repository at \url{https://github.com/lochbika/Rwatershed}
#' @export
#' @import magic
#' @import reshape2

rwatershed <- function(x) {
  # index of shift directions
  # 1 2 3
  # 4 5 6
  # 7 8 9
  shift.vectors <- matrix(
    c(1, 1,   #1
      1, 0,   #2
      1, -1,  #3
      0, 1,   #4
      0, 0,   #5
      0, -1,  #6
      -1, 1,  #7
      -1, 0,  #8
      -1, -1),#9
      ncol = 2,
      byrow = TRUE
    )
    # create 9 shifted copies; 5 is the original
    data.shifted <- array(0.0, dim = c(dim(x)[1], dim(x)[2], 9))
    for (i in 1:9) {
      data.shifted[, , i] <- magic::ashift(x, v = shift.vectors[i, ])
    }
    # get the direction to the neighbors at each pixel
    # if a drop of water would have to follow the gradient
    data.direction <-
      reshape2::melt(apply(data.shifted, FUN = which.min, MARGIN = c(1, 2)))
    # generate initial coordinates for each drop of the input matrix
    init.coor <- expand.grid(seq(1, dim(x)[1]), seq(1, dim(x)[2]))
    # initialize, then loop and let drops flow along the gradient
    changed <- TRUE
    old.coor <- init.coor
    new.coor <- init.coor
    while (changed) {
      # calculate new coordinates
      new.coor[, 1] <-
        old.coor[, 1] - shift.vectors[data.direction[(old.coor[, 2] - 1) * dim(x)[1] + old.coor[, 1], 3], 1]
      new.coor[, 2] <-
        old.coor[, 2] - shift.vectors[data.direction[(old.coor[, 2] - 1) * dim(x)[1] + old.coor[, 1], 3], 2]
      # boundary checks
      new.coor[new.coor[, 1] > dim(x)[1], 1] <- 1
      new.coor[new.coor[, 2] > dim(x)[2], 2] <- 1
      new.coor[new.coor[, 1] < 1, 1] <- dim(x)[1]
      new.coor[new.coor[, 2] < 1, 2] <- dim(x)[2]
      # check if anything changed
      if (all(new.coor == old.coor)) {
        changed <- FALSE
      } else{
        # update old.coor for next iteration
        old.coor <- new.coor
      }
    }

    # fill matrix with labels for segments
    data.output <- matrix(0, nrow = dim(x)[1], ncol = dim(x)[2])
    loc.min <- as.integer(rownames(data.direction[data.direction[, 3]==5, c(1, 2)]))

    for (i in 1:length(loc.min)) {
      data.output[seq(1, dim(x)[1] * dim(x)[2])[(old.coor[, 2] - 1) * dim(x)[1] + old.coor[, 1] ==
                                                  loc.min[i]]] <- i
    }
    return(data.output)
}
