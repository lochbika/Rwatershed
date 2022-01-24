#' @title Watershed algorithm
#' @description Performs the watershed algorithm on a matrix
#' @param x input matrix
#' @param method Chose the method to use. Must be a character. Currently supported method is "simple".
#' @param periodic Use periodioc boundary conditions? Default: False.
#' @return Returns a matrix of the same dimensions as x.
#' @source This function is part of the following repository at \url{https://github.com/lochbika/Rwatershed}
#' @export
#' @useDynLib Rwatershed

watershed <- function(x,
                      method = "simple",
                      periodic = FALSE) {
  # only allow a matrix as input
  if (!is.matrix(x)) {
    stop("Error: x must be a matrix!")
  }
  # get dimensions of input matrix for fortran
  nx <- dim(x)[2]
  ny <- dim(x)[1]
  # call the fortran subroutine
  if (method == "simple") {
    retdata <- .Fortran(
      "watershed_simple",
      # the input matrix x
      indata = as.double(x),
      # the output matrix
      outdata = t(matrix(rep(
        as.integer(0), nx * ny
      ), ncol = ny)),
      # dimension size x = dim y in fortran
      nx = as.integer(ny),
      # dimension size y = dim x in fortran
      ny = as.integer(nx),
      # periodic boundary conditions?
      periodici = periodic
    )
  }else{
    stop("Error: Unknown method!")
  }
  # return the data
  return(retdata$outdata)
}
