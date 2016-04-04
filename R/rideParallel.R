#' @title Setup Parallel
#' @description Sets up parallel processing.  Used if called from a computer running OSX
#' @import doMC
#' @param threads The number of "cores" (or threads) to use when parallelizing processes
#' @return An indicator that can be passed to the .parallel argument of dplyr methods
#' @examples \donttest{
#' # Sets the number of cores to use for parallelized function calls
#' # This only has an effect on non-MS operating systems where the doMC package
#' # can be installed
#' para <- rideR::rideParallel(32)
#'
#' }
#' @export rideParallel
#'
rideParallel <- function(threads = NULL) {
	if (!is.null(threads) && .Platform$OS.type != "windows") {
		doMC::registerDoMC(cores = threads)
		return(TRUE)
	} else {
		return(FALSE)
	}
}

