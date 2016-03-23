#' @title Setup Parallel
#' @description Sets up parallel processing.  Used if called from a computer running OSX
#' @import doMC
#' @param threads The number of "cores" (or threads) to use when parallelizing processes
#' @return An indicator that can be passed to the .parallel argument of dplyr methods
#'
rimaParallel <- function(threads = NULL) {
	if (!is.null(threads)) {
		doMC::registerDoMC(cores = threads)
		return(TRUE)
	} else {
		return(FALSE)
	}
}

