#' @title getFred
#' @description A brute force approach to identifying candidate URLs that may
#' have Frequently Requested Educational Data files for download.
#' @param start Defines the FileID that the function will begin the search
#' @param end Defines the FileID that the function will end the search
#' @param candidatesOnly a Boolean used to return only the URLs that contain
#' a link used to trigger options for downloading the associated data
#' @param outFile A file path where a csv containing the data will be written to
#' (useful if the data will be shared with others and/or to maintain a list in
#' MS Excel or a database).
#' @param outObject A file path where an R object will be stored (most useful if
#' you need to run parts of the process at different times)
#' @param isParallel a Boolean passed to internal function calls used to process
#' the data in parallel
#' @importFrom plyr llply ldply
#' @import magrittr
#' @importFrom rvest html_nodes html_attr
#' @importFrom xml2 read_html
#' @return A data frame containing the URLs and Booleans indicating if the URL
#' contains the link used to trigger download options
#'

getFred <- function(start = 0, end = 100000, candidatesOnly = TRUE,
					outFile = NULL, outObject = NULL, isParallel = FALSE) {

	# Internal function used to construct the list of URLs over which the search
	# will be performed
	fredSites <- function(starting, ending) {

		# Defines the base URL that will have ID values added to it to construct
		# the full URL
		base <- "http://www.eride.ri.gov/FileExchange/fredDetails.aspx?fileID="

		# Remaining portion of the URL that needs to be appended after the file ID
		suffix <- "&download=no"

		# Creates a list of URLs across the range of ID values passed to the
		# fredSites function
		plyr::llply(as.list(c(start:end)), .parallel = isParallel, .fun = function(x) {

			# Builds URLS of the form:
			# http://www.eride.ri.gov/FileExchange/fredDetails.aspx?fileID=1&download=no
			# where the value 1 would be a value in the range of starting/ending
			paste0(base, x, suffix)

		}) # End llply call

	} # End Function definition

	# Method used to read the URL and check for the link used to trigger
	# downloading the file and accepting the end user agreement
	hasDownloadButton <- function(url) {

		# Try function calls below, if an error is thrown catch it and return
		# FALSE in its place, and print the URL value
		tryCatch(
			   # Read the HTML from the site passed to the function
			   # then search for a the cell within a row of a table DOM element
			   # Extract the second element then search for a link within it,
			   # Extract the first link and test whether the ID for said link
			   # has the value urlDownloadFile
			   (xml2::read_html(url) %>%
		    	rvest::html_nodes(xpath = "//table//tr//td") %>%
				extract(2) %>% rvest::html_nodes("a") %>%
				extract(1) %>% rvest::html_attr("id") == "urlDownloadFile"),
				# Returns false if the link doesn't exist (this link would indicate)
				# the availability of data from this URL
				error = function(e) FALSE,
				# Prints the URL
				finally = print(url))

	} # End Function definition

	# Creates a data frame with the URLs and Booleans indicating whether the
	# URL includes the download link
	data <- plyr::ldply(fredSites(start, end), .parallel = isParallel,
						.fun = function(x) {

		# Creates a list of booleans indicating the presence of the download link
		datum <- list(hasDownloadButton(x));

		# Creates a list of URLs
		uri <- list(x)

		# Names each of the lists above
		names(datum) <- "hasdlbutton"; names(uri) <- "url"

		# Creates a data frame with the URLs and booleans
		dplyr::bind_cols(uri, datum)

	}) # End of ldply call

	# If only the candidate sites should be returned this filters out cases where
	# there is a download button on the site
	if (candidatesOnly) data <- dplyr::filter(data, hasdlbutton == TRUE)

	# Will serialize the data frame to disk
	if (!is.null(outObject)) saveRDS(smallData, file = outObject)

	# Will serialize the data frame to a csv file
	if (!is.null(outFile)) write.csv(smallData, file = outFile, row.names = FALSE)

	# Returns the data object
	return(data)

} # End of function definition


