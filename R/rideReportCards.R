#' @title rideReportCards
#' @description Scrapes ESEA accountability report card data from RIDE
#' @param parallel Indicator of whether or not to use parallelized function calls
#' when
#' @param distout The name of the file where the district level html will be saved
#' @param schout The name of the file where the school level html will be saved
#' @importFrom plyr llply
#' @import magrittr
#' @importFrom rvest html_nodes html_attr html_node
#' @importFrom xml2 read_html
#' @examples \donttest{
#' # Get report card content quickly
#' rideAMO <- rima::rimaParallel(8) %>% rideReportCards()
#' }
#' @return A list containing the list of district and list of school HTML content
#'

rideReportCards <- function(parallel = FALSE, distout = NULL, schout = NULL) {

	# Year values used in the construction/ID of URLS
	years <- as.list(c("05", "06", "07", "08", "09", "10", "11", "12", "13",
					   "14", "15"))

	# URL roots.  Name of the list elements corresponds to the list above
	base <- list("05" = "http://www.eride.ri.gov/reportcard/",
		 "06" = "http://www.eride.ri.gov/reportcard/",
		 "07" = "http://www.eride.ri.gov/reportcard/",
		 "08" = "http://www.eride.ri.gov/reportcard/",
		 "09" = "http://www.eride.ri.gov/reportcard/",
		 "10" = "http://www.eride.ri.gov/reportcard/",
		 "11" = "http://www.eride.ri.gov/reportcard/",
		 "12" = "http://www.eride.ri.gov/eride40/reportcards/",
		 "13" = "http://www.eride.ri.gov/eride40/reportcards/",
		 "14" = "http://www.eride.ri.gov/eride40/reportcards/",
		 "15" = "http://www.eride.ri.gov/eride40/reportcards/")

	# School suffixes for each year
	schSuffix <- list("05" = "/rcSchools.asp?rcType=schoolRC",
		 "06" = "/schools.aspx", "07" = "/schools.aspx", "08" = "/schools.aspx",
		 "09" = "/schools.aspx", "10" = "/Schools.aspx", "11" = "/Schools.aspx",
		 "12" = "/Schools.aspx", "13" = "/Schools.aspx", "14" = "/Schools.aspx",
		 "15" = "/Schools.aspx")

	# District suffixes for each year
	distSuffix <- list("05" = "/rcDistricts.asp", "06" = "/rcDistricts.asp",
		 "07" = "/rcDistricts.asp", "08" = "/rcDistricts.asp",
		 "09" = "/rcDistricts.asp",  "10" = "/Districts.aspx",
		 "11" = "/Districts.aspx", "12" = "/Districts.aspx",
		 "13" = "/Districts.aspx", "14" = "/Districts.aspx",
		 "15" = "/Districts.aspx")

	# Builds the list of District URLs
	eseaDistricts <- plyr::llply(years, .parallel = parallel, .fun = function(x) {

		# Constructs the URL to parse
		url <- paste0(base[[x]], x, distSuffix[[x]])

		# Reads the url, looks for the DOM element with ID #dgSchoolList,
		# gets all of the link nodes, and then gets the relative paths
		plyr::llply(as.list(xml2::read_html(url) %>%
		rvest::html_node("#dgSchoolList") %>%
		rvest::html_nodes("a") %>% rvest::html_attr("href")), .fun = function(y) {

			# Creates the URL for the given district by school year
			paste0(base[[x]], x, "/", y)

		}) # End inner call to llply

	# Ends outer call to llply then stores all of the urls in a flattened list
	}) %>% unlist() %>% as.list()

	# Builds the list of School URLs
	eseaSchools <- plyr::llply(years, .fun = function(x) {

		# Constructs the URL to parse
		url <- paste0(base[[x]], x, schSuffix[[x]])

		# Reads the url, looks for the DOM element with ID #dgSchoolList,
		# gets all of the link nodes, and then gets the relative paths
		plyr::llply(as.list(xml2::read_html(url) %>%
		rvest::html_node("#dgSchoolList") %>%
		rvest::html_nodes("a") %>% rvest::html_attr("href")), .fun = function(y) {

			# Creates the URL for the given school by school year
			paste0(base[[x]], x, "/", y)

		}) # End inner call to llply

	# Ends outer call to llply then stores all of the urls in a flattened list
	}) %>% unlist() %>% as.list()

	# Scrapes HTML content from each of the district URLs across years
	districts <- plyr::llply(eseaDistricts, .parallel = parallel, .fun = function(x) {

		# Gets the table elements from the URL
		tabs <- xml2::read_html(x) %>% rvest::html_nodes("table")

		# Names the list element with the URL
		names(tabs) <- x

		# Returns the HTML as a named list
		return(tabs)

	}) # End of llply call for district sites

	# Scrapes HTML content from each of the school URLs across years
	schools <- plyr::llply(eseaSchools, .parallel = parallel, .fun = function(x) {

		# Gets the table elements from the URL
		tabs <- xml2::read_html(x) %>% rvest::html_nodes("table")

		# Names the list element with the URL
		names(tabs) <- x

		# Returns the HTML as a named list
		return(tabs)

	}) # End of llply call for school sites

	# Serialize the district HTML to disk if file name is passed to function
	if (!is.null(distout)) saveRDS(districts, file = distout)

	# Serialize the school HTML to disk if file name is passed to function
	if (!is.null(schout)) saveRDS(schools, file = schout)

	# Return a list object with the HTML content
	return(list("districts" = districts, "schools" = schools))

} # End Function definition
