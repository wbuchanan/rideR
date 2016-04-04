#' @title dlFredData
#' @description Uses Selenium to simulate website interactions to download data
#' @param sites the data frame returned by the getFred function
#' @param useChrome indicates if Selenium should drive an instance of a Chrome browser
#' @param parallel Indicator of whether or not to use parallelized function calls
#' when
#' @param meta Indicates whether or not the file meta data should be scraped and
#' put into an file
#' @param output The name of the file to save the metadata in
#' @importFrom plyr llply ldply
#' @import magrittr
#' @importFrom rvest html_nodes html_attr
#' @importFrom xml2 read_html
#' @import RSelenium
#' @export dlFredData
#' @examples \donttest{
#'
#' # Set up parallel processing if wanted
#' useParallel <- rideR::rideParallel(8)
#'
#' # Search over first 100,000 pages with IDs [0, 100000]
#' urls <- rideR::getFred(isParallel = useParallel)
#'
#' # Download the files and save/scrape the metadata
#' dlFredData(urls, parallel = useParallel, meta = TRUE, output = "~/Desktop/fredMetaData.csv")
#' }
#'

dlFredData <- function(sites, useChrome = TRUE, parallel = FALSE, meta = FALSE,
					   output = NULL) {

	# Throw an error for illegal parameter combination
	if (meta == TRUE & is.null(output)) stop("Must specify an output file with meta = TRUE")

	# Start selenium server
	RSelenium::startServer()

	# Creates a chrome profile
	chromeProfile <- RSelenium::getChromeProfile("~/Library/Application Support/Google/Chrome", "Profile")

	# Create a driver instance
	remDr <- RSelenium::remoteDriver(browserName = "chrome", extraCapabilities = chromeProfile)

	# Wait a few seconds for the JVM to get started
	Sys.sleep(2.5)

	# Open the browser
	remDr$open()

	# Waits for browser to open for 5 seconds
	Sys.sleep(5)

	# substitute the URL suffix to the page that already has the download link
	# clicked
	siteList <- as.list(gsub("download=no", "download=yes", sites$url))

	# Loop over the URLs in the data frame
	for(i in c(1:length(siteList))) {

		# Navigate to the site
		remDr$navigate(siteList[[i]])

		# Check the accept disclaimer button
		remDr$findElement(using = "id", value = "ckbAcceptDisclaimer")$clickElement()

		# Get the
		remDr$findElement(using = "id", value = "btnDownload")$clickElement()

		# Pause for 2.5 seconds every 25 pages
		if ((i %% 25) == 0) Sys.sleep(2.5)

		# Close an open a new browser every 50 pages and wait for 5 seconds
		# before moving to the next URL
		else if ((i %% 50) == 0) remDr$close(); remDr$open(); Sys.sleep(5)

	} # End Loop over URLs

	# IF user wants metadata as well
	if (meta == TRUE) {

		# Get the meta data from all the files
		metaData <- plyr::ldply(siteList, .parallel = parallel, .fun = function(x) {

			# Reads the HTML from the url, gets the DOM element containing the
			# table, then selects all of the table cell elements
			i <- xml2::read_html(x) %>%
					rvest::html_node(".flowPanel") %>%
					rvest::html_nodes("td")

			# Creates a list with each of the table elements then convert to a
			# data frame object
			list(
			"File Name" = rvest::html_nodes(i, "#lblFileName") %>% rvest::html_text(),
			"File ID" = rvest::html_nodes(i, "#lblFileID") %>% rvest::html_text(),
			"Title" = rvest::html_nodes(i, "#lblTitle") %>% rvest::html_text(),
			"School Year" = rvest::html_nodes(i, "#lblSchYear") %>% rvest::html_text(),
			"Description" = rvest::html_nodes(i, "#lblDescription") %>% rvest::html_text(),
			"Purpose" = rvest::html_nodes(i, "#lblPurpose") %>% rvest::html_text(),
			"Keywords" = rvest::html_nodes(i, "#lblKeywords") %>% rvest::html_text()) %>%
				dplyr::as_data_frame()

		}) # End ldply call

		# If output file is specified will write the meta data to a csv file
		if (!is.null(output) == TRUE) write.csv(metaData, file = output, row.names = FALSE)

	} # End IF Block for meta data handling

} # End Function definition



