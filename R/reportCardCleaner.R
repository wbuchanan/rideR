#' @title reportCardCleaner
#' @description Used to scrape and organize contents of report card websites
#' @param page the URL whose content is to be parsed
#' @param school Whether the pages being parsed are school report cards
#' @import magrittr
#' @importFrom rvest html_nodes html_attr html_node html_text
#' @importFrom xml2 read_html
#' @examples \donttest{
#' # Get report card content quickly
#' para <- rideR::rideParallel(8)
#'
#' # Gets the URLs with the report card data
#' rideAMO <- rideReportCards(para)
#'
#' # Get all of the report card data for Districts
#' distData <- plyr::ldply(rideAMO[[1]], .fun = reportCardCleaner, school = FALSE,
#' 						.parallel = para)
#'
#' # And saves the data frame as a file that can be used elsewhere
#' write.csv(distData, "~/Desktop/districtReportCards.csv", row.names = FALSE)
#'
#' # Get all of the report card data for Districts
#' schData <- plyr::ldply(rideAMO[[2]], .fun = reportCardCleaner, .parallel = para)
#'
#' # And saves the data frame as a file that can be used elsewhere
#' write.csv(schData, "~/Desktop/schoolReportCards.csv", row.names = FALSE)
#'
#' }
#' @return A dataframe containing the report card website contents
#' @export reportCardCleaner
#'

reportCardCleaner <- function(page, school = TRUE) {

	# Utility function so gsub can be called using pipes
	revgs <- function(datum, pattern, sub) {
		return(gsub(pattern, sub, datum))
	}

	# Organization type strings
	if (school) orgType <- "school" %>% as.data.frame(stringsAsFactors = FALSE)
	else orgType <- "district" %>% as.data.frame(stringsAsFactors = FALSE)

	# Gets the LEA ID or NA value
	leaid <- stringr::str_match(page, "(leaCode=)([0-9]+)")[[3]]

	# Gets the school ID or NA value
	schid <- stringr::str_match(page, "(schCode=)([0-9]+)")[[3]]

	# Gets the educational level of the organization
	levid <- stringr::str_match(page, "((lea)|(sch))(Type=)([0-9])")[[6]] %>%
			 as.numeric()

	# Gets the school year from the page URL
	schyr <- paste0("20", stringr::str_match(page, "(/)([0-9]{2})(/)")[[3]]) %>%
		     as.numeric()

	# Get the HTML from the page
	pageData <- xml2::read_html(page)

	# For districts
	if (school == FALSE) {

		# Get the page header use these values for the first three column names
		pageHeaders <-  pageData %>%
						rvest::html_nodes("td .header-text") %>%
						rvest::html_text() %>% unlist()

		# Grabs the corresponding values from the header these become the values in
		# the first three columns/variables
		pageHeaderValues <-  pageData %>%
							 rvest::html_nodes("td .header-heading-smaller") %>%
							 rvest::html_text() %>%
							 revgs("%", "") %>% rbind() %>%
							 as.data.frame(stringsAsFactors = FALSE) %>%
	 						 cbind(orgType,
	 						 	  as.data.frame(leaid, stringsAsFactors = FALSE),
	 						 	  as.data.frame(schid, stringsAsFactors = FALSE),
	 						 	  levid, schyr)

		# Applies variable names to the data frame
		names(pageHeaderValues) <- c("org", "level", "attndrate", "type",
									 "leaid", "schid", "levid", "schyr")

		# Makes sure the attendance rate is numeric
		pageHeaderValues$attndrate <- as.numeric(pageHeaderValues$attndrate)

	# For schools
	} else {

		# Gets the school name from the HTML
		org <- 	pageData %>% rvest::html_node("#lblSchoolName") %>%
				rvest::html_text() %>% rbind() %>%
				as.data.frame(stringsAsFactors = FALSE)

		# Creates the text label for the report card level
		level <- ifelse(levid == 1, "Elementary",
				 ifelse(levid == 2, "Middle", "High")) %>%
				 as.data.frame(stringsAsFactors = FALSE)

		# No attendance rate on the school pages so set this to NA
		attndrate <- as.numeric(NA)

		# Create the row for the data frame
		pageHeaderValues <- cbind(as.data.frame(org, stringsAsFactors = FALSE),
								  as.data.frame(level, stringsAsFactors = FALSE),
								  attndrate, orgType,
								  as.data.frame(leaid, stringsAsFactors = FALSE),
								  as.data.frame(schid, stringsAsFactors = FALSE),
								  levid, schyr) %>%
							as.data.frame(stringsAsFactors = FALSE)

		# Appends names to the data frame object
		names(pageHeaderValues) <- c("org", "level", "attndrate", "type",
							 "leaid", "schid", "levid", "schyr")

	} # End ELSE Block for school pages

	# Labels for the rows from the tables
	tabLabs <- 	c("amogroup", "readorg", "readerr", "readtarget", "readamo",
				  "mathorg", "matherr", "mathtarget", "mathamo", "readpartic",
				  "readparticamo", "mathpartic", "mathparticamo")

	# Creates an empty variable used to fillin dataframes for consistency
	readerr <- as.data.frame(cbind(NA), stringsAsFactors = FALSE)
	readtarget <- as.data.frame(cbind(NA), stringsAsFactors = FALSE)
	readamo <- as.data.frame(cbind(NA), stringsAsFactors = FALSE)
	matherr <- as.data.frame(cbind(NA), stringsAsFactors = FALSE)
	mathtarget <- as.data.frame(cbind(NA), stringsAsFactors = FALSE)
	mathamo <- as.data.frame(cbind(NA), stringsAsFactors = FALSE)

	# Gets the data from the table
	tableData <- pageData %>%
				 rvest::html_nodes(".table1") %>%
				 rvest::html_nodes("tr")

	# For school pages, the first row with data is 14
	if (school == TRUE) startpos <- 14

	# For districts the first row with data is 12
	else startpos <- 12

	# Get the number of rows
	tablerows <- (length(tableData) - startpos) + 1

	# If there are the appropriate number of rows in the HTML
	if (tablerows >= 11) {

		# Gets the table data and recodes values into numeric formats including
		# missing/NA data types
		rows <- plyr::ldply(as.list(c(startpos:length(tableData))), .fun = function(x) {

				# Parse the rows of the table from the HTML
				data <- rvest::html_nodes(tableData[[x]], "td") %>%
					    rvest::html_text() %>%
						revgs("NO†", "-3") %>% revgs("NO‡", "-4") %>%
					    revgs("YES", "1") %>% revgs("NO", "0") %>%
					    revgs("\\*\\*", "-2") %>% revgs("\\*", "-1") %>%
					    revgs("All Students", "1") %>%
					    revgs("African-American", "2") %>%
					    revgs("Asian", "3") %>%
					    revgs("Pacific Islander", "4") %>%
					    revgs("Hispanic", "5") %>%
					    revgs("Native American", "6") %>%
					    revgs("White", "7") %>%
					    revgs("Multi-Racial", "8") %>%
					    revgs("Students with Disabilities", "9") %>%
					    revgs("English-Language Learners", "10") %>%
					    revgs("Economically Disadvantaged Students", "11") %>%
					    revgs("Minority Consolidated Sub-Group", "12") %>%
					    revgs("Program Consolidated Sub-Group", "13") %>%
						as.numeric() %>% rbind() %>%
					    as.data.frame(stringsAsFactors = FALSE)

				# If there are only 7 variables parsed
				if (ncol(data) == 7) {

					# Fills the data frame with missing values for this config
					data <- cbind(data[, 1], data[, 2], readerr, readtarget, readamo,
								  data[, 3], matherr, mathtarget, mathamo,
								  data[, 4], data[, 5], data[, 6], data[, 7])  %>% as.data.frame()

					# Apply variable names to the data frame
					names(data) <- tabLabs

					# Returns the data frame object
					return(data)

				# When there are 17 variables parsed
				} else if (ncol(data) == 17) {

					# Select specific variables/positions to keep returned result
					# consistent regardless of page config
					data <- cbind(data[, 1], data[, 2], readerr, readtarget,
								  data[, 3], data[, 6], matherr, mathtarget,
								  data[, 7], data[, 10], data[, 11],
								  data[, 14], data[, 15]) %>%
							as.data.frame()

					# Apply variable names to the data frame
					names(data) <- tabLabs

					# Returns the data frame object
					return(data)

				# Case where only a single variable is parsed (happens with some
				# of the earlier school report cards and results in a single NA
				# value in the variable V1)
				} else if (ncol(data) == 1) {

					# Return nothing in this case
					return(NULL)

				# All other scenarios
				} else {

					# Apply variable names to the data frame
					names(data) <- tabLabs

					# Returns the data frame object
					return(data)

				} # Ends ELSE Block for the parsing

			}) # End of ldply function call


		# Used to fill observations for years prior to 2013
		filler <- cbind(c(12, 13), rbind(rbind(rep(NA, 12)), rbind(rep(NA, 12)))) %>%
				  as.data.frame()

		# Name the columsn
		names(filler) <- tabLabs

		# For years prior to 2013 this will fill in the additional sub-group records
		if (tablerows == 11) rows %<>% dplyr::bind_rows(filler)

	# For malformed HTML tables
	} else {

		# Creates a completely null data frame
		rows <- cbind(c(1:13), rbind(rbind(rep(NA, 12)), rbind(rep(NA, 12)),
						rbind(rep(NA, 12)), rbind(rep(NA, 12)), rbind(rep(NA, 12)),
						rbind(rep(NA, 12)), rbind(rep(NA, 12)), rbind(rep(NA, 12)),
						rbind(rep(NA, 12)), rbind(rep(NA, 12)), rbind(rep(NA, 12)),
						rbind(rep(NA, 12)), rbind(rep(NA, 12)))) %>%
					as.data.frame()

		# Attaches the names to the variables
		names(rows) <- tabLabs

	} # End ELSE Block for inconsistent page config

	# Number of times to repeat the header
	repnum <- nrow(rows)

	# Repeats the header values to be the same length as the tabular data from
	# the page
	pageHeaderValues <- pageHeaderValues[rep(seq_len(nrow(pageHeaderValues)), repnum),]

	# Data object returned by the function if no grad rates present
	retData <- reshape(cbind(pageHeaderValues, rows),
					   v.names = c("amogroup", "readorg", "readerr",
					   			   "readtarget", "readamo", "mathorg",
					   			   "matherr", "mathtarget", "mathamo",
					   			   "readpartic", "readparticamo", "mathpartic",
					   			   "mathparticamo"),
					   timevar = "amogroup", direction = "wide",
					   idvar = c("org", "level", "attndrate", "type", "leaid",
					   		  "schid", "levid", "schyr"))

	# Test to see if the URL contains graduation rates
	# If so, get the graduation rate table data
	if (grepl(".*Type=3", page) == TRUE) {

		# Need to check if page is leaType=3 or schoolType=3
		gradData <- pageData %>%
					rvest::html_node(".table2") %>%
					rvest::html_nodes("tr")

		# Gets and cleans the table headers
		gradNames <- gradData %>%
					rvest::html_nodes("td.header") %>%
					rvest::html_text() %>%
					revgs("\\W", "") %>% tolower()

		# Parses/cleans the table contents
		gradData %<>% 	rvest::html_nodes("td.data") %>%
						rvest::html_text() %>%
						revgs("NO†", "-3") %>% revgs("NO‡", "-4") %>%
						revgs("\\W", "") %>% revgs("YES", "1") %>%
						revgs("NO", "0") %>% revgs("\\*\\*", "-2") %>%
						revgs("\\*", "-1") %>% as.numeric() %>% rbind() %>%
						as.data.frame(stringsAsFactors = FALSE)

		# Attaches names to the data
		names(gradData) <- gradNames

		# Bind these data to the return data object
		retData %<>% cbind(gradData)

	# Ends IF Block for cases with graduation rate data
	} else {

		# Creates an empty vector for cases where there are no graduation rates
		gradData <- rbind(c(rep(NA, 7)))

		# Attaches the same names to the vector
		names(gradData) <- c("4year", "5year", "6year", "composite", "final",
							 "error", "targetmet")

		# Adds the missing values for cases without graduation rates
		retData %<>% cbind(gradData)

	} # Ends ELSE Block for cases of schools/districts without graduation rates

	# Only schools have AMOs, so this
	if (school) {

		# Gets the AMO target table data
		targetTab <- pageData %>%
					 rvest::html_node(".table3") %>%
					 rvest::html_nodes("tr") %>%
					 rvest::html_nodes("td.data") %>%
					 rvest::html_text() %>% as.numeric() %>% rbind() %>%
					 as.data.frame(stringsAsFactors = FALSE)

		# For malformed AMO target tables (happens starting in 2015)
		if (ncol(targetTab) != 2) targetTab <- as.numeric(c(NA, NA)) %>% rbind() %>% as.data.frame()

		# Adds names to the variables
		names(targetTab) <- c("targetsmet", "targetseval")

		# Adds the status of the AMO targets
		retData %<>% cbind(targetTab)

	} # End IF Block for school AMO target types

	# Removes periods from variable names
	names(retData) <- gsub("\\.", "_", names(retData))

	# Returns the data object
	return(retData)

} # End of function definition

