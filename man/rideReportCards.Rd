% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rideReportCards.R
\name{rideReportCards}
\alias{rideReportCards}
\title{rideReportCards}
\usage{
rideReportCards(parallel = FALSE, distout = NULL, schout = NULL)
}
\arguments{
\item{parallel}{Indicator of whether or not to use parallelized function calls
when}

\item{distout}{The name of the file where the district level URLs}

\item{schout}{The name of the file where the school level URLs}
}
\value{
A list containing the list of district and list of school URLs
}
\description{
Generates lists of URLs containing the HTML ESEA report cards
}
\examples{
\donttest{
# Gets the URLs containing all of the ESEA report card websites
rideAMO <- rideR::rideParallel(8) \%>\% rideR::rideReportCards()

# Shows the number of District-level report card sites
length(rideAMO[[1]])

# Shows the number of School-level report card sites
length(rideAMO[[2]])

# Save this to disk so you can read the data in the future w/o the need for
# connections
saveRDS(rcUrls, file = "~/Desktop/RIMA/reportCardUrls.Rds")
}
}

