# Package for Scraping Educational Accountability Data from RIDE/FRED
Makes it a bit easier to access publicly available data from the Rhode Island Department of Education and their Frequently Requested Educational Data website.

## Examples

```R
# If you're using OSX or another platform that supports the doMC package
# set up to use 8 cores in parallel
useParallel <- rideR::rimaParallel(8)

# Get the list of candidate sites with downloadable content on the FRED pages
fredSites <- rideR::getFred(parallel = useParallel)

# Download all available content from FRED
rideR::dlFredData(fredSites, parallel = useParallel, meta = TRUE, output = "~/Desktop/fredMetaData.csv")

# Get accountability report card data
eseaReportCards <- rideR::rideReportCards(useParallel)
```


