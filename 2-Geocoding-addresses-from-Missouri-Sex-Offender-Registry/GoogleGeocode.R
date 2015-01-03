# Google Geocoding
# http://code.google.com/apis/maps/documentation/geocoding/index.html
#
# Earl F Glynn, Franklin Center for Government & Public Integrity, 15 Dec. 2010.

library(XML)     # htmlTreeParse

################################################################################

# Function to call Google's Geocoding API for given street, city and state.
# "id" could be used to add a "key" to the data record being processed.
get.geocode <- function (id, street, city, state, zip)
{

  street <- gsub("#", "Apt+", street)   # kludge fix for Apartment number symbol, 22 Jan 2010
  address <- paste(street, city, state, zip, sep=", ")
  URL <- paste("http://maps.googleapis.com/maps/api/geocode/xml?address=",
               gsub(" ", "+", address), "&sensor=false", sep="")
  xml <- htmlTreeParse(URL, useInternal=TRUE)

  # http://code.google.com/apis/maps/documentation/webservices/index.html#XMLParsing
  # http://code.google.com/apis/maps/documentation/geocoding/index.html#StatusCodes
  status <- unlist(xpathApply(xml, "//status", xmlValue))

  # if more than one, pick only the first in result[1]
  if (status == "OK")
  {
    results <- getNodeSet(xml, "//result")
    result.count <- length(results)     # study why this is ever > 1

    formatted.address <- unlist(xpathApply(xml, "//result[1]/formatted_address", xmlValue))
    if (is.null(formatted.address)) formatted.address <- ""
    zip5              <- unlist(xpathApply(xml, "//result[1]/address_component[* = 'postal_code']/long_name", xmlValue))
    if (is.null(zip5)) zip5 <- ""

    # use "state.code" since "state.name" is a defined R dataset
    state.code        <- unlist(xpathApply(xml, "//result[1]/address_component[* = 'administrative_area_level_1']/long_name", xmlValue))
    if (is.null(state.code)) state.code <- ""

    county            <- unlist(xpathApply(xml, "//result[1]/address_component[* = 'administrative_area_level_2']/long_name", xmlValue))
    if (is.null(county)) county <- ""

    lat <- unlist(xpathApply(xml, "//result[1]/geometry/location/lat", xmlValue))
    if (is.null(lat)) lat <- ""
    lng <- unlist(xpathApply(xml, "//result[1]/geometry/location/lng", xmlValue))
    if (is.null(lng)) lng <- ""

    location.type <- unlist(xpathApply(xml, "//result[1]/geometry/location_type", xmlValue))
    if (is.null(location.type)) location.type <- ""
  } else {
    formatted.address <- ""
    zip5 <- ""
    state.code <- ""
    county <- ""
    lat <- ""
    lng <- ""
    location.type <- ""
    result.count <- 0
  }

  free(xml)

  data.frame(id, status, street, city, state, state.code, zip5, county,
             lat, lng, location.type,
             formatted.address, result.count, stringsAsFactors=FALSE)
}
