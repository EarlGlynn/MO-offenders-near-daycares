# Google Geocoding of Missouri Sex Offenders
# http://code.google.com/apis/maps/documentation/geocoding/index.html
#
# Input file:   geocode-MO-offender-in.txt
# Output file:  geocode-MO-offender-out.txt
# Note:  Use "out" file as next "in" file after daily quota exhausted.
# Be sure to fix status column replacing OVER_QUERY_LIMIT.

# Earl F Glynn, Franklin Center for Government & Public Integrity, 21 June 2011

################################################################################

basedir <- "C:/Franklin/Projects/2011/KSHB-Sex-Offenders/Geocoding-Missouri-Sex-Offenders/"     ##### CHANGE BY PROJECT #####

setwd(basedir)
source("GoogleGeocode.R")

filename <- paste("geocode-MO-offender-in.txt")  ##### CHANGE BY PROJECT #####

d <- read.delim(filename, as.is=TRUE)

i <- 1
geocoding <- TRUE
continue <- TRUE
while (geocoding)
{
  # Only process rows from file if status field is empty.
  if ( (is.na(d$status[i])) || (nchar(d$status[i]) == 0) )
  {
    street <- d$Address[i]
    city   <- d$City[i]
    state  <- d$St[i]
    zip    <- d$Zip[i]
    x <- get.geocode(i, street, city, state, zip)

    d$status[i]            <- x$status[1]
    d$zip[i]               <- x$zip5[1]
    d$county[i]            <- x$county[1]
    d$state[i]             <- x$state.code[1]
    d$lat[i]               <- x$lat[1]
    d$lng[i]               <- x$lng[1]
    d$location.type[i]     <- x$location.type[1]
    d$formatted.address[i] <- x$formatted.address[1]
    d$result.count[i]      <- x$result.count[1]

    # stop when over daily quota
    continue <- (x$status[1] != "OVER_QUERY_LIMIT")

    cat(i, d$Provider.Number[i], street, city, state, x$status[1], "\n")
    flush.console()

    Sys.sleep(0.5)  # don't hit server too often
  }
  i <- i + 1
  geocoding <- (i <= nrow(d)) && continue
}

write.table(d, file="geocode-MO-offender-out.txt",  ##### CHANGE BY PROJECT #####
            sep="\t", row.names=FALSE)
