# Single file of combined offenders and providers from geocoding and distance
# analysis is needed as input for BatchGeo.com.  This file selects
# offenders and providers within 1000 feet of each other.

# Analysis for Ryan Kath, KSHB NBC Action News

# Earl F Glynn, Franklin Center for Government & Public Integrity
# 13 July 2011.  Updated 17 July 2011.

basedir <- "C:/Franklin/Projects/2011/KSHB-Sex-Offenders/"
setwd(basedir)

### Offenders

# Offender "master file" with information about offenses
offense <- read.csv("msor-offender-master-file.csv", as.is=TRUE)

# Let's create a compound key for matching files
offense$key  <- paste(offense$Name, offense$Address, offense$City, offense$St,
                      offense$Zip, offense$County, offense$Compliant, sep="|")

# Offender distance information
offender <- read.delim("ChurchDaycare-min-distance-from-offender-to-any-provider.txt", as.is=TRUE)
dim(offender)

offender$key <- paste(offender$Name, offender$Address, offender$City, offender$St,
                      offender$Zip, offender$County, offender$Compliant, sep="|")

# Select offenders within 1000 feet of a provider
offender <- offender[offender$min.dist.feet.to.provider <=1000,]
dim(offender)

# Knock out "Address Unknown" (fix upstream next time)
offender <- offender[-which(offender$Address == "ADDRESS UNKNOWN"),]

# Add new column
offender$Category <- "Offender"
offender$Comments <- paste("Geocode ", offender$location.type, "; ",
                           ifelse(offender$Compliant == "N", "NOT Compliant", "Compliant"),
                           "; ", sep="")

# Lookup offenses for offender in master file
for (i in 1:nrow(offender))
{
  offender$Comments[i] <- paste(offender$Comments[i], offense$Comments[which(offender$key[i] == offense$key)], sep="")
}

offender <- offender[,c("Category", "Name", "Address", "City", "St", "Zip", "County", "lat", "lng", "min.dist.feet.to.provider", "providers.too.close","Comments")]
colnames(offender) <- c("Category", "Name", "Address", "City", "State", "Zip", "County", "Latitude", "Longitude", "DistanceFeet", "CountTooClose", "Comments")
dim(offender)

### Providers
provider <- read.delim("ChurchDaycare-min-distance-from-provider-to-any-offender.txt", as.is=TRUE)
dim(provider)

# Select providers within 1000 feet of an offender
provider <- provider[provider$min.dist.feet.to.offender <= 1000,]
dim(provider)
provider$Category <- "ChurchDayCare"
provider$Comments <- paste("Geocode", provider$location.type)

provider <- provider[,c("Category", "FACILITYNAME", "STREET", "CITY", "STATE", "ZIP", "county", "lat", "lng", "min.dist.feet.to.offender", "offenders.too.close","Comments")]
colnames(provider) <- c("Category", "Name", "Address", "City", "State", "Zip", "County", "Latitude", "Longitude", "DistanceFeet", "CountTooClose", "Comments")
dim(provider)

combined <- rbind(offender, provider)
write.csv(combined, "ProviderOffenderGoogleMaps.csv", row.names=FALSE)
