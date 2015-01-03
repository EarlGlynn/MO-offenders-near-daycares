# Process Missouri Sex Offender Registry (MSOR) data transferred from bottom of page
# http://www.mshp.dps.mo.gov/MSHPWeb/PatrolDivisions/CRID/SOR/SORPage.html and
# prepare for geocoding the list.
#
# Each line in the Excel file is an offense committed by an offender.  There are
# multiple lines of offenses for some offenders.
#
# Earl F Glynn
# Franklin Center for Government & Public Integrity
# 17 July 2011
#
# Setup steps
#   1. Extract msor.xls from msor.zip
#   2. msor.xls is an XML file (20 June 2011).  Instead of parsing XML, let's
#      open in Excel and re-save as Excel file Missouri-Sex-Offenders.xls
#   3. Delete first 13 rows of summary information in Missouri-Sex-Offenders.xls
#      and save modified file.
#   4. Note worksheet name is "Sheet1"

################################################################################
### Read Missouri Sex Offender Registry (MSOR) data

library(RODBC)   # Use RODBC to read the Excel file

setwd("C:/Franklin/Projects/2011/KSHB-Sex-Offenders/Missouri-Sex-Offender-Registry/")    ### Project directory

connection <- odbcConnectExcel("Missouri-Sex-Offenders.xls")
msor <- sqlFetch(connection, "Sheet1", as.is=TRUE)
odbcClose(connection)

# Look at overview info of data.frame
dim(msor)
#  18234     9
colnames(msor)
str(msor)

################################################################################
# After reviewing the data several street addresses were noted that cannot be geocoded,
# like "Homeless" and "Unknown".  Let's review top 10 most frequent addresses.
# Note:  Some of the high-frequency adddress are prisons.

counts <- sort(table(msor$Address), decreasing=TRUE)[1:10]
data.frame(counts)

# Let's remove addresses that cannot be geocoded:

# Address                                 counts (20 June 2011)
# --------------------------------------- ------
# Incarcerated in MO                        2403
# Previously Exempt/Registration Required    749
# Moved Out of State                         593
# Incarcerated Out of State                  255
# HOMELESS                                   106
# UNKNOWN                                     94

# Other problem addresses found with lower frequencies:
# UNKNOWN ADDRESS
# ADDRESS UNKNOWN       [future]

delete.list <- c("Incarcerated in MO", "Previously Exempt/Registration Required",
                 "Moved Out of State", "Incarcerated Out of State",
                 "HOMELESS",           "UNKNOWN",
                 "UNKNOWN ADDRESS")

select <- msor$Address %in% delete.list
sum(select)

problem.addresses <- msor[select,]
write.csv(problem.addresses, "msor-problem-addresses.csv", row.names=FALSE)

dim(problem.addresses)                 # Offenses at problem addresses
# 4203    9
length(unique(problem.addresses$Name)) # Number of names of offenders at problem addresses
# 3543


# maintain original msor data.frame by using offense data.frame here
offense <- msor[!select,]
dim(offense)

# Since focus for now is on Missouri, let's exclude out-of-state names

# Stats on how many are out of state
table(offense$St)

#  AR    IA    IL    IN    KS    MO    MS    OK    PA    TN
#   7     5    63     1   102 13833     1    15     1     3

select <- offense$St != "MO"
out.of.state <- offense[select,]
write.csv(out.of.state, "msor-out-of-state.csv", row.names=FALSE)

nrow(out.of.state)
# 198

offense <- offense[!select,]
rownames(offense) <- 1:nrow(offense)  # Renumber
nrow(offense)

counts <- sort(table(offense$Offense, useNA="ifany"), decreasing=TRUE)
counts <- data.frame(Offense=names(counts), Count=as.numeric(counts))
write.csv(counts, "msor-offender-offenses-counts.csv", row.names=FALSE)

################################################################################
# Create offender list from offense list

offender <- offense[,c("Name", "Address", "City", "St", "Zip", "County", "Compliant")]
nrow(offender)
offender <- unique(offender)
nrow(offender)

# Note:  Remove "Compliant" above and the same number of offenders was found.
# Therefore, there appears to be no inconsistent "Compliant" fields by name.

# R doesn't like to put a header over the row names (or I don't know how), so
# add new first column "N" with a row number.

offender <- data.frame(N=1:nrow(offender), offender)

################################################################################
# Create input file for R geocoding script

geocode <- offender

# Add some fields to be used by the process.  By design all "original" data
# starts with an upper case letter and the new geocoding fields are all in
# lower case.  So, "Zip" is from the raw data and "zip" will be from geocoding.
# They should match but when they don't that's a sign of a geocoding problem.

# Some of the fields for geocoding are explained here
# http://code.google.com/apis/maps/documentation/geocoding/index.html

geocode$status         <- ""
geocode$zip            <- ""
geocode$county         <- ""
geocode$state          <- ""
geocode$lat            <- ""
geocode$lng            <- ""
geocode$location.type  <- ""
geocode$formatted.address  <- ""
geocode$result.count   <- ""

# Write tab-delimited file
write.table(geocode, file="geocode-MO-offender-in-MASTER.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

################################################################################
# Use all fields in offender data.frame as compound key to connect back to
# offense data.frame

offender$key <- paste(offender$Name, offender$Address, offender$City, offender$St,
                      offender$Zip, offender$County, offender$Compliant, sep="|")

offense$key  <- paste(offense$Name, offense$Address, offense$City, offense$St,
                      offense$Zip, offense$County, offense$Compliant, sep="|")

# Step throug all offenders and create a "Comments" field with the
# concatenated list of offenses.  To be used as Comments for offender
# on Google maps.

offender$Comments <- ""

# There may be a way to vectorize this and speed this up, but this brute force
# approach works for now.
for (i in 1:nrow(offender))
{
  if (i %% 1000 == 0)
  {
    cat(i, offender$key[i], "\n")
    flush.console()   # show progress
  }

  offenders.offenses <- offense[offender$key[i] == offense$key,]
  offender$Comments[i] <- paste(offenders.offenses$Offense, collapse="; ")
}

# Drop key column and write data.frame to file
offender <- offender[,-which(colnames(offender) == "key")]
write.csv(offender, "msor-offender-master-file.csv", row.names=FALSE)

# Drop first column "N"
offender <- offender[,-1]

# These files are suitable for plotting using BatchGeo.com

# Create regional subsets (leave row name indices)
select <- offender$County %in% c("BUCHANAN","CASS", "CLAY", "JACKSON", "PLATTE")
write.csv(offender[select,], "msor-offender-kc-area.csv", row.names=FALSE)

select <- offender$County %in% c("ST. CHARLES", "ST. LOUIS", "ST. LOUIS CITY", "JEFFERSON ")
write.csv(offender[select,], "msor-offender-st-louis-area.csv", row.names=FALSE)

select <- offender$County %in% c("GREENE", "JASPER", "LAWRENCE")
write.csv(offender[select,], "msor-offender-joplin-springfield-area.csv", row.names=FALSE)

################################################################################
# Create summaries

# Offender compliance by county
ByCounty <- table(offender$County, offender$Compliant)
write.csv(ByCounty,"msor-offenders-compliance-by-county.csv")

# Mulitple offenders at same address (could miss variations in spelling, standardization)
complete.address <- paste(offender$Address, offender$City, offender$St, offender$Zip,
                          offender$County, sep="|")
counts <- table(complete.address)
Above2List <- data.frame(Address=names(counts), Count=as.numeric(counts))
Above2List <- Above2List[Above2List$Count > 2,]
Above2List <- Above2List[order(Above2List$Count,decreasing=TRUE),]
write.csv(Above2List, "msor-more-than-2-offenders-at-address.csv", row.names=FALSE)
