# Plot Missouri sexual offenders / child care providers.
# Analysis for Ryan Kath, KSHB NBC Action News

# Earl F Glynn, Franklin Center for Government & Public Integrity
# 12 July 2011

library(fields)         # tools for spatial data:  rdist.earth

basedir <- "C:/Franklin/Projects/2011/KSHB-Sex-Offenders/"   ### BY PROJECT ###
setwd(basedir)

STATE <- "Missouri"                                          ### BY PROJECT ###

###############################################################################
### Process.Dataset common function

Process.Dataset <- function(INPUT.FILENAME, BAD.DATA.FILENAME)
{
  d <-  read.delim(INPUT.FILENAME, as.is=TRUE)
  cat(INPUT.FILENAME, dim(d), "\n")
  select <- (d$status == "OK")           &
            (d$result.count > 0)         &
            (d$state == STATE)

  if (sum(!select) >0)
  {
    bad.data <- d[!select,]
    write.table(bad.data, BAD.DATA.FILENAME, row.names=FALSE)
  }

  d <- d[select,]

  cat("status OK:", sum(d$status == "OK"), ", count >0 >1:", sum(d$result.count > 0), sum(d$result.count > 1),
      ", match state:", sum(d$state == STATE), ", selected:", sum(select), "\n")

  d$fuzzy <- (d$result.count > 1) | (d$location.type != "ROOFTOP")

  d
}

###############################################################################
### Read Geocoded Data

offender.data <- Process.Dataset("geocode-MO-offender-out.txt",
                                 "bad-data-offender.txt")

religious.data <- Process.Dataset("geocode-ReligiousOrganizations-out.txt",
                                  "bad-data-ReligiousOrganizations-provider.txt")

###############################################################################
# Compute distance matrix

# Create distance matrix, minimum distance stats for providers and offenders,
# and a "connections" file.
#
# Since this is a "quick and dirty" function don't worry that it mixes
# computation, console output, and creation of a PDF file.

compute.distance.matrix <- function(DATASET.NAME, OFFENDER.DATA, PROVIDER.DATA, THRESHOLD)
{
  pdf(paste(DATASET.NAME, "-min-dist-histograms.pdf",sep=""))

  cat("Provider:", DATASET.NAME, "\n")
  print(table(PROVIDER.DATA$location.type))
  provider.points <- cbind(PROVIDER.DATA$lng, PROVIDER.DATA$lat)
  cat(nrow(provider.points), "\n")

  cat("Offender:\n")
  print(table(OFFENDER.DATA$location.type))
  offender.points <- cbind(OFFENDER.DATA$lng, OFFENDER.DATA$lat)
  cat(nrow(offender.points), "\n")

  # rdist.earth creates "great circle distance matrix"
  m <- rdist.earth(provider.points, offender.points, miles=TRUE)
  cat("Matrix:", dim(m), "\n")

  m <- round(5280 * m)    # convert miles to feet and round

  flush.console()         # show progress


  ###############################################################################
  # How many providers are closer than given distance to offender

  min.dist.from.provider <- apply(m, ROW<-1, min)
  hist(min.dist.from.provider, breaks=50,
       main="Minimum Distance from Provider to Any Offender",
       xlab="Distance[feet]", ylab="Number of Providers")
  mtext(DATASET.NAME, col="blue")
  hist(min.dist.from.provider[min.dist.from.provider <= 1.5*THRESHOLD], breaks=50,
       main="Minimum Distance from Provider to Any Offender",
       xlab="Distance[feet]", ylab="Number of Providers")
  mtext(paste(DATASET.NAME, ", THRESHOLD=", THRESHOLD, sep=""), col="blue")
  abline(v=THRESHOLD, col="blue")

  offenders.too.close <- apply(m, ROW<-1, function(x) sum(x<=THRESHOLD))

  cat("minimum distance from provider to any offender\n")
  cat(sum(min.dist.from.provider <= 5280), "<= 5280 feet\n")   # 1.0 mile
  cat(sum(min.dist.from.provider <= 2640), "<= 2640 feet\n")   # 0.5 mile
  cat(sum(min.dist.from.provider <= 2000), "<= 2000 feet\n")
  cat(sum(min.dist.from.provider <= 1000), "<= 1000 feet\n")
  cat(sum(min.dist.from.provider <=  500), "<=  500 feet\n")
  cat(sum(min.dist.from.provider <=  250), "<=  250 feet\n")
  cat(sum(min.dist.from.provider <=  100), "<=  100 feet\n")
  cat(sum(min.dist.from.provider <=   50), "<=   50 feet\n")
  cat(sum(min.dist.from.provider <=    0), "<=    0 feet\n")

  PROVIDER.DATA$min.dist.feet.to.offender <- min.dist.from.provider
  PROVIDER.DATA$offenders.too.close       <- offenders.too.close

  column.order <-
     c("min.dist.feet.to.offender",
       "offenders.too.close",
       "fuzzy",
       "location.type",
       "result.count",
       "FACILITYNAME",
       "STREET",
       "CITY",
       "STATE",
       "ZIP",
       "status",
       "state",
       "zip",
       "county",
       "lat",
       "lng",
       "formatted.address")

  PROVIDER.DATA <- PROVIDER.DATA[,column.order]

  write.table(PROVIDER.DATA,
              file=paste(DATASET.NAME, "-min-distance-from-provider-to-any-offender.txt", sep=""),
              sep="\t", row.names=FALSE)

  ###############################################################################
  # How many offenders are closer than given distance to provider

  min.dist.from.offender <- apply(m, COLUMN<-2, min)
  length(min.dist.from.offender)

  hist(min.dist.from.offender, breaks=50,
       main="Minimum Distance from Offender to Any Provider",
       xlab="Distance[feet]", ylab="Number of Offenders")
  mtext(DATASET.NAME, col="blue")
  hist(min.dist.from.offender[min.dist.from.offender <= 1.5*THRESHOLD], breaks=50,
       main="Minimum Distance from Offender to Any Provider",
       xlab="Distance[feet]", ylab="Number of Offenders")
  mtext(DATASET.NAME, col="blue")
  abline(v=THRESHOLD, col="blue")

  providers.too.close <- apply(m, COLUMN<-2, function(x) sum(x<=THRESHOLD))

  cat("minimum distance from offender to any provider\n")
  cat(sum(min.dist.from.offender <= 5280), "<= 5280 feet\n")   # 1.0 mile
  cat(sum(min.dist.from.offender <= 2640), "<= 2640 feet\n")   # 0.5 mile
  cat(sum(min.dist.from.offender <= 2000), "<= 2000 feet\n")
  cat(sum(min.dist.from.offender <= 1000), "<= 1000 feet\n")
  cat(sum(min.dist.from.offender <=  500), "<=  500 feet\n")
  cat(sum(min.dist.from.offender <=  250), "<=  250 feet\n")
  cat(sum(min.dist.from.offender <=  100), "<=  100 feet\n")
  cat(sum(min.dist.from.offender <=   50), "<=   50 feet\n")
  cat(sum(min.dist.from.offender <=    0), "<=    0 feet\n")

  OFFENDER.DATA$min.dist.feet.to.provider <- min.dist.from.offender
  OFFENDER.DATA$providers.too.close       <- providers.too.close

  column.order <- c(
       "min.dist.feet.to.provider",
       "providers.too.close",
       "fuzzy",
       "location.type",
       "result.count",
       "N",
       "Name",
       "Address",
       "City",
       "St",
       "Zip",
       "County",
       "Compliant",
       "status",
       "zip",
       "county",
       "state",
       "lat",
       "lng",
       "formatted.address")

  # Rearrangment for easier display in Excel
  OFFENDER.DATA <- OFFENDER.DATA[,column.order]

  write.table(OFFENDER.DATA, file=paste(DATASET.NAME, "-min-distance-from-offender-to-any-provider.txt",sep=""),
              sep="\t", row.names=FALSE)

  ###############################################################################

  ConnectionsFile <- file(paste(DATASET.NAME, "-provider-offender-connections-threshold-", THRESHOLD, "ft.txt", sep=""), "w")

  for (i in 1:nrow(PROVIDER.DATA))
  {
    if (PROVIDER.DATA$min.dist.feet.to.offender[i] <= THRESHOLD)
    {

      min.offender.distance <- ifelse(PROVIDER.DATA$offenders.too.close[i] == 1,
                                      paste("Distance to offender:", PROVIDER.DATA$min.dist.feet.to.offender[i], "ft."),
                                      paste("Min to", PROVIDER.DATA$offenders.too.close[i], "offenders:",
                                            PROVIDER.DATA$min.dist.feet.to.offender[i], "ft.") )

      provider.info <- paste("Provider ",
                             i+1,                                   " ",     # Index for Excel
                             PROVIDER.DATA$location.type[i],        "(",
                             PROVIDER.DATA$result.count[i],         "). ",
                             min.offender.distance,                 " ",
                             PROVIDER.DATA$FACILITYNAME[i],         ", ",
                             PROVIDER.DATA$STREET[i],               ", ",
                             PROVIDER.DATA$CITY[i],                 ", ",
                             PROVIDER.DATA$STATE[i],                " ",
                             PROVIDER.DATA$ZIP[i],                  ", ",
                             PROVIDER.DATA$county[i],               ", ",
                             PROVIDER.DATA$lat[i],                  ", ",
                             PROVIDER.DATA$lng[i],
                             sep="")
      cat("\n", provider.info, "\n", file=ConnectionsFile)

      for (j in 1:ncol(m))
      {
        if (m[i,j] <= THRESHOLD)
        {
          min.provider.distance <- paste("Distance to provider:", m[i,j], "ft.")

          if (OFFENDER.DATA$providers.too.close[j] > 1)
          {
             if (OFFENDER.DATA$min.dist.feet.to.provider[j] < m[i,j])
             {
               min.provider.distance <- paste(min.provider.distance,
                                           "Min to ",OFFENDER.DATA$providers.too.close[j],
                                           "providers = ", OFFENDER.DATA$min.dist.feet.to.provider[j], "ft.")
             } else {
               min.provider.distance <- paste(min.provider.distance,
                                           "Min to ",OFFENDER.DATA$providers.too.close[j],
                                           "providers.")
             }
          }

          offender.info <- paste("Offender ",
                                 j+1,                                        " ",  # Index for Excel
                                 OFFENDER.DATA$location.type[j],             "(",
                                 OFFENDER.DATA$result.count[j],              "). ",
                                 min.provider.distance,                      " ",
                                 OFFENDER.DATA$Name[j],                         ", ",
                                 OFFENDER.DATA$Address[j],                   " ",
                                 OFFENDER.DATA$City[j],                      ", ",
                                 OFFENDER.DATA$St[j],                        " ",
                                 OFFENDER.DATA$Zip[j],                       ", ",
                                 OFFENDER.DATA$county[j],                    ", ",
                                 OFFENDER.DATA$lat[j],                       ", ",
                                 OFFENDER.DATA$lng[j],                       ", ",
                                 sep="")
          cat("     ", offender.info, "\n", file=ConnectionsFile)
        }
      }
    }
  }

  close(ConnectionsFile)

  dev.off()
}


THRESHOLD <- 1000 # feet between offender and provider

compute.distance.matrix("ChurchDaycare",    offender.data, religious.data,         THRESHOLD)
