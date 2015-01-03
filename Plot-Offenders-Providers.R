# Plot Missouri sexual offenders / child care providers.
# Analysis for Ryan Kath, KSHB NBC Action News

# Earl F Glynn, Franklin Center for Government & Public Integrity
# 12 July 2011

library(maps)

basedir <- "C:/Franklin/Projects/2011/KSHB-Sex-Offenders/"

setwd(basedir)

STATE <- "Missouri"

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

child.care.center.data <- Process.Dataset("geocode-MO-child-care-center-out.txt",
                                          "bad-data-MO-child-care-center-provider.txt")

religious.data <- Process.Dataset("geocode-ReligiousOrganizations-out.txt",
                                  "bad-data-ReligiousOrganizations-provider.txt")

nursery.schools.data <- Process.Dataset("geocode-NurserySchools-out.txt",
                                        "bad-data-NurserySchools-provider.txt")

###############################################################################
### Maps by Dataset

plot.missouri.map <- function(AREA, TITLE.PREFIX)
{
  # Offenders
  map("county", AREA, col="grey")
  mtext(paste(TITLE.PREFIX,"Sexual Offenders [June 20, 2011]"), adj=0, col="red")
  points(offender.data$lng, offender.data$lat,
    col=ifelse(offender.data$fuzzy, "palevioletred", "red"))

  # Child Care Centers
  map("county", AREA, col="grey")
  mtext(paste(TITLE.PREFIX, "Child Care Centers [June 28, 2011]"), adj=0, col="blue")
  points(child.care.center.data$lng, child.care.center.data$lat, pch=1,  # circle
    col=ifelse(child.care.center.data$fuzzy, "royalblue", "blue"))

  # Child Care by Religious Organizations
  map("county", AREA, col="grey")
  mtext(paste(TITLE.PREFIX, "Child Care by Religious Organizations [June 14, 2011]"), adj=0, col="blue")
  points(religious.data$lng, religious.data$lat, pch=5,  # diamond
    col=ifelse(religious.data$fuzzy, "royalblue", "blue"))

  # Nursery Schools
  map("county", AREA, col="grey")
  mtext(paste(TITLE.PREFIX, "Nursery Schools [June 14, 2011]"), adj=0, col="blue")
  points(nursery.schools.data$lng, nursery.schools.data$lat, pch=2,  # triangle
    col=ifelse(nursery.schools.data$fuzzy, "royalblue", "blue"))

  # Offenders and Child Care Facilities
  map("county", AREA, col="grey")
  mtext(paste(TITLE.PREFIX, "Sexual Offenders"), adj=0, col="red")
  mtext("Child Care Facilities", adj=0.7, col="blue")
  points(offender.data$lng, offender.data$lat,
    col=ifelse(offender.data$fuzzy, "palevioletred", "red"))
  points(child.care.center.data$lng, child.care.center.data$lat, pch=1,  # circle
    col=ifelse(child.care.center.data$fuzzy, "royalblue", "blue"))
  points(religious.data$lng, religious.data$lat, pch=5,  # diamond
    col=ifelse(religious.data$fuzzy, "royalblue", "blue"))

  # Kludge: This only works on AREA=STATE map now
  legend(-94.5, 36.4, c("Child Care Center", "Religious Organization", "Nursery School"),
         col="blue", pch=c(1,5,2), horiz=TRUE, bty="n", cex=0.75, text.col="blue")

  # Latitude-Longitude
  map("county", AREA, col="grey")
  mtext(paste(TITLE.PREFIX, "Latitude-Longitude Grid"), adj=0)
  grid(NULL, col="gray20")
  axis(1)
  axis(2)
}

# Statewide
pdf("MO-Offenders-Child-Care-Providers-Maps-Statewide.pdf", width=8, height=10)
  plot.missouri.map(STATE, "Missouri")
dev.off()

# KC Metro - Misouri Side
# Kansas City Missouri-Side Metro Area

#kc.metro.missouri.side <- c("missouri,bates",     "missouri,caldwell", "missouri,cass",
#                            "missouri,clay",      "missouri,clinton",  "missouri,jackson",
#                            "missouri,lafayette", "missouri,platte",   "missouri,ray")

kc.metro.missouri.side <- c("missouri,cass","missouri,clay", "missouri,jackson", "missouri,platte")

pdf("MO-Offenders-Child-Care-Providers-Maps-KC-Metro.pdf", width=8, height=10)
  plot.missouri.map(kc.metro.missouri.side, "KC Metro (MO side)")
dev.off()
