
basedir <- "C:/Franklin/Projects/2011/KSHB-Sex-Offenders/Geocoding-Missouri-Sex-Offenders/"
source(basedir)

source("GoogleGeocode.R")

d <- get.geocode("IDNumber", "5007 S Fuller Ave", "Independence", "MO", "")
d


d <- get.geocode("IDNumber", "5007 S Fuller Ave", "Independence", "MO", "64055")
t(d)
