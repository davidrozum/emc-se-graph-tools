# File-Name:        dots-by-zip.r
# Date:             2015-05-12
# Author:           David Rozum
# Purpose:          Produce a map of the installed base locations by zip code. Tested on USA only, but should be portable
# Data Used:        Configuration file: dots-by-zip_cfg.csv - defines input/output file names
#                   CSV file of zip codes with the zip code column named "zip"
#                   The Shapefile for the geography desired (USA is provided) 
# Packages Used:    maptools
# Output File:      .png with map of the data center locations.
#
# Note:             The SYR output files do not have the zip code in them. But zip code processing yields a more accurate
#                   map. Therefore, do some preprocessing and get a customer file with zip codes in it to use this script.
#                   NOTE: the demo file cheats and includes system counts in the "Installed" column.
#
#                   Also, this is more of a quick and dirty effort. dots-on-a-map.r has better commenting.
#

library(maptools)

#read the configuration file - assumes it's in the current directory. Force it to be a matrix instead of a data frame
#a matrix lets me use a config[row-label,column-label] convention to access elements
#-------------------
config <- as.matrix(read.csv("dots-by-zip_cfg.csv", header=TRUE, row.names=1))
cat("Using the following configuration:\n") #show 'em what we got
print(config)

#read the zip code file and the customer data file, then merge them (inner join)
#-------------------
cat("\nReading zip code and customer data\n")
zip_codes <- read.csv(config["Zip Code File","value"],header=TRUE)
cust_zips <- read.csv(config["Customer File","value"], header=TRUE)
data <- merge(cust_zips,zip_codes,by="zip")
maxer <- max(data$Installed)
data$size <- 0.5 + data$Installed/maxer *1.5       #size the dot between 0.5 and 2


cat("Reading shapefile\n")  #Shapefiles are an open spec developed by ESRI. This one comes from census.gov
data.shp <- readShapePoly(config["Shapefile","value"], proj4string=CRS("+proj=longlat"))


#create a png device (file) for output. Could do a jpg, tiff, whatever
#-------------------
cat("Generating plot\n")
png(config["Output Image","value"],width=1200,height=750,res=100)
# print US state map, limit to continental US (x is longitude, y is latitude). The shapefile has AK, HI, marshall islands...
#-------------------
plot(data.shp, xlim=c(-125,-65), ylim=c(25,51), main=config["Title","value"], sub=config["Sub","value"])
points(data$long, data$lat, col="blue", cex=data$size, pch=8)   #data center locations
dev.off()  #close the file
cat("Done! Output file is ", config["Output Image","value"], "\n") # "\n" means linefeed (newline) in C (and R)
