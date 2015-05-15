# File-Name:        dots-on-a-map.r
# Date:             2015-05-12
# Author:           David Rozum
# Purpose:          Using EMC SYR data, produce a map of the installed base locations. Tested on USA only, but should be portable
# Data Used:        Configuration file: dots_cfg.csv - defines input/output file names
#                   Customer SYR output file as a .csv
#                   The Shapefile for the geography desired (USA is provided) 
# Packages Used:    maptools
# Output File:      .png with map of the data center locations.
#
# Note:             The SYR output files do not have the zip code in them. Therefore we take the first instance of the
#                   City+State lattitude and longitude. For large cities, this means the plotted position won't be exact.
#                   Won't be exact for most cities, actually, since most have multiple zip codes.
#

library(maptools)

# some initial values with the WGS84 coordinates for the continental US
minX <- -125.0 #longitude
maxX <- -65.0 
minY <- 24.0 #latitude
maxY <- 51.0

#read the configuration file - assumes it's in the current directory. Force it to be a matrix instead of a data frame
#a matrix lets me use a config[row-label,column-label] convention to access elements
#----------
config <- as.matrix(read.csv("dots_cfg.csv", header=TRUE, row.names=1))
cat("Using the following configuration:\n") #show 'em what we got
print(config)

#read the zip code file and the customer data file, then do some clean up
#----------
cat("\nReading zip code and customer data\n")
zip.codes <- read.csv(config["Zip Code File","value"],header=TRUE)
zip.codes$CityState <- paste(toupper(zip.codes$City),toupper(zip.codes$state))  #create a 'CityState' column
cust.syr <- read.csv(config["Customer File","value"], header=TRUE)
cust.syr <- cust.syr[cust.syr$Category != "ESRS",]                   #get rid of ESRS entries
cust.syr$CityState <- paste(toupper(cust.syr$City),toupper(cust.syr$State))      #create a 'CityState' column here too
cust.syr$CityState <- factor(cust.syr$CityState)
cust.tab <- as.data.frame(table(cust.syr$CityState,dnn=c("CityState")))  #create a dataframe with CityState & # of systems

# merge (natural join) the zip.codes and customer data frames
#----------
data <- merge(cust.tab,zip.codes,by="CityState")    #would be easier with zip codes
data <- data[!duplicated(data$CityState),]    #just take first CityState entry
maxer <- max(data$Freq)
data$size <- 0.5 + data$Freq/maxer *1.5       #size the dot between 0.5 and 2 based on # systems/CityState
###write.csv(data,file="ofil.csv")      #if you want the results in a CSV, uncomment this line


cat("Reading shapefile\n")  #Shapefiles are an open spec developed by ESRI. This one comes from census.gov
data.shp <- readShapePoly(config["Shapefile","value"], proj4string=CRS("+proj=longlat"))

# do some lat/long sanity testing
#----------
if( min(data$long) < minX ){
   minX <- min(data$long) - 10.0  #add some border (but not testing for offscale)
} 
if( max(data$long) > maxX ){
   maxX <- max(data$long) + 10.0
} 
if( min(data$lat) < minY ){
   minY <- min(data$lat) - 10.0
} 
if( max(data$lat) > maxY ){
   maxY <- max(data$lat) + 10.0
} 
cat("Longs:",minX,maxX,"Lats:",minY,maxY,"\n") #where did we end up

#create a png device (file) for output. Could do a jpg, tiff, whatever
#----------
cat("Generating plot\n")
png(config["Output Image","value"],width=1200,height=750,res=100)

# print US state map, limit to continental US (x is longitude, y is latitude). The shapefile has AK, HI, marshall islands...
#----------
plot(data.shp, xlim=c(minX,maxX), ylim=c(minY,maxY), main=config["Title","value"], sub=config["Sub","value"])
points(data$long, data$lat, col="red", cex=data$size, pch=8)   #data center locations
dev.off()  #close the file
cat("Done! Output file is ", config["Output Image","value"], "\n")
