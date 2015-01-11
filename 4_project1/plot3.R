## Author: Yiwei Yan
## -------------------------------------------------------
# Remove all current data in workspace:
rm(list=ls())
# Go to the directory where the data file exits
setwd("C:/Users/elva.yan/Desktop/Coursera/Exploratory data analysis/exdata-data-household_power_consumption")

## -------------------------------------------------------
## Get the required subset of data:
# Read the file
rawdata<- read.table("household_power_consumption.txt", sep = ";", header = T)
# Convert date
rawdata$RDate <- as.Date(rawdata$Date,"%d/%m/%Y")
# Subsetting
plotdata <- subset(rawdata, RDate == "2007-02-01" | RDate == "2007-02-02")
## --------------------------------------------------------
## Process the subtable (mainly the datetime)
# Get the datetime for plotting:
RDatetime2Plot <- strptime(paste(plotdata$Date, plotdata$Time), "%d/%m/%Y %H:%M:%S")

## --------------------------------------------------------
## For Plot 3
# Process the missing data:
#   Sub_metering_1
currentSubMetering <-as.character(plotdata$Sub_metering_1)
currentSubMetering[currentSubMetering=="?"] <-NA
Y1 <- as.numeric(as.character(currentSubMetering))
#   Sub_metering_2
currentSubMetering <-as.character(plotdata$Sub_metering_2)
currentSubMetering[currentSubMetering=="?"] <-NA
Y2 <- as.numeric(as.character(currentSubMetering))
#   Sub_metering_3
currentSubMetering <-as.character(plotdata$Sub_metering_3)
currentSubMetering[currentSubMetering=="?"] <-NA
Y3 <- as.numeric(as.character(currentSubMetering))

# Save to file
png("plot3.png" ,width = 480, height = 480) 
# Plot plot3
plot (RDatetime2Plot, Y1,type="n", xlab="", ylab="Energy Sub Metering")
lines(RDatetime2Plot, Y1, col="black")
lines(RDatetime2Plot, Y2, col="red")
lines(RDatetime2Plot, Y3, col="blue")
legend("topright",lty=c(1,1,1), col = c("black", "red","blue"),legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"))

# clean up
dev.off()
rm(list=ls())
