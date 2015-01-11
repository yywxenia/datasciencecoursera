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
## For Plot 2
# Process the missing data:
plotdata$Global_active_power[plotdata$Global_active_power=="?"] <- NA
Global_active_power <- as.numeric(as.character(plotdata$Global_active_power))
# Save to file
png("plot2.png" ,width = 480, height = 480) 
# Plot plot2
plot (RDatetime2Plot, Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")

# clean up
dev.off()
rm(list=ls())
