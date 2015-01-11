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


## -------------------------------------------------------
#Start plotting: 

# Plot1:
# Process the missing data:
plotdata$Global_active_power[plotdata$Global_active_power=="?"] <- NA
Global_active_power <- as.numeric(as.character(plotdata$Global_active_power))

# Save to file
png("plot1.png" ,width = 480, height = 480) 
# Plot histogram
hist(Global_active_power,  xlab = "Global Active Power (kilowatts)", 
     ylab = "Frequency", main = "Global Active Power", 
     xlim = c(0, 6), ylim = c(0, 1200),col = "red", breaks = 12)

# clean up
dev.off()






