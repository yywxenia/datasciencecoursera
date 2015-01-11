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

## -------------------------------------------------------
##Plot 4:
# Process the missing data:
#>>> subplot1: 
#   Global_active_power:
plotdata$Global_active_power[plotdata$Global_active_power=="?"] <- NA
Global_active_power <- as.numeric(as.character(plotdata$Global_active_power))

#>>> subplot2:
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

#>>> subplot3: 
#   Voltage:
plotdata$Voltage[plotdata$Voltage=="?"] <- NA
Voltage <- as.numeric(as.character(plotdata$Voltage))

#>>> subplot4: 
#   Global_reactive_power:
plotdata$Global_reactive_power[plotdata$Global_reactive_power=="?"] <- NA
Global_reactive_power <- as.numeric(as.character(plotdata$Global_reactive_power))

#----------------------------
# Save to file
png("plot4.png" ,width = 480, height = 480) 

#----------------------------
# Plot plot4
# subplots grid
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

#----------------------------
# subplot1:
plot (RDatetime2Plot, Global_active_power,type="l", xlab="", ylab="Global Active Power (kilowatts)")

# subplot2:
plot (RDatetime2Plot, Voltage, type="l", xlab="datetime", ylab="Voltage")

# subplot3:
plot (RDatetime2Plot, Y1,type="n", xlab="", ylab="Energy Sub Metering")
lines(RDatetime2Plot, Y1, col="black")
lines(RDatetime2Plot, Y2, col="red")
lines(RDatetime2Plot, Y3, col="blue")
legend("topright",lty=c(1,1,1), col = c("black", "red","blue"), 
       legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"))

# subplot4:
plot (RDatetime2Plot, Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")

# clean up
dev.off()
rm(list=ls())
