## Assignment 1 - Plot 3
## Exploratory Data Analysis - Course Project ----
## Our overall goal here is simply to examine how household energy usage 
## varies over a 2-day period in February, 2007.


#---  preparatory work------
rm(list=ls()) # clear environment, delete all

# setting the working directory, change it for your needs
setwd("D:\\008d-exdata\\projects\\asg-1")
# ++++++++++++++ obtaining the datas +++++++++++++++++++++++++++++
# data are unzipped in the directory, 
filename = "household_power_consumption.txt"
hpc <- read.csv("household_power_consumption.txt",  header = TRUE, sep = ";",
                na.strings = "?", colClasses = "character")

hpc$Date <- as.Date(hpc$Date,"%d/%m/%Y")
head(hpc)
# subsets of the days: 
part1 <- subset(hpc,hpc$Date=="2007-02-01")
part2 <- subset(hpc,hpc$Date=="2007-02-02")

#Join the data into one data frame "hpc2", use this as source for all plots
hpc2 <- rbind(part1,part2) #: This data frame will have data for those 2 dates only. 
# look at the data: start at 2007-02-01, 00:00:00
head(hpc2)
# look at the data: ends at 2007-02-02, 23:59:00
tail(hpc2)
# change date and time to POSIX-class
hpc2$Date <- strptime(paste(hpc2$Date,hpc2$Time), "%Y-%m-%d %H:%M:%S")
# change back to numeric
hpc2$Global_active_power  <- as.numeric(hpc2$Global_active_power)
hpc2$Global_reactive_power  <- as.numeric(hpc2$Global_reactive_power)
hpc2$Voltage  <- as.numeric(hpc2$Voltage)
hpc2$Global_intensity  <- as.numeric(hpc2$Global_intensity)
hpc2$Sub_metering_1  <- as.numeric(hpc2$Sub_metering_1)
hpc2$Sub_metering_2  <- as.numeric(hpc2$Sub_metering_2)
hpc2$Sub_metering_3  <- as.numeric(hpc2$Sub_metering_3)

# what classes are the columns? all ok?
f1 <- function(x)class(x)
sapply(hpc2, f1)

# overview, are there any NAs?
summary(hpc2)


#---plot 3-------
par(mfrow=c(1,1)) # single panel
Sys.setlocale("LC_TIME", "USA") # Windows, setting x-label
plot(hpc2$Date, hpc2$Sub_metering_1, type="l",  xlab="",ylab="Energy sub metering")
lines(hpc2$Date, hpc2$Sub_metering_2, col="red")
lines(hpc2$Date, hpc2$Sub_metering_3, col="blue")
legend("topright",horiz=F,
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       col=c("black","red","blue"),lty=1)

# copy plot to png-file
dev.copy(png, file = "plot3.png", width=480, height=480)
dev.off() # don't forget to close!!

