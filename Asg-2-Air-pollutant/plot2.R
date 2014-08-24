## Assignment 2 - Plot Question 2---------------
## Exploratory Data Analysis - Course Project ----

## Question --------------------
# Have total emissions from PM2.5  decreased in the Baltimore City, Maryland ( fips == "24510" ) from
# 1999 to 2008? Use the base plotting system to make a plot answering this question.
#----------------------------------
#---  preparatory work------
rm(list=ls()) # clear environment, delete all
setwd("D:\\008d-exdata\\projects\\asg-2")
# ++++++++++++++ obtaining the datas +++++++++++++++++++++++++++++
# data are unzipped in the directory, 
dir()
## reading will likely take a few seconds. 
NEI <- readRDS("summarySCC_PM25.rds")
# look at the data ---
str(NEI)
head(NEI)
tail(NEI)
names(NEI)
# are there any NA's?
sum(is.na(NEI$fips))
sum(is.na(NEI$Emissions))


# reduce the data
# subsetting on Baltimore == "24510": 
bm <- NEI[NEI$fips == "24510" , c(6, 4, 1)]

head(bm)
bm$year  <- as.factor(bm$year)

# only needed years --------------------
y1999 <- subset(bm,bm$year== 1999)
y2008 <- subset(bm,bm$year== 2008)
head(y2008)
sum1999 <- sum(y1999$Emissions)
sum2008 <- sum(y2008$Emissions)

# Plot 1 --------------
years <- c(1999, 2008)
emis <- c(sum1999, sum2008)
total  <- sum(sum1999, sum2008)

#- new dataframe
q1 <- data.frame(years, emis)
q1
# making plots -------------
png("plot2.png", width= 512, height=384)
par(mfrow=c(1,2)) # double panel
# absolut ---
barplot(q1$emis, names.arg= q1$year, 
        ylab="overall emissions in tons")

# prop --------------------------------------
emis.prob <- c(sum1999/sum1999, sum2008/sum1999)
q1.prob <- data.frame(years, emis.prob)
q1.prob


barplot(q1.prob$emis.prob, names.arg= q1.prob$year, 
        ylab="overall emissions related to 1999")


# switch back ---------
par(mfrow=c(1,1))
title("Total emissions of PM2.5 in Baltimore City, Maryland")

dev.off() # don't forget to close!!


