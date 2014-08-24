## Assignment 2 - Plot Question 1---------------
## Exploratory Data Analysis - Course Project ----

## Question --------------------
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM  emission from 
# all sources for each of the years 1999, 2002, 2005, and 2008.
#----------------------------------
#---  preparatory work------
rm(list=ls()) # clear environment, delete all
setwd("D:\\008d-exdata\\projects\\asg-2")
# ++++++++++++++ obtaining the datas +++++++++++++++++++++++++++++
# data are unzipped in the directory, 
dir()
## reading will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")

# look at the data ---
str(NEI)
head(NEI)
# look at every 10th row
NEI[seq(1,500,10), ]
tail(NEI)
names(NEI)
# are there any NA's?
sum(is.na(NEI$Emissions))
sum(is.na(NEI$Pollutant))

# what classes are the columns?
f1 <- function(x)class(x)
sapply(NEI, f1)

# reduce the data - only year and emissions
totem <- NEI[ , c(6, 4)]
str(totem)
head(totem)
totem$year  <- as.factor(totem$year)

# emissions per year
epy <- with(totem, tapply(Emissions, year, sum, na.rm=T))
str(epy)
summary(epy)
cnames <- names(epy)
cnames
q0 <- data.frame(cnames, epy)
q0

# making plot -------------
png("plot1.png", width= 512, height=384)
# Plot 1 --------------
barplot(q0$epy/1000, names.arg= cnames, 
        main="Total emissions of PM2.5 in the United States ",
        ylab="overall emissions in 1.000 tons")

#--------------------------
dev.off() # don't forget to close!!

