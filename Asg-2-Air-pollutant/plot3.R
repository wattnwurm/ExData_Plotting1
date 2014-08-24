## Assignment 2 - Plot Question 3---------------
## Exploratory Data Analysis - Course Project ----

## Question --------------------
# Of the four types of sources indicated by the  type  (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? 
# Use the ggplot2 plotting system to make a plot answer this question.
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
NEI[seq(1,1000,20), ] # look only at every 20th row
tail(NEI)
names(NEI)
# are there any NA's?
sum(is.na(NEI$fips))
sum(is.na(NEI$Emissions))


# reduce the data
# subsetting on Baltimore == "24510": 
balt <- NEI[NEI$fips == "24510" , c(6, 4, 5, 1)]

head(balt)
str(balt)
balt$year  <- as.factor(balt$year)
balt$type  <- as.factor(balt$type)
#
library(plyr)
indi <- ddply(balt, .(year, type), summarise, sum(Emissions))
cnames <- names(indi)
cnames[3] <- "Emissions"
cnames
class(indi)
names(indi) <- cnames
indi

library(ggplot2)
# making plots -------------
png("plot3.png", width= 600, height=600)
ggplot(indi, aes(x=factor(year), y=Emissions, fill=type)) +
        geom_bar(stat = "identity", position="dodge", colour="black") +
        scale_fill_brewer(palette="Pastel1") +
        facet_grid(. ~ type) +
        labs(x=expression(PM[2.5]*" emissions of incated sources in Baltimore City, Maryland")) +
        labs(y="emissions in tons")


dev.off() # don't forget to close!!
