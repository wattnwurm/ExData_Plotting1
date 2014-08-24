## Assignment 2 - Plot Question 4---------------
## Exploratory Data Analysis - Course Project ----

## Question --------------------
# Across the United States, how have emissions from 
# coal combustion-related sources changed from 1999–2008?
#----------------------------------
#---  preparatory work------
rm(list=ls()) # clear environment, delete all
setwd("D:\\008d-exdata\\projects\\asg-2")
# ++++++++++++++ obtaining the datas +++++++++++++++++++++++++++++
# data are unzipped in the directory, 
dir()
## reading will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# look at the data ---
str(NEI)
head(NEI)
tail(NEI)
NEI[seq(1,10000,50), ] # overview, only every 50th row

# are there any NA's?
sum(is.na(NEI$fips))
sum(is.na(NEI$Emissions))

str(SCC)
# look at the data separated
head(SCC[, 1:3])
head(SCC[, 4:7])
head(SCC[, 8:11])
head(SCC[, 12:15])
SCC[seq(1,1000,50), 1:3 ] # overview, only every 50th row

# what classes are the columns?
cs <- function(x)class(x)
sapply(NEI, cs)
sapply(SCC, cs)

# reduce the data
# subsetting on "Coal": 
#------------------------------------
coal <- grep('*[cC]oal*',SCC$Short.Name)
SCC.Coal <- SCC[coal,]
# subsetting on "Combustion": 
r <- grep('*[cC]omb*',SCC.Coal$Short.Name)
data.CC <- SCC.Coal[r,]
CC.Codes <-data.CC[,1]
coal.comb <- subset(NEI,NEI$SCC %in% CC.Codes)
head(coal.comb)
str(coal.comb)

coal.comb$type  <- as.factor(coal.comb$type)
coal.comb$year  <- as.factor(coal.comb$year)


library(plyr)
emi <- ddply(coal.comb, .(year), summarise, sum(Emissions))
head(emi)
cnames <- names(emi)
cnames[2] <- "Emissions"
cnames
names(emi) <- cnames
emi

library(ggplot2)
# making plots -------------
png("plot4.png", width= 480, height=480)

ggplot(emi , aes(year, Emissions)) +
        geom_bar(position="dodge", colour="black", stat="identity") +
        scale_fill_brewer(palette="Pastel1") +
        labs(title = "Changing of emissions from coal combustion related sources \n across the USA") +
        labs(y =expression(PM[2.5]*" overall emissions in tons")) +
        labs(x ="Year")  


dev.off() # don't forget to close!!
