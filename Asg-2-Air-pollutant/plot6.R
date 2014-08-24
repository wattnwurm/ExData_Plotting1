## Assignment 2 - Plot Question 5---------------
## Exploratory Data Analysis - Course Project ----

## Question --------------------
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
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
names(NEI)
# are there any NA's?
sum(is.na(NEI$fips))
sum(is.na(NEI$Emissions))

str(SCC)
names(SCC)
head(SCC[, 1:3])
head(SCC[, 4:7])
head(SCC[, 8:11])
head(SCC[, 12:15])

# subsetting on Baltimore and Los Angeles: 
la <- NEI[NEI$fips == "06037", ]
str(la)
la$type  <- as.factor(la$type)
la$year  <- as.factor(la$year)

nrow(la[la$Emissions > 100, ])

head(la)
b.l <- NEI[NEI$fips == "24510" | NEI$fips == "06037", ]

library(plyr)
########################################
# looking for motor vehicles sources in the Source_Classification_Code----
# 1. view
pm0 <- grep("*[Mm]obile*", SCC$SCC.Level.One) 
length(pm0)  # number of matches

# 2. view
pm1 <- grep("*[Mm]obile*", SCC$EI.Sector) 
length(pm1)  # number of matches

# 3. view
motor <- grep("*[Mm]otor*", SCC$Short.Name) 
length(motor)

# 4. view
veh <- grep("*[Vv]ehicle*", SCC$Short.Name) 
length(veh)

#-----------------------------------
# subsetting with different views
p0 <- b.l[pm0,  ]
p0

p1 <- b.l[pm1,  ]
p1
m  <- b.l[motor,  ]
v  <- b.l[veh,  ]
 

# putting them all together
p <- rbind(p0,p1, m , v)

# delete double rows
mvs <- unique(p)
head(mvs)
str(mvs)

mvs$year  <- as.factor(mvs$year)
mvs$type  <- as.factor(mvs$type)
ddply(mvs, .(year, fips), summarise, sum(Emissions))
# there are no cases in 1999
# delete cases with NA
mvs <- na.omit(mvs)

mot.veh.source <- ddply(mvs, .(year, fips), summarise, Emissions=sum(Emissions))
mot.veh.source


library(ggplot2)
# making plots -------------
png("plot6.png", width= 600, height=600)

ggplot(mot.veh.source, aes(x=factor(year), y=log10(Emissions), fill=fips)) +
        geom_bar(stat = "identity", position="dodge", colour="black") +
        scale_fill_brewer(palette="Pastel1") +
        facet_grid(. ~ fips) +
        levels(mot.veh.source$fips <- c("Los Angeles", "Baltimore")) +
        labs(title = "Changing of emissions from motor vehicle sources \n in Baltimore and Los Angeles") +
        labs(y =expression(PM[2.5]*" overall emissions in tons (log10)")) +
        labs(x ="Year")  


dev.off() # don't forget to close!!

