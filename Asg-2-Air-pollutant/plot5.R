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

# subsetting on Baltimore == "24510": 
balt <- NEI[NEI$fips == "24510" , ]
head(balt)
str(balt)

#-------------------------
str(SCC)
names(SCC)
head(SCC[, 1:3])
head(SCC[, 4:7])
head(SCC[, 8:11])
head(SCC[, 12:15])

library(plyr)
########################################
# looking for motor vehicles sources ----
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

# 5. view
mv <- grep("*[Mm]otor [Vv]ehicle*", SCC$Short.Name) 
length(mv)

#-----------------------------------
# subsetting with different views
p0 <- balt[pm0,  ]
p1 <- balt[pm1,  ]
m  <- balt[motor,  ]
v  <- balt[veh,  ]
n  <- balt[mv, ]


# putting them all together
p <- rbind(p0,p1, m, v, n)

# delete double rows
mvs <- unique(p)
head(mvs)
str(mvs)

mvs$year  <- as.factor(mvs$year)
mvs$type  <- as.factor(mvs$type)
ddply(mvs, .(year), summarise, sum(Emissions))
# there are no cases in 1999
# delete cases with NA
mvs <- na.omit(mvs)

mot.veh.source <- ddply(mvs, .(year), summarise, sum(Emissions))
cnames <- names(mot.veh.source)
cnames
cnames[2] <- "Emissions"
cnames
names(mot.veh.source) <- cnames
mot.veh.source

library(ggplot2)
# making plots -------------
png("plot5.png", width= 480, height=480)

ggplot(mot.veh.source , aes(year, Emissions)) +
        geom_bar(position="dodge", colour="black", stat="identity") +
        scale_fill_brewer(palette="Pastel1") +
        labs(title = "Changing of emissions from motor vehicle sources in Baltimore \n (in 1999 no data available)") +
        labs(y =expression(PM[2.5]*" overall emissions in tons")) +
        labs(x ="Year")  


dev.off() # don't forget to close!!
