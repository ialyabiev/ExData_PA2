
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

## Questions

## -- 1 -- 
## Have total emissions from PM2.5 decreased in the United States 
## from 1999 to 2008? 
## Using the base plotting system, make a plot showing the total PM2.5
## emission from all sources for each of the years 1999, 2002, 2005, and 2008.

par(optim)# mar = rep(5, 4))

ct <- xtabs(Emissions ~ year, data = NEI)
data <- round(ct/1e+06,1)

#plot
barplot(data,axes = T, col="palegreen", ylim = c(0,9),
    xlab = expression(bold("Year")),
    ylab = expression(bold("PM ") [2.5]*"   ( in million tons )"), 
    main = "Total emissions")
text(seq(0.7, 1.2*4, by=1.2), data, labels=as.character(data), pos = 3)

plot(data, col="palegreen", ylim = c(0,9),type = "b",
        xlab = expression(bold("Year")),
        ylab = expression(bold("PM ") [2.5]*"   ( in million tons )"), 
        main = "Total emissions")
text(seq(1999,2008,3),data+1, labels=as.character(data), col = "black")


#xlab = expression('hi'[5]*'there'[6]^8*'you'[2]),#"Year",

## -- 2 --
## Have total emissions from PM2.5 decreased in the Baltimore City, 
## Maryland (fips == "24510") from 1999 to 2008? 
## Use the base plotting system to make a plot answering this question.


ct2 <- xtabs(Emissions ~ year, data = NEI[NEI$fips == "24510",c("Emissions", "year")])
data <- round(ct2,1)

#plot
barplot(data,axes = T, col="palegreen", ylim = c(0,3500),
        xlab = expression(bold("Year")),
        ylab = expression(bold("PM ") [2.5]*"   ( in tons )"), 
        main = "Total emissions in Baltimore City, Maryland")
text(seq(0.7, 1.2*4, by=1.2), data, labels=as.character(data), pos = 3)

## -- 3 -- 
## Of the four types of sources indicated by the type 
## (point, nonpoint, onroad, nonroad) variable, 
## which of these four sources have seen decreases in emissions 
## from 1999–2008 for Baltimore City? 
## Which have seen increases in emissions from 1999–2008? 
## Use the ggplot2 plotting system to make a plot answer this question.

ct3 <- xtabs(Emissions ~ type +year , data = NEI[NEI$fips == "24510",])
data <- round(ct3,1);data
df <- data.frame(data )

library(ggplot2)
p <- qplot(year,Freq, data = df, facets = . ~type, stat="identity",geom="bar", 
      main = "Total emissions in Baltimore City by type\n", 
      ylab = expression(bold("PM ") [2.5]*"   ( in tons )"),
      xlab = expression(bold("Year")))

p +  geom_bar(stat="identity",fill = "palegreen", color = "darkgreen")+
    theme( plot.title = element_text(size = 22, face = "bold"), 
           axis.title = element_text(size = 16))

#p + theme(plot.title = element_text(size = 22, face = "bold"))
#p + ggtitle("Custom Theme")


## -- 4 -- 
## Across the United States, how have emissions from 
## coal combustion-related sources changed from 1999–2008?

#ts <- "Ext Comb /Electric Gen /Anthracite Coal /Traveling Grate (Overfeed) Stoker"
#grepl("comb (.*)? coal", ts, ignore.case=T)

nSCC <- SCC[ grepl("comb (.*)? coal", SCC$Short.Name, ignore.case=T),1]
ct4 <- xtabs(Emissions ~ year, data = NEI[NEI$SCC %in% nSCC ,])
data <- round(ct4/1000,1)
data

barplot(data,axes = T, col="palegreen", ylim = c(0,700),
        xlab = expression(bold("Year")),
        ylab = expression(bold("PM ") [2.5]*"   ( in kilo tons )"), 
        main = "Emissions of coal combustors in USA")
text(seq(0.7, 1.2*4, by=1.2), data, labels=as.character(data), pos = 3)


## -- 5 -- 
## How have emissions from motor vehicle sources changed from 1999–2008 
## in Baltimore City?
ct5 <- xtabs(Emissions ~ year, data = NEI[NEI$type == "ON-ROAD" & NEI$fips == "24510",])
data <- round(ct5,1)
data

barplot(data,axes = T, col="palegreen", ylim = c(0,400),
        xlab = expression(bold("Year")),
        ylab = expression(bold("PM ") [2.5]*"   ( in tons )"), 
        main = "Emissions from motor vehicle in Baltimor City")
text(seq(0.7, 1.2*4, by=1.2), data, labels=as.character(data), pos = 3)



## -- 6 --
## Compare emissions from motor vehicle sources in Baltimore City 
## with emissions from motor vehicle sources in Los Angeles County, 
## California (fips == "06037"). 
## Which city has seen greater changes over time in motor vehicle emissions?
ct6 <- xtabs(Emissions ~ fips + year , data = NEI[NEI$type == "ON-ROAD" & NEI$fips %in% c("24510","06037") ,])
data <- round(ct6,1);data
df <- data.frame(data )
df
names(df)[1] <- "Location"
df <- transform(df, Location = as.character(Location))
#df$Location
df$Location[df$Location == "06037"] = "Los Angeles County"
df$Location [df$Location == "24510"] = "Baltimore City"

for (iLoc in unique(df$Location)) {
    base <- df[df$Location == iLoc & df$year == 1999 ,3]
    df[df$Location == iLoc,3] <- df[df$Location == iLoc,3] - base
}
df
#df[df$fips == "06037",3] <- df[df$fips == "06037",3] - df[df$fips == "06037" & df$year == 1999 ,3]
#df[df$fips == "24510",3] <- df[df$fips == "24510",3] - df[df$fips == "24510" & df$year == 1999 ,3]



h = df$Freq[df$year == 2008];h
#h <- c(0,h)

library(ggplot2)

p <- ggplot(data = df, aes(x = year, y = Freq, group = Location, colour = Location),
            )
#p +  geom_line() +geom_point() +
#     geom_hline(yintercept= h, col="burlywood4")+ 
#     geom_text(aes(x = 4.3, y = h[1]+20, label = as.character(h[1])), col="black")+
#     geom_text(aes(x = 4.3, y = h[2]+20, label = as.character(h[2])),col="black")
              

p +  geom_line() +geom_point(size=4,show_guide=F) + 
    ggtitle("Change in emission in \nBaltimore City and Los Angeles County\n")+
    xlab (expression(bold("Year")))+
    ylab (expression(bold("PM ") [2.5]*"   ( in tons )"))+
    theme( plot.title = element_text(size = 18), 
            axis.title = element_text(size = 16))+
    geom_hline(yintercept= h, col="burlywood4",show_guide = F)+
    geom_text(aes(x = 4.25, y = h[1]+25, label = as.character(h[1])), col="black")+
    geom_text(aes(x = 4.25, y = h[2]+25, label = as.character(h[2])),col="black")



