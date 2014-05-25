#reading data
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

#receiving data for plotting
ct6 <- xtabs(Emissions ~ fips + year , data = NEI[NEI$type == "ON-ROAD" & NEI$fips %in% c("24510","06037") ,])
data <- round(ct6,1);data
df <- data.frame(data )

names(df)[1] <- "Location"
df <- transform(df, Location = as.character(Location))
df$Location[df$Location == "06037"] = "Los Angeles County"
df$Location [df$Location == "24510"] = "Baltimore City"

for (iLoc in unique(df$Location)) {
    base <- df[df$Location == iLoc & df$year == 1999 ,3]
    df[df$Location == iLoc,3] <- df[df$Location == iLoc,3] - base
}

h = df$Freq[df$year == 2008]


#plotting
library(ggplot2)

png(file = "plot6.png", width = 800, height = 600)

p <- ggplot(data = df, aes(x = year, y = Freq, group = Location, colour = Location))

p +  geom_line() +geom_point(size=4,show_guide=F) + 
    ggtitle("Change in emission in \nBaltimore City and Los Angeles County\n")+
    xlab (expression(bold("Year")))+
    ylab (expression(bold("PM ") [2.5]*"   ( in tons )"))+
    theme( plot.title = element_text(size = 18), 
           axis.title = element_text(size = 16))+
    geom_hline(yintercept= h, col="burlywood4",show_guide = F)+
    geom_text(aes(x = 4.25, y = h[1]+25, label = as.character(h[1])), col="black")+
    geom_text(aes(x = 4.25, y = h[2]+25, label = as.character(h[2])),col="black")+
    geom_hline(yintercept= 0, col="burlywood4",show_guide = F, size=1)+
    geom_text(aes(x = 4.25, y = 25, label = as.character(0.00)),col="black")+
    ylim(-300, 750)



dev.off()