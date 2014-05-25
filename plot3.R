#reading data
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

#receiving data for plotting
ct3 <- xtabs(Emissions ~ type +year , data = NEI[NEI$fips == "24510",])
data <- round(ct3,1);data
df <- data.frame(data )

#plotting
png(file = "plot3.png", width = 800, height = 600)

library(ggplot2)
p <- qplot(year,Freq, data = df, facets = . ~type, stat="identity",geom="bar", 
           main = expression("Total emissions of PM " [2.5]~"in Baltimore City by types"),
           ylab = expression(bold("PM ") [2.5]*"   ( in tons )"),
           xlab = expression(bold("Year")))

p +  geom_bar(stat="identity",fill = "palegreen", color = "darkgreen")+
    theme( plot.title = element_text(size = 18, face = "bold"), 
           axis.title = element_text(size = 16))


dev.off()
