#reading data
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

#receiving data for plotting
ct5 <- xtabs(Emissions ~ year, data = NEI[NEI$type == "ON-ROAD" & NEI$fips == "24510",])
data <- round(ct5,1)
data

#plotting
png(file = "plot5.png", width = 800, height = 600)

par(optim)
par(mar = c(6,10,6,6))
barplot(data,axes = T, col="palegreen", ylim = c(0,400),
        cex.main=2,cex.lab=1.5, cex.axis=1.5,
        xlab = expression(bold("Year")),
        ylab = expression(bold("PM ") [2.5]*"   ( in tons )"), 
        main = "Emissions from motor vehicles in Baltimor City")
text(seq(0.7, 1.2*4, by=1.2), data, labels=as.character(data), pos = 3)



dev.off()