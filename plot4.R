#reading data
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

#receiving data for plotting
nSCC <- SCC[ grepl("comb (.*)? coal", SCC$Short.Name, ignore.case=T),1]
ct4 <- xtabs(Emissions ~ year, data = NEI[NEI$SCC %in% nSCC ,])
data <- round(ct4/1000,1)
data


#plotting
png(file = "plot4.png", width = 800, height = 600)

par(optim)
par(mar = c(6,10,6,6))
barplot(data,axes = T, col="palegreen", ylim = c(0,700),
        cex.main=2,cex.lab=1.5, cex.axis=1.5,
        xlab = expression(bold("Year")),
        ylab = expression(bold("PM ") [2.5]*"   ( in kilo tons )"), 
        main = "Emissions of coal combustors in USA")
text(seq(0.7, 1.2*4, by=1.2), data, labels=as.character(data), pos = 3)


dev.off()
