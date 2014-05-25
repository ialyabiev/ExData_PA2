#reading data
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

#receiving data for plotting
ct <- xtabs(Emissions ~ year, data = NEI)
data <- round(ct/1e+06,1)

#plotting
png(file = "plot1.png", width = 800, height = 600)

par(optim)
par(mar = c(6,10,6,6))
barplot(data,axes = T, col="palegreen", ylim = c(0,9),
        cex.main=2,cex.lab=1.5, cex.axis=1.5,
        xlab = expression(bold("Year")),
        ylab = expression(bold("PM ") [2.5]*" ( in millions of tons )"), 
        main = expression("Total emissions of PM " [2.5]* " in USA"))
text(seq(0.7, 1.2*4, by=1.2), data, labels=as.character(data), pos = 3)

dev.off()

