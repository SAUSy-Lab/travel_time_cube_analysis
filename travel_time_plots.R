# creates travel time plots between two points for multiple departure times

# clear env
rm(list = ls())

# read in data
OD_traveltimes <- read.csv("OD_pairs/vp_ellesmere_to_CBD.csv")
# out name of plot
oname <- "OD_pairs/vp_ellesmere_to_CBD.png"
# title name
title <- "Ellesmere to CBD"

# read in plottings
x1 <- c(seq(ISOdate(2016,7,18,11,0), ISOdate(2016,7,18,12,59), "mins"))
y1 <- c(OD_traveltimes[,'Schedule'])
y2 <- c(OD_traveltimes[,'Real_Monday'])
y3 <- c(OD_traveltimes[,'Real_Tuesday'])

# png the output
png(oname, width = 800)

# plot
plot(x1, y1, yaxt="n", main=title, cex.main=1.1, panel.first = c(abline(v=ISOdate(2016,7,18,12,30), col='lightgrey'), abline(v=ISOdate(2016,7,18,11,00), col='lightgrey'), abline(v=ISOdate(2016,7,18,12,0), col='lightgrey'),abline(v=ISOdate(2016,7,18,11,30), col='lightgrey')) , xlab="Departure Time", ylab="Travel Time (minutes)",  type='l', lwd=2, col='red', cex.lab=1, ylim=c(20,80), bty="n")
axis(2, pos=1468839450, col = "black")
lines(x1, y2, col='blue', lwd=2)
lines(x1, y3, col='#00B055', lwd=2)
grid (0,NULL, lty = 1, col = "lightgray")
legend(1468839600, 80, cex=0.9, c("Schedule","Monday RealTime","Tuesday RealTime"), lty=c(1,1), lwd=c(2,2,2),col=c("red","blue",'#00B055'))
dev.off()
