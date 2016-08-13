# creates plot of overall access scores vs thresholds used for multiple cubes

# clear env
rm(list = ls())

# read in data for 2 cubes and DA population
R_mon <- read.table("stats/R_mon_access_T_means.csv", header = TRUE, row.names = 1, sep = ",", check.names = FALSE)
Sched <- read.table("stats/sched_access_T_means.csv", header = TRUE, row.names = 1, sep = ",", check.names = FALSE)
DA_pop <- read.table("DA_pop.csv", header = TRUE, row.names = 1, sep = ",", check.names = FALSE)

# calc total acccess
R_mon_total <- R_mon * c(DA_pop)
Sched_total <- Sched * c(DA_pop)

# plot the plots - normalizing by population in each DA
plot(c(1:60), (colSums(R_mon_total) / sum(DA_pop))^(1/3), type="l", col="red" , xlab="T", ylab="A^(1/3)")
lines(c(1:60), (colSums(Sched_total) / sum(DA_pop))^(1/3), col="blue")
grid (NULL,NULL, lty = "dotted", col = "lightgray")
