# Assuming the correct script for setting up the environment and reading data
rm(list = ls())
library(Kendall)
library(ggplot2)
library(gplots)
 
setwd('C:/R_scripts/HT_R')

pet.daily <- read.table('per.daily.txt', header = TRUE)

station.names <- names(pet.daily[5:length(pet.daily)])
no.stations <- length(station.names)

# Monthly trend
pet.monthly <- aggregate(pet.daily[, 5:ncol(pet.daily)],
                         by = list(pet.daily$Month, pet.daily$Year),
                         FUN = sum)

pet.annual <- aggregate(pet.daily[, 2:ncol(pet.daily)],
                        by = list(pet.daily$Month, pet.daily$Year),
                        FUN = sum)

pet.annual_check <- aggregate(pet.daily[, 2:ncol(pet.daily)],
                              by = list(pet.daily$Year),
                              FUN = sum)

pet.trend.monthly <- data.frame(matrix(nrow = 12, ncol = no.stations))
pet.trend.annual <- data.frame(matrix(nrow = 34, ncol = no.stations))

rownames(pet.trend.monthly) <- 1:12
colnames(pet.trend.monthly) <- station.names

rownames(pet.trend.annual) <- 1:34
colnames(pet.trend.annual) <- station.names

for (month in 1:12) {
  subset <- pet.monthly[pet.monthly$Group.1 == month, ]
  for (station in 1:no.stations) {
    pet.trend.monthly[month, station] <- MannKendall(subset[, station + 2])$tau # Kendall tau
    pet.trend.monthly[month, station] <- summary(lm(subset[,station+2] ~ c(1:nrow(subset))))$coefficient[2,1] # Regression line
  }
}



for (year in unique(pet.annual$Group.2)) {  # Use unique years from pet.annual
  subset <- pet.annual[pet.annual$Group.2 == year, ]
  for (station in 1:no.stations) {
    if (nrow(subset) > 1) {  # Check if subset has more than one row
      pet.trend.annual[year, station] <- MannKendall(subset[, station + 2])$tau # Kendall tau
      pet.trend.annual[year, station] <- summary(lm(subset[,station+2] ~ c(1:nrow(subset))))$coefficient[2,1] # Regression line
    } else {
      pet.trend.annual[year, station] <- NA  # Assign NA if subset has less than two rows
    }
  }
}

pet.trend.annual <- na.omit(pet.trend.annual)

  
my_palette <- colorRampPalette(c('red', 'white', 'blue'))

png('PET_monthly_trend.png', width = 2000, height = 2000)


lmat_m <- rbind( c(5,3), c(2,1),c(4,4) )
lwid_m <- c(1, 4)
lhei_m <- c(2, 4, 2)

m <- as.matrix(pet.trend.monthly)

heatmap.2(m,lmat = lmat_m, lwid = lwid_m,lhei = lhei_m, Rowv = FALSE, Colv = TRUE,
          col = my_palette, breaks = c(seq(-1, 1, by = 0.1)), key = TRUE)

dev.off()

png('PET_annual_trend.png', width = 2000, height = 2000)
lmat_a <- rbind( c(5,3,0), c(2,1,4) )
lhei_a <- c(2, 4) 
lwid_a <- c(1, 4, 2)


a <- as.matrix(pet.trend.annual)

heatmap.2(a,lmat = lmat_a, lwid = lwid_a,lhei = lhei_a, Rowv = FALSE, Colv = TRUE,
          col = my_palette, breaks = c(seq(-1, 1, by = 0.1)), key = TRUE)
  

dev.off() 

print(a)
print(y)
