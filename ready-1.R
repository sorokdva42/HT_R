# Assuming the correct script for setting up the environment and reading data
rm(list = ls()) # clear environment  
library(Kendall); library(ggplot2); library(gplots) # export libraries
 
setwd('C:/Users/PC/Documents/HT_R') # set working directory 

pet.daily <- read.table('per.daily.txt', header = TRUE) # read table

station.names <- names(pet.daily[5:length(pet.daily)]) # set station names from pet.daily started from 5
no.stations <- length(station.names) # number of station  


pet.monthly <- aggregate(pet.daily[, 5:ncol(pet.daily)], # aggregate staring from 5 column in pet.daily
                         by = list(pet.daily$Month, pet.daily$Year), # aggregation predicate  by month and Year
                         FUN = sum) # function summarize 

pet.annual <- aggregate(pet.daily[, 2:ncol(pet.daily)], # aggregate staring from 2 column in pet.daily
                        by = list(pet.daily$Month, pet.daily$Year), # aggregation predicate  by month and Year
                        FUN = sum) # function summarize 

# next code just need to understand how many years in pet.annual
pet.annual_check <- unique(pet.annual$Group.2)

pet.trend.monthly <- data.frame(matrix(nrow = 12, ncol = no.stations)) # create empty table with 12 rows and 17 columns 
pet.trend.annual <- data.frame(matrix(nrow = 34, ncol = no.stations)) # create empty table with 34 rows and 17 columns 

rownames(pet.trend.monthly) <- 1:12 # get rows names 
colnames(pet.trend.monthly) <- station.names # get columns names from station.names

rownames(pet.trend.annual) <- 1:34 # get rows names 
colnames(pet.trend.annual) <- station.names # get columns names from station.names

for (month in 1:12) { # loop for month
  subset <- pet.monthly[pet.monthly$Group.1 == month, ] # create subset and get from pet.monthly month
  for (station in 1:no.stations) { # loop inside station start from 1 to 17
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

# somehow it create table startin  g year from 1, so I just delete NA values
pet.trend.annual <- na.omit(pet.trend.annual)

  
my_palette <- colorRampPalette(c('red', 'white', 'blue')) # create list of colors for next dendrogram  

png('PET_monthly_trend.png', width = 2000, height = 2000) # create empty png file with given range


lmat_m <- rbind( c(5,3), c(2,1),c(4,4) ) # parameters of 
lwid_m <- c(1, 4) 
lhei_m <- c(2, 4, 2) # relative height 

m <- as.matrix(pet.trend.monthly)

heatmap.2(m,lmat = lmat_m, lwid = lwid_m,lhei = lhei_m, Rowv = TRUE, Colv = TRUE,
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



mat <- matrix(rnorm(200), nrow = 20)
colCols <- rep(c("red", "blue"), 5)
heatmap.2(m, trace="none", ColSideColors = colCols, Rowv = TRUE,
          lmat=rbind(c(5,4), c(3,2), c(0,1)),
          lhei=c(2,4,0.2),breaks = c(seq(-4e+05, 4e+05, by = 1e+04))) 
