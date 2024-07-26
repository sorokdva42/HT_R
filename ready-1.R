# Assuming the correct script for setting up the environment and reading data
rm(list = ls()) # clear environment  
library(Kendall); library(ggplot2); library(gplots) # export libraries
 


setwd('C:/R_scripts/HT_R') # set working directory 

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

# somehow it create table starting year from 1, so I just delete NA values
pet.trend.annual <- na.omit(pet.trend.annual)

m <- as.matrix(pet.trend.monthly) # create a matrix 
a <- as.matrix(pet.trend.annual)

png('PET_monthly_trend.png', width = 2000, height = 2000) # create empty png file with given range
png('PET_annual_trend.png', width = 2000, height = 2000)

heatmap.2(m,
          main = "Heatmap",
          trace = "none",        # Вимикаємо трасування
          dendrogram = "both",   # Побудова дендрограми для рядків і стовпців
          Rowv = TRUE,           # Впорядкування рядків
          Colv = TRUE,           # Впорядкування стовпців
          col = bluered(75),     # Використання кольорової шкали
          scale = "column",         # Масштабування даних по рядках
          margins = c(10, 10))    # Встановлення відступів для графіку


heatmap.2(a,
          main = "Heatmap",
          Rowv = TRUE,
          Colv = TRUE,           
          col = bluered(75),
          scale = "column",
          margins = c(10, 10),
          key = TRUE)

dev.off() 
