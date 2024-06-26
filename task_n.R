# Calculate of PET based on air temperature
# Created: March 12 2024
# Author: OH

setwd('C:/Users/PC/Documents/HT_R')
library(readr)
T <- read_csv("T.csv")
T


T.station.lat <- c(49.07,49.39,48.93,49.05,50.23,50.82,50.79,50.78,50.16,49.97,50.03,50.15,49.78,49.55,49.34,49.54,49.59) # station latitude
elevation <- c(1118,425,804,483,377,777,772,675,577,328,593,498,748,1322,387,436,380) # station elevation


#############
library(sirad)
setwd('C:/Users/PC/Documents/HT_R')
Temp <- read.csv('T.csv', header = TRUE, sep = ',')


T.station.names <-  names(Temp[5:ncol(Temp)])


no.Tstation <- length(T.station.names)


lambda <- 2.265
ro <-  1000
k1 <- 100
k2 <- 5


pet.daily <- data.frame(Temp[,1:4])


for(statation in 1:no.Tstation) {
  pet <- data.frame(Temp[,1:4],Temp[,4+statation])
  
  
  for (day in 1:nrow(pet)) {
    
    Re <-  extrat(dayOfYear(as.Date(as.character(pet$Date[day]),format = '%Y%m%d')), 
                  radians(T.station.lat[statation]))$ExtraTerrestrialSolarRadiationDaily
    if(pet[day,5] + k2 >0 ) {
      pet$PET[day] <-  1000*(Re/lambda*ro)*(pet[day,5]+k2)/k1
    } else{
      pet$PET[day] <- 0
    }
    
    pet$DayLenght <- extrat(dayOfYear(as.Date(as.character(pet$Date[day]),format = '%Y%m%d')), 
                            radians(T.station.lat[statation]))$DayLenght
  } # day
  
  pet.daily[,statation+4] <- pet$PET
  
  pet.daily.station <- aggregate(pet[,6],by=list(pet[,4],pet[,3]),FUN=mean)
  x <- c(1:366)
  y <- pet.daily.station[,3] 
  y <- y * 0.000001
  plot(x,y, type = 'l', xlab= 'Day of year', ylab = 'PET [mm/d]')
  
  print(paste('Done:', Sys.time(),
              T.station.names[statation], sep = ' '))
  
} #  station 


pet.daily.station

write.table(pet.daily,'per.daily.txt',sep = '\t',
            row.names = F,col.names = colnames(Temp))



pet.annul <- aggregate(pet.daily[,5:ncol(pet.daily)],
                       by=list(pet.daily$Year), FUN = sum)
####### Liner regresion model


y <- colMeans(pet.annul[,2:ncol(pet.annul)])

lingrese <- lm(y ~ elevation)
lingrese


summary(lingrese)

p.value <- summary(lingrese)$coefficient[2,4]
slope <- summary(lingrese)$coefficient[2,1]




png('pet_elevevation34.png',width = 2000, height = 2000)
par(cex=5)
plot(elevation,y * 0.000001, pch=16,xlab = 'Elevation [m.a.s.l.]', 
     ylab= 'Mean annual PET')
# abline(lingrese)
# abline(0,1)
# abline(h=500)

dev.off()



