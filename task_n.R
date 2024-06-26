# Calculate of PET based on air temperature
# Created: March 12 2024
# Author: OH

setwd('C:/Users/PC/Documents/HT_R') # set working directoty 
library(readr); library(sirad) # import library 
T <- read_csv("T.csv") # read csv file



T.station.lat <- c(49.07,49.39,48.93,49.05,50.23,50.82,50.79,50.78,50.16,49.97,50.03,50.15,49.78,49.55,49.34,49.54,49.59) # station latitude
elevation <- c(1118,425,804,483,377,777,772,675,577,328,593,498,748,1322,387,436,380) # station elevation


#############
Temp <- read.csv('T.csv', header = TRUE, sep = ',') # read csv with header and separator ,


T.station.names <-  names(Temp[5:ncol(Temp)]) # difine station names from  Temp


no.Tstation <- length(T.station.names) # number of station 

# constants to calculate PET 
lambda <- 2.265 
ro <-  1000
k1 <- 100
k2 <- 5


pet.daily <- data.frame(Temp[,1:4]) # create new data frame to collect result of PET

# calculate PET daily, monthly, annual 
for(statation in 1:no.Tstation) { # loop for each station 
  pet <- data.frame(Temp[,1:4],Temp[,4+statation]) # create new data frame with first 4 columns from Temp + temperature column 
  
  
  for (day in 1:nrow(pet)) { # loop for each day
    
    Re <-  extrat(dayOfYear(as.Date(as.character(pet$Date[day]),format = '%Y%m%d')),  # extrat from sirad library to calculate solar radiation
                  radians(T.station.lat[statation]))$ExtraTerrestrialSolarRadiationDaily # change latitude to radians 
    if(pet[day,5] + k2 >0 ) { # check temperature > k2
      pet$PET[day] <-  1000*(Re/lambda*ro)*(pet[day,5]+k2)/k1 # if its true 1000*(Re/lambda*ro)*(pet[day,5]+k2)/k1
    } else{
      pet$PET[day] <- 0 # other cases PET = 0
    }
    
    pet$DayLenght <- extrat(dayOfYear(as.Date(as.character(pet$Date[day]),format = '%Y%m%d')), # add column with day lenght
                            radians(T.station.lat[statation]))$DayLenght
  } # day end loop
  
  pet.daily[,statation+4] <- pet$PET # add calculated daily values
  
  pet.daily.station <- aggregate(pet[,6],by=list(pet[,4],pet[,3]),FUN=mean) # calculate monthly mean PET value 
  x <- c(1:366) # days in year
  y <- pet.daily.station[,3]   
  y <- y * 0.000001 #
  plot(x,y, type = 'l', xlab= 'Day of year', ylab = 'PET [mm/d]') # create chart PET median annual
  
  print(paste('Done:', Sys.time(),
              T.station.names[statation], sep = ' ')) # show time spend and station name
  
} #  end station 


pet.daily.station

write.table(pet.daily,'per.daily.txt',sep = '\t',
            row.names = F,col.names = colnames(Temp)) # result of PET in txt file 



pet.annul <- aggregate(pet.daily[,5:ncol(pet.daily)],
                       by=list(pet.daily$Year), FUN = sum) # aggregate annual PET values 
####### Liner regresion model


y <- colMeans(pet.annul[,2:ncol(pet.annul)])

lingrese <- lm(y ~ elevation) # linear regression between mean PET value and height
lingrese


summary(lingrese) # show result of regretssion

p.value <- summary(lingrese)$coefficient[2,4]
slope <- summary(lingrese)$coefficient[2,1]




png('pet_elevevation34.png',width = 2000, height = 2000) # create png image
par(cex=5)
plot(elevation,y * 0.000001, pch=16,xlab = 'Elevation [m.a.s.l.]', 
     ylab= 'Mean annual PET')  # create chart

abline(lingrese)
abline(0,1)
abline(h=500)

dev.off()



