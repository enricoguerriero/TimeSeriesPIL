# Analisi del GDP con i nuovi metodi

#come prima cosa setto la directory per importare i dati

setwd("~/Univerità/Serie Storiche Economiche/Progetto R")

#importo i dati

library(readr)
gdp_nom_tot <- read_csv("GDP_NOM_TOT.csv")
gdp_nom_pc <- read_csv("GDP_NOM_PC.csv")
cpi <- read_csv(("CPI.csv"))



# mi tiro giù i dati che mi servono

CPI_ITA <- cpi[cpi$LOCATION == "ITA", ]$Value/cpi[cpi$LOCATION == "ITA", ]$Value[1]
CPI_IRL <- cpi[cpi$LOCATION == "IRL", ]$Value/cpi[cpi$LOCATION == "IRL", ]$Value[1]
CPI_DNK <- cpi[cpi$LOCATION == "DNK", ]$Value/cpi[cpi$LOCATION == "DNK", ]$Value[1]

GDP_ITA <- gdp_nom_tot[gdp_nom_tot$LOCATION == "ITA", ]$Value/CPI_ITA
GDP_IRL <- gdp_nom_tot[gdp_nom_tot$LOCATION == "IRL", ]$Value/CPI_IRL
GDP_DNK <- gdp_nom_tot[gdp_nom_tot$LOCATION == "DNK", ]$Value/CPI_DNK

GDP_PC_ITA <- gdp_nom_pc[gdp_nom_pc$LOCATION == "ITA", ]$Value/CPI_ITA
GDP_PC_IRL <- gdp_nom_pc[gdp_nom_pc$LOCATION == "IRL", ]$Value/CPI_IRL
GDP_PC_DNK <- gdp_nom_pc[gdp_nom_pc$LOCATION == "DNK", ]$Value/CPI_DNK


# grafichini per vedere che tutto sia andato bene


plot(ts(GDP_ITA, start = c(2000,1), frequency = 1), col = "#FFBF00", lwd = 3, ylim = c(0,2500000), ylab = "Real GDP")
lines(ts(GDP_IRL, start = c(2000,1), frequency = 1), col = "#DE3163", lwd = 3)
lines(ts(GDP_DNK, start = c(2000,1), frequency = 1), col = "#FF7F50", lwd = 3)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = 2, lwd = 2,  equilogs = TRUE)
legend("topleft", c("Italia", "Irlanda", "Danimarca"), col = c("#FFBF00","#DE3163","#FF7F50"), lwd = 3)


plot(ts(GDP_PC_ITA, start = c(2000,1), frequency = 1), col = "#FFBF00", lwd = 3, ylim = c(20000,80000), ylab = "Real GDP")
lines(ts(GDP_PC_IRL, start = c(2000,1), frequency = 1), col = "#DE3163", lwd = 3)
lines(ts(GDP_PC_DNK, start = c(2000,1), frequency = 1), col = "#FF7F50", lwd = 3)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = 2, lwd = 2,  equilogs = TRUE)
legend("topleft", c("Italia", "Irlanda", "Danimarca"), col = c("#FFBF00","#DE3163","#FF7F50"), lwd = 3)


# proviamo ad applicare un easy lisciamento in media mobile di ordine 5 al gdp italiano

library(forecast)

plot(ts(GDP_PC_ITA, start = 2000), col = 1, lty = 2, lwd = 2, ylab = "GDP PRO CAPITE ITALIA")
lines(ma(ts(GDP_PC_ITA, start = 2000),5), col = 2, lwd = 3)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = 2, lwd = 2,  equilogs = TRUE)

summary(ma(ts(GDP_PC_ITA, start = 2000),5))

# diamo un'occhiata all'r quadro di sta cosa

sum((ma(GDP_PC_ITA,5)-mean(GDP_PC_ITA))^2, na.rm = TRUE)/
  sum((GDP_PC_ITA-mean(GDP_PC_ITA))^2, na.rm = TRUE)


# stampo i residui e vediamo se sono effettivamente plausibili white noise

plot(ma(ts(GDP_PC_ITA, start = 2000),5)-ts(GDP_PC_ITA, start = 2000), ylab = "Residui")
abline(0,0, lty = 2)

# sembra che questi residui facciano proprio schifo

#proviamo a fare una funzione che guarda l'r quadro di più medie mobili e le confronta

BEST_AM <- function(myserie, mypar) {
  
  # intanto "estraiamo" i parametri per fissare i limiti della funzione
  
  inf <- mypar[1]
  sup <- mypar[2]
  
  # creo il vettore degli R quadro
  
  r.vett <- vector(mode = "numeric", length = sup - inf + 1)
  
  # creiamo ciclo for che confronta le varie medie mobili
  
  for (i in inf:sup){
    
    r.squared <- sum((ma(myserie,i)-mean(myserie))^2, na.rm = TRUE)/
      sum((myserie-mean(myserie))^2, na.rm = TRUE)
    
    r.vett[i+1] <- r.squared
    
  }
  
  # andiamo a pescare l'r quadro maggiore e lo torniamo alla funzione
  
  return(which(r.vett == max(r.vett)))
  
}


# la funzione mostra come il grado migliore di media mobile per il gdp pro capite italiano è 4
# ovviamente in un range da 3 a 6

BEST_AM(GDP_PC_ITA, c(1,10))

plot(ts(GDP_PC_ITA, start = 2000), col = 1, lty = 2, lwd = 2, ylab = "GDP PRO CAPITE ITALIA")
lines(ma(ts(GDP_PC_ITA, start = 2000), 2), col = 4, lwd = 3)




spencer <- function(vect){
  
  n <- length(vect)
  
  spen <- vector(mode = "numeric", length = n-14)
  
  for (i in 1:n-14){
    
    spen[i] <- (-3*vect[i]-6*vect[i+1]-5*vect[i+2]+3*vect[i+3]+21*vect[i+4]+46*vect[i+5]+67*vect[i+6]+74*vect[i+7]+67*vect[i+8]+46*vect[i+9]+21*vect[i+10]+3*vect[i+11]-5*vect[i+12]-6*vect[i+13]-3*vect[i+14])/320
    
  }
  
  return(spen)
  
}


x <- rnorm(100)

plot(x, type = "l")

spencer(x)

