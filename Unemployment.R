# serie storiche sulla disoccupazione


# per prima cosa setto la directory

setwd("~/Univerità/Serie Storiche Economiche/Progetto R")

library(readr)

# ora importo tutti i dati che mi servono

UnempDataset <- read_csv("Unemployment.csv")


# divido i dati tirati giù nei 3 vettori

Unemp_ITA <- UnempDataset[UnempDataset$LOCATION == "ITA", ]$Value
Unemp_IRL <- UnempDataset[UnempDataset$LOCATION == "IRL", ]$Value
Unemp_DNK <- UnempDataset[UnempDataset$LOCATION == "DNK", ]$Value



# plottino di rito per farci un'idea


plot(ts(Unemp_ITA, start = 2000, frequency = 4), col = "#0B3245", ylim = c(3, 17), ylab = "Unemplyment")
lines(ts(Unemp_IRL, start = 2000, frequency = 4), col = "#1780B2")
lines(ts(Unemp_DNK, start = 2000, frequency = 4), col = "#0DA7F1")
legend("topleft", c("Italia", "Irlanda", "Danimarca"), col = c("#0B3245","#1780B2","#0DA7F1"), lwd = 1)



# la buona notizia è che sta calando in tutti e 3 i paesi



# solite cose per calcolare i trend

ttrend <- 1:length(Unemp_ITA) #tanto la lunghezza è uguale per tutti


# importo la mia bellissima funzione per trovare i migliori fit polinomiali

BestPolynomialFit <- function(myvec){
  
  #provo a creare diversi trend polinomiali per confrontare gli R^2 aggiustati
  
  
  #prima cosa si valuta il trend lineare
  
  polymod1 <- lm(myvec ~ ttrend)
  
  #intanto creo la variabile di appoggio e il primo R^2 aggiustato (d'ora in poi lo sottointendo)
  
  i <- 1
  r.squared.succ <- summary(polymod1)$adj.r.squared
  
  #do il valore 0 a r.squared solo per far partire il ciclo
  
  r.squared <- 0
  
  #assegno i valori per il primo ciclo
  
  polymodnext <- polymod1
  mytrend <- ttrend
  
  #creo un ciclo while
  
  while (r.squared.succ > r.squared) {
    
    r.squared <- r.squared.succ
    i <- i+1
    mytrend <- matrix(c(mytrend, ttrend^i), ncol=i)
    polymod <- polymodnext
    polymodnext <- lm(myvec ~ mytrend)
    r.squared.succ <- summary(polymodnext)$adj.r.squared
    
  }
  
  # ritorno il polymod migliore
  
  return(polymod)
  
}



# prendiamo l'italia e vediamo il polinomio che meglio la approssima


bestpolyUnemp_ITA <- BestPolynomialFit(Unemp_ITA)



# già guardando i coefficienti si vede che il trend è lineare ed è "piatto", plottiamo per avere un'idea

plot(ts(Unemp_ITA, start = 2000, frequency = 4), ylab = "Unemployment Italia")
lines(ts(fitted(bestpolyUnemp_ITA), start = 2000, frequency = 4), col = "blue", lwd = 2)

# la retta non sembra approssimare benissimo 

# vediamo l'R^2

summary(bestpolyUnemp_ITA)$adj.r.squared


# piccola analisi dei residui

plot(residuals(bestpolyUnemp_ITA))


# i residui sono palesemente ciclici





# ctrl c ctrl v per irlanda e danimarca





# solite cose per calcolare i trend

ttrend <- 1:length(Unemp_ITA) #tanto la lunghezza è uguale per tutti


# importo la mia bellissima funzione per trovare i migliori fit polinomiali

BestPolynomialFit <- function(myvec){
  
  #provo a creare diversi trend polinomiali per confrontare gli R^2 aggiustati
  
  
  #prima cosa si valuta il trend lineare
  
  polymod1 <- lm(myvec ~ ttrend)
  
  #intanto creo la variabile di appoggio e il primo R^2 aggiustato (d'ora in poi lo sottointendo)
  
  i <- 1
  r.squared.succ <- summary(polymod1)$adj.r.squared
  
  #do il valore 0 a r.squared solo per far partire il ciclo
  
  r.squared <- 0
  
  #assegno i valori per il primo ciclo
  
  polymodnext <- polymod1
  mytrend <- ttrend
  
  #creo un ciclo while
  
  while (r.squared.succ > r.squared) {
    
    r.squared <- r.squared.succ
    i <- i+1
    mytrend <- matrix(c(mytrend, ttrend^i), ncol=i)
    polymod <- polymodnext
    polymodnext <- lm(myvec ~ mytrend)
    r.squared.succ <- summary(polymodnext)$adj.r.squared
    
  }
  
  # ritorno il polymod migliore
  
  return(polymod)
  
}


# miglior trend polinomiale


bestpolyUnemp_IRL <- BestPolynomialFit(Unemp_IRL)


# provo comunque con la retta

polyUnemp1_IRL <- lm(Unemp_IRL ~ ttrend)

# r quadro bassissimo

plot(ts(Unemp_IRL, start = 2000, frequency = 4), ylab = "Unemployment Ireland")
lines(ts(fitted(polyUnemp1_IRL), start = 2000, frequency = 4), col = "blue", lwd = 2)



# già guardando i coefficienti si vede che il trend è lineare ed è "piatto", plottiamo per avere un'idea

plot(ts(Unemp_IRL, start = 2000, frequency = 4), ylab = "Unemployment Ireland")
lines(ts(fitted(bestpolyUnemp_IRL), start = 2000, frequency = 4), col = "blue", lwd = 2)

# la retta non sembra approssimare benissimo 

# vediamo l'R^2

summary(bestpolyUnemp_IRL)$adj.r.squared


# piccola analisi dei residui

plot(residuals(bestpolyUnemp_IRL), ylab = "Residuals")


# i residui sono palesemente ciclici










# solite cose per calcolare i trend

ttrend <- 1:length(Unemp_DNK) #tanto la lunghezza è uguale per tutti


# importo la mia bellissima funzione per trovare i migliori fit polinomiali

BestPolynomialFit <- function(myvec){
  
  #provo a creare diversi trend polinomiali per confrontare gli R^2 aggiustati
  
  
  #prima cosa si valuta il trend lineare
  
  polymod1 <- lm(myvec ~ ttrend)
  
  #intanto creo la variabile di appoggio e il primo R^2 aggiustato (d'ora in poi lo sottointendo)
  
  i <- 1
  r.squared.succ <- summary(polymod1)$adj.r.squared
  
  #do il valore 0 a r.squared solo per far partire il ciclo
  
  r.squared <- 0
  
  #assegno i valori per il primo ciclo
  
  polymodnext <- polymod1
  mytrend <- ttrend
  
  #creo un ciclo while
  
  while (r.squared.succ > r.squared) {
    
    r.squared <- r.squared.succ
    i <- i+1
    mytrend <- matrix(c(mytrend, ttrend^i), ncol=i)
    polymod <- polymodnext
    polymodnext <- lm(myvec ~ mytrend)
    r.squared.succ <- summary(polymodnext)$adj.r.squared
    
  }
  
  # ritorno il polymod migliore
  
  return(polymod)
  
}



# prendiamo l'italia e vediamo il polinomio che meglio la approssima


bestpolyUnemp_DNK <- BestPolynomialFit(Unemp_DNK)


polyUnemp1_DNK <- lm(Unemp_DNK ~ ttrend)


plot(ts(Unemp_DNK, start = 2000, frequency = 4), ylab = "Unemployment Denmark")
lines(ts(fitted(polyUnemp1_DNK), start = 2000, frequency = 4), col = "blue", lwd = 2)


# già guardando i coefficienti si vede che il trend è lineare ed è "piatto", plottiamo per avere un'idea

plot(ts(Unemp_DNK, start = 2000, frequency = 4), ylab = "Unemployment Denmark")
lines(ts(fitted(bestpolyUnemp_DNK), start = 2000, frequency = 4), col = "blue", lwd = 2)

# la retta non sembra approssimare benissimo 

# vediamo l'R^2

summary(bestpolyUnemp_DNK)$adj.r.squared


# piccola analisi dei residui

plot(residuals(bestpolyUnemp_DNK), ylab = "Residuals")


# i residui sono palesemente ciclici


