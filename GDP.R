# progetto serie storiche

#come prima cosa setto la directory per importare i dati

setwd("~/Univerità/Serie Storiche Economiche/Progetto R")

#importo i dati

library(readr)
gdp_nom_tot <- read_csv("GDP_NOM_TOT.csv")
gdp_nom_pc <- read_csv("GDP_NOM_PC.csv")
cpi <- read_csv(("CPI.csv"))



#salvo nei vettori i dati interessati per il GDP

GDP_NOM_ITA <- gdp_nom_tot[gdp_nom_tot$LOCATION == "ITA", ]$Value
GDP_NOM_IRL <- gdp_nom_tot[gdp_nom_tot$LOCATION == "IRL", ]$Value
GDP_NOM_DNK <- gdp_nom_tot[gdp_nom_tot$LOCATION == "DNK", ]$Value


#stampo per un confronto i tre GDP nominali totali

plot(ts(GDP_NOM_ITA, start = c(2000,1), frequency = 1), col = "#0B3245", ylim = c(0,3000000), ylab = "Nominal GDP")
lines(ts(GDP_NOM_IRL, start = c(2000,1), frequency = 1), col = "#1780B2")
lines(ts(GDP_NOM_DNK, start = c(2000,1), frequency = 1), col = "#0DA7F1")
legend("topleft", c("Italia", "Irlanda", "Danimarca"), col = c("#0B3245","#1780B2","#0DA7F1"), lwd = 1)


#Confronto non molto sensato, influenzato da inflazione e demografia dei paesi


# Proviamo con il GDP pro capita, vediamo che succede


# Salvo nei vettori quello che mi serve

GDP_NOM_PC_ITA <- gdp_nom_pc[gdp_nom_pc$LOCATION == "ITA", ]$Value
GDP_NOM_PC_IRL <- gdp_nom_pc[gdp_nom_pc$LOCATION == "IRL", ]$Value
GDP_NOM_PC_DNK <- gdp_nom_pc[gdp_nom_pc$LOCATION == "DNK", ]$Value


#stampo per un confronto i tre GDP nominali pro capita

plot(ts(GDP_NOM_PC_ITA, start = c(2000,1), frequency = 1), col = "#0B3245", ylim = c(25000, 120000), ylab = "Nominal GDP pro capita")
lines(ts(GDP_NOM_PC_IRL, start = c(2000,1), frequency = 1), col = "#1780B2")
lines(ts(GDP_NOM_PC_DNK, start = c(2000,1), frequency = 1), col = "#0DA7F1")
legend("topleft", c("Italia", "Irlanda", "Danimarca"), col = c("#0B3245","#1780B2","#0DA7F1"), lwd = 1)



# Già meglio, ma ora proviamo a mettere i dati al netto dell'inflazione






# Calcoliamo il GDP reale utilizzando l'indice dei prezzi al consumo




# Tiriamo giù dal dataframe il vettore che ci interessa

CPI_ITA_15 <- cpi[cpi$LOCATION == "ITA", ]$Value
CPI_IRL_15 <- cpi[cpi$LOCATION == "IRL", ]$Value
CPI_DNK_15 <- cpi[cpi$LOCATION == "DNK", ]$Value



# Ora portiamo la base all'anno 2000 (perché è più bello)

CPI_ITA_00 <- CPI_ITA_15/CPI_ITA_15[1]*100
CPI_IRL_00 <- CPI_IRL_15/CPI_IRL_15[1]*100
CPI_DNK_00 <- CPI_DNK_15/CPI_DNK_15[1]*100




# Ora si divide il GDP nominale per l'indice dei prezzi al consumo e si ottiene il GDP reale

GDP_REAL_ITA <- GDP_NOM_ITA/CPI_ITA_00
GDP_REAL_IRL <- GDP_NOM_IRL/CPI_IRL_00
GDP_REAL_DNK <- GDP_NOM_DNK/CPI_DNK_00



# Plottiamo sti GDP e confrontiamoli

plot(ts(GDP_REAL_ITA, start = c(2000,1), frequency = 1), col = "#0B3245", ylim = c(0,25000), ylab = "Real GDP")
lines(ts(GDP_REAL_IRL, start = c(2000,1), frequency = 1), col = "#1780B2")
lines(ts(GDP_REAL_DNK, start = c(2000,1), frequency = 1), col = "#0DA7F1")
legend("topleft", c("Italia", "Irlanda", "Danimarca"), col = c("#0B3245","#1780B2","#0DA7F1"), lwd = 1)



# Anche qui, ci siamo depurati dall'inflazione ma c'è ancora la componente demografica


# Passiamo al GDP reale PRO CAPITA così non avremo più di questi problemi



# Dividiamo il GDP nominale pro capita per il CPI (sempre stessa roba)

GDP_REAL_PC_ITA <- GDP_NOM_PC_ITA/CPI_ITA_00
GDP_REAL_PC_IRL <- GDP_NOM_PC_IRL/CPI_IRL_00
GDP_REAL_PC_DNK <- GDP_NOM_PC_DNK/CPI_DNK_00



# Plottiamoli e finalmente uscirà qualcosa di sensato

plot(ts(GDP_REAL_PC_ITA, start = c(2000,1), frequency = 1), col = "#0B3245", ylim = c(200,800), ylab = "Real GDP pro capita")
lines(ts(GDP_REAL_PC_IRL, start = c(2000,1), frequency = 1), col = "#1780B2")
lines(ts(GDP_REAL_PC_DNK, start = c(2000,1), frequency = 1), col = "#0DA7F1")
legend("topleft", c("Italia", "Irlanda", "Danimarca"), col = c("#0B3245","#1780B2","#0DA7F1"), lwd = 1)


# FINALMENTE UNA COSA CHE HA SENSO!

# Tempo di fare le cose per cui abbiamo studiato

# Troviamo solo il trend perché abbiamo i dati annui e ad occhio non sembrano esserci ciclicità di medio - lungo periodo



# Proviamo ad utilizzare un modello di regressione lineare


# Come prima cosa creiamo la solita variabile ttrend

ttrend <- 1:length(GDP_REAL_PC_ITA) #tanto è la stessa lunghezza anche per IRL e DNK





#creo una funzione per verificare quale polinomio fitta meglio i dati (in generale)

BestPolynomialFit <- function(myvec){
  
  #provo a creare diversi trend polinomiali per confrontare gli R^2 aggiustati
  
  
  #prima cosa si valuta il trend lineare
  
  polymod1 <- lm(myvec ~ ttrend)
  
  #intanto creo la variabile di appoggio e il primo R^2 aggiustato (d'ora in poi lo sottointendo)
  
  i <- 1
  r.squared.succ <- summary(polymod1)$adj.r.squared
  
  #do il valore 0 a r.squared solo per far partire il ciclo
  
  r.squared <- -1
  
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




# facciamo la solita regressione lineare di primo grado

polyval1_ITA <- lm(GDP_REAL_PC_ITA ~ ttrend)


# vediamo l'R^2 aggiustato e come fitta il modello

plot(ts(GDP_REAL_PC_ITA, start = c(2000,1), frequency = 1), col = 1, ylab = "Real GDP pro capita Italia")
abline(polyval1_ITA$coefficients[1]-2000*polyval1_ITA$coefficients[2],polyval1_ITA$coefficients[2], col="blue", lwd = 2)

summary(polyval1_ITA)$adj.r.squared


# si vede sia dal grafico che dall'R^2 che la retta fa schifissimo


# utilizzo la funzione bespolifit per vedere il grado polinomiale migliore

bestpolyval_ITA <- BestPolynomialFit(GDP_REAL_PC_ITA)

#vediamo l'R^2

summary(bestpolyval_ITA)$adj.r.squared



# vediamo se la funzione ha funzionato graficamente

plot(ts(GDP_REAL_PC_ITA, start = c(2000,1), frequency = 1), col = 1, ylab = "Real GDP pro capita Italia")
lines(ts(fitted(bestpolyval_ITA), start=c(2000,1), frequency = 1), col="blue", lwd = 2)


# direi che ci sta


# diamo un'occhiata ai residui per vedere se sono effettivamente casuali o se hanno ancora un trend intrinseco

plot(2000:2021,residuals(bestpolyval_ITA), ylab = "Residuals", xlab = "Time")
abline(0,0)


# nei residui sembra esserci ciclicità, ma i picchi sono riconducibili a situazioni politiche e macroeconomiche dei vari periodi






# sostanzialmente faccio copia e incolla di questo pezzo di codice per gli altri due paesi





# partiamo con l'Irlanda



# facciamo la solita regressione lineare di primo grado

polyval1_IRL <- lm(GDP_REAL_PC_IRL ~ ttrend)


# vediamo l'R^2 aggiustato e come fitta il modello

plot(ts(GDP_REAL_PC_IRL, start = c(2000,1), frequency = 1), col = 1, ylab = "Real GDP pro capita Ireland")
abline(polyval1_IRL$coefficients[1]-2000*polyval1_IRL$coefficients[2],polyval1_IRL$coefficients[2], col="blue", lwd = 2)

summary(polyval1_IRL)$adj.r.squared


# si vede sia dal grafico che dall'R^2 che la retta fa schifissimo


# utilizzo la funzione bespolifit per vedere il grado polinomiale migliore

bestpolyval_IRL <- BestPolynomialFit(GDP_REAL_PC_IRL)


#vediamo l'R^2

summary(bestpolyval_IRL)$adj.r.squared


# vediamo se la funzione ha funzionato graficamente

plot(ts(GDP_REAL_PC_IRL, start = c(2000,1), frequency = 1), col = 1, ylab = "Real GDP pro capita Ireland")
lines(ts(fitted(bestpolyval_IRL), start=c(2000,1), frequency = 1), col="blue", lwd = 2)


# direi che ci sta


# diamo un'occhiata ai residui per vedere se sono effettivamente casuali o se hanno ancora un trend intrinseco

plot(2000:2021,residuals(bestpolyval_IRL), ylab = "Residuals", xlab = "Time")
abline(0,0)


# nei residui sembra esserci ciclicità, ma i picchi sono riconducibili a situazioni politiche e macroeconomiche dei vari periodi







# proviamo con la Danimarca e poi siamo a posto




# facciamo la solita regressione lineare di primo grado

polyval1_DNK <- lm(GDP_REAL_PC_DNK ~ ttrend)


# vediamo l'R^2 aggiustato e come fitta il modello

plot(ts(GDP_REAL_PC_DNK, start = c(2000,1), frequency = 1), col = 1, ylab = "Real GDP pro capita Denmark")
abline(polyval1_DNK$coefficients[1]-2000*polyval1_DNK$coefficients[2],polyval1_DNK$coefficients[2], col=2)

summary(polyval1_DNK)$adj.r.squared


# già la retta è decente

# vuoi che la retta sia il migliore?

# boh ora vediamo


# utilizzo la funzione bespolifit per vedere il grado polinomiale migliore

bestpolyval_DNK <- BestPolynomialFit(GDP_REAL_PC_DNK)

#non lo è


#vediamo l'R^2

summary(bestpolyval_DNK)$adj.r.squared


# vediamo se la funzione ha funzionato graficamente

plot(ts(GDP_REAL_PC_DNK, start = c(2000,1), frequency = 1), col = 1, ylab = "Real GDP pro capita Denmark")
lines(ts(fitted(bestpolyval_DNK), start=c(2000,1), frequency = 1), col="blue", lwd = 2)


# direi che ci sta


# diamo un'occhiata ai residui per vedere se sono effettivamente casuali o se hanno ancora un trend intrinseco

plot(2000:2021,residuals(bestpolyval_DNK), ylab = "Residuals", xlab = "Time")
abline(0,0)


# nei residui sembra esserci ciclicità ma assai più lieve degli altri










# confrontiamo i trend che sono venuti fuori da tutta sta cosa 

plot(ts(fitted(bestpolyval_ITA), start = 2000), col = "#0B3245", ylim = c(250,800), ylab = "Trend")
lines(ts(fitted(bestpolyval_IRL), start = 2000), col = "#1780B2")
lines(ts(fitted(bestpolyval_DNK), start = 2000), col = "#0DA7F1")
legend("topleft", c("Italia", "Irlanda", "Danimarca"), col = c("#0B3245","#1780B2","#0DA7F1"), lwd = 1)

