# analisi delle serie storiche dei prezzi delle case


# come prima cosa, anche qui, setto la repository per scaricare i dati

setwd("~/Univerità/Serie Storiche Economiche/Progetto R")


# ora importo i dataset necessari 

library(readr)

HPDatabase <- read_csv("RealHousePriceDatabase.csv")
gdp_nom_pc <- read_csv("GDP_NOM_PC.csv")
cpi <- read_csv("CPI.csv")
HPYDatabase <- read_csv("RealHousePriceYearly.csv")




# salvo nei vettori le variabili interessanti

# l'indice dei prezzi lo converto subito in base 2000

CPI_ITA_2000 <- cpi[cpi$LOCATION == "ITA",]$Value/cpi[cpi$LOCATION == "ITA",]$Value[1]*100
CPI_IRL_2000 <- cpi[cpi$LOCATION == "IRL",]$Value/cpi[cpi$LOCATION == "IRL",]$Value[1]*100
CPI_DNK_2000 <- cpi[cpi$LOCATION == "DNK",]$Value/cpi[cpi$LOCATION == "DNK",]$Value[1]*100

# il gpd lo salvo direttamente reale 

GDP_REAL_ITA <- gdp_nom_pc[gdp_nom_pc$LOCATION == "ITA", ]$Value/CPI_ITA_2000
GDP_REAL_IRL <- gdp_nom_pc[gdp_nom_pc$LOCATION == "IRL", ]$Value/CPI_IRL_2000
GDP_REAL_DNK <- gdp_nom_pc[gdp_nom_pc$LOCATION == "DNK", ]$Value/CPI_DNK_2000

# i prezzi delle case vanno convertiti subito come base 2000

HP_ITA <- HPDatabase[HPDatabase$LOCATION == "ITA", ]$Value/HPDatabase[HPDatabase$LOCATION == "ITA", ]$Value[1]*100
HP_IRL <- HPDatabase[HPDatabase$LOCATION == "IRL", ]$Value/HPDatabase[HPDatabase$LOCATION == "IRL", ]$Value[1]*100
HP_DNK <- HPDatabase[HPDatabase$LOCATION == "DNK", ]$Value/HPDatabase[HPDatabase$LOCATION == "DNK", ]$Value[1]*100

#stessa roba per quelli annui

HPY_ITA <- HPYDatabase[HPYDatabase$LOCATION == "ITA", ]$Value/HPYDatabase[HPYDatabase$LOCATION == "ITA", ]$Value[1]*100
HPY_IRL <- HPYDatabase[HPYDatabase$LOCATION == "IRL", ]$Value/HPYDatabase[HPYDatabase$LOCATION == "IRL", ]$Value[1]*100
HPY_DNK <- HPYDatabase[HPYDatabase$LOCATION == "DNK", ]$Value/HPYDatabase[HPYDatabase$LOCATION == "DNK", ]$Value[1]*100




# ora c'ho tutti i dati che mi servono


# iniziamo con un grafichino easy per farsi un'idea

plot(ts(HP_ITA, start = c(2000,1), frequency = 4), col = "#0B3245", ylim = c(80,220), ylab = "Indice dei prezzi delle case")
lines(ts(HP_IRL, start = c(2000,1), frequency = 4), col = "#1780B2")
lines(ts(HP_DNK, start = c(2000,1), frequency = 4), col = "#0DA7F1")
legend("topleft", c("Italia", "Irlanda", "Danimarca"), col = c("#0B3245","#1780B2","#0DA7F1"), lwd = 1)



# e un'idea ce la siamo fatta



# vediamo se sono trendizzabili, li prendiamo uno per volta


#intanto solita variabile di supporto ttrend

ttrend <- 1:length(HP_ITA) #tanto la lunghezza è uguale per tutti


# importo la mia bellissima funzione per trovare i migliori fit polinomiali

BestPolynomialFit <- function(myvec, firstcycle = 1){
  
  #provo a creare diversi trend polinomiali per confrontare gli R^2 aggiustati
  
  #intanto dichiaro il ttrend
  
  ttrend <- 1:length(myvec)
  mytrend <- ttrend
  
  #vediamo se è stato inserito l'argomento facoltativo
  
  
      
  for (i in 1:firstcycle){
    
    mytrend <- matrix(c(mytrend, ttrend^i), ncol = i)
        
  }
  
  
  
  
  polymod <- lm(myvec ~ mytrend)
  
  #intanto creo la variabile di appoggio e il primo R^2 aggiustato (d'ora in poi lo sottointendo)
    
  i <- 1
  r.squared.succ <- summary(polymod)$adj.r.squared
    
  #do il valore 0 a r.squared solo per far partire il ciclo
    
  r.squared <- -1
    
  #assegno i valori per il primo ciclo
    
  polymodnext <- polymod
    
  
  
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





# bom ora abbiamo tutto, proviamo a trendizzare il prezzo delle case in Italia (ricordando che è trimestrale)

# si vede subito che il lineare non ci sta per niente, stavolta neanche ci provo a fare il grafichetto iniziale


#mandiamo in input nella funzione bestpolyfit il vettore dei numeri indici dei prezzi delle case in italia e vediamo che salta fuori

bestpolyfit_HP_ITA <- BestPolynomialFit(HP_ITA) 



# si può notare che il polinomio suggerito è di 5o grado, un po' grandino

# vediamo come fitta (sperando non si mangi la stagionalità)


plot(ts(HP_ITA, start = c(2000,1), frequency = 4), ylab = "House Price Italia")
lines(ts(fitted(bestpolyfit_HP_ITA), start = c(2000, 1), frequency = 4), col = "blue", lwd = 2)


# il fit sembra SPAZIALE

# proviamo a vedere i residui, magari sono casuali o magari troviamo una stagionalità
# (o magari fanno schifo)

plot(seq(from = 2000, to = 2021.75, by = 0.25),residuals(bestpolyfit_HP_ITA), ylab = "Residuals", xlab = "Time")
abline(0,0)


# Nei residui potrebbe esserci traccia di una stagionalità


#creo la variabile su cui sto per regredire i residui

trimestri <- rep(c("Q1", "Q2", "Q3", "Q4"),22)


# regrediamooooooo

# proviamo con la regressione semplicissima


stag.HP_ITA <- lm(residuals(bestpolyfit_HP_ITA) ~ trimestri)


# vediamo com'è venuta

# ora plottiamo e facciamo R^2

plot(seq(from = 2000, to = 2021.75, by = 0.25),residuals(bestpolyfit_HP_ITA))
lines(ts(fitted(stag.HP_ITA), start = c(2000,1), frequency = 4), col = 2)

summary(stag.HP_ITA)$adj.r.squared

# penso la peggiore regressione che io abbia mai visto

# che schifo


# la stagionalità è così pessima perché i dati che abbiamo preso sono destagionalizzati



# ora analizziamo l'irlanda




# si vede subito che il lineare non ci sta per niente, stavolta neanche ci provo a fare il grafichetto iniziale


#mandiamo in input nella funzione bestpolyfit il vettore dei numeri indici dei prezzi delle case in Irlanda

bestpolyfit_HP_IRL <- BestPolynomialFit(HP_IRL) 



# la funzione non funziona 

# che palle
#proviamo manualmente qualche soluzione


polyfit10_HP_IRL <- lm(HP_IRL ~ ttrend + I(ttrend^2) + I(ttrend^3) + I(ttrend^4) + I(ttrend^5) + I(ttrend^6) + I(ttrend^7) + I(ttrend^8) + I(ttrend^9) + I(ttrend^10))


# il grado 10 è il migliore





plot(ts(HP_IRL, start = c(2000,1), frequency = 4), ylab = "House Price Ireland")
lines(ts(fitted(polyfit10_HP_IRL), start = c(2000, 1), frequency = 4), col = "blue", lwd = 2)




# proviamo a vedere i residui, magari sono casuali o magari troviamo una stagionalità
# (o magari fanno schifo)

plot(seq(from = 2000, to = 2021.75, by = 0.25),residuals(polyfit10_HP_IRL), ylab = "Residuals", xlab = "Time")
abline(0,0)


# residui abbastanza randomici, vediamo comunque se è presente stagionalità


#creo la variabile su cui sto per regredire i residui

trimestri <- rep(c("Q1", "Q2", "Q3", "Q4"),22)


# regrediamooooooo

# proviamo con la regressione semplicissima


stag.HP_IRL <- lm(residuals(polyfit10_HP_IRL) ~ trimestri)


# vediamo com'è venuta

# ora plottiamo e facciamo R^2

plot(seq(from = 2000, to = 2021.75, by = 0.25),residuals(bestpolyfit_HP_IRL))
lines(ts(fitted(stag.HP_IRL), start = c(2000,1), frequency = 4), col = 2)

summary(stag.HP_IRL)$adj.r.squared

# penso la peggiore regressione che io abbia mai visto

# che schifo


# la stagionalità è così pessima perché i dati che abbiamo preso sono destagionalizzati

# altro copia e incolla in arrivo



#mandiamo in input nella funzione bestpolyfit il vettore dei numeri indici dei prezzi delle case in italia e vediamo che salta fuori

bestpolyfit_HP_DNK <- BestPolynomialFit(HP_DNK) 


# mado non funziona più la funzione

# il grado migliore è 3 

polyfit3_HP_DNK <- lm(HP_DNK ~ ttrend + I(ttrend^2) + I(ttrend^3))



# si può notare che il polinomio suggerito è di 5o grado, un po' grandino

# vediamo come fitta (sperando non si mangi la stagionalità)


plot(ts(HP_DNK, start = c(2000,1), frequency = 4), ylab = "House Price Denmark")
lines(ts(fitted(polyfit3_HP_DNK), start = c(2000, 1), frequency = 4), col = "blue", lwd = 2)


# il fit sembra SPAZIALE

# proviamo a vedere i residui, magari sono casuali o magari troviamo una stagionalità
# (o magari fanno schifo)

plot(seq(from = 2000, to = 2021.75, by = 0.25),residuals(polyfit3_HP_DNK), ylab = "Residuals", xlab = "Time")
abline(0,0)


# Nei residui potrebbe esserci traccia di una stagionalità


#creo la variabile su cui sto per regredire i residui

trimestri <- rep(c("Q1", "Q2", "Q3", "Q4"),22)


# regrediamooooooo

# proviamo con la regressione semplicissima


stag.HP_DNK <- lm(residuals(polyfit3_HP_DNK) ~ trimestri)


# vediamo com'è venuta

# ora plottiamo e facciamo R^2

plot(seq(from = 2000, to = 2021.75, by = 0.25),residuals(polyfit3_HP_DNK))
lines(ts(fitted(stag.HP_DNK), start = c(2000,1), frequency = 4), col = 2)

summary(stag.HP_DNK)$adj.r.squared

# penso la peggiore regressione che io abbia mai visto

# che schifo








# vediamo un po' sto affordability index

# tanto per cominciare abbiamo i dati del GDP solamente annui

# quindi dobbiamo prendere la serie annua dei prezzi delle case


# calcoliamo i vettori di affordability index

AI_ITA <- HPY_ITA/GDP_REAL_ITA*100
AI_IRL <- HPY_IRL/GDP_REAL_IRL*100
AI_DNK <- HPY_DNK/GDP_REAL_DNK*100



# solito plot di rito

plot(ts(AI_ITA, start = c(2000,1), frequency = 1), col = "#0B3245", ylim = c(15,75), ylab = "Affordability index")
lines(ts(AI_IRL, start = c(2000,1), frequency = 1), col = "#1780B2")
lines(ts(AI_DNK, start = c(2000,1), frequency = 1), col = "#0DA7F1")
legend("topleft", c("Italia", "Irlanda", "Danimarca"), col = c("#0B3245","#1780B2","#0DA7F1"), lwd = 1)



# ora studiamo uno stato per volta come sempre


# vediamo la nostra amata italia


# proviamo a regredire per vedere se si ottiene un trend

ttrend <- 1:length(AI_ITA)

polyAI_ITA <- BestPolynomialFit(AI_ITA)


#plottiamo e vediamo

plot(ts(AI_ITA, start = c(2000,1), frequency = 1), ylab = "Affordability index Italia")
lines(ts(fitted(polyAI_ITA), start = 2000), col = "blue", lwd = 2)


# solito plot dei residui

plot(residuals(polyAI_ITA), ylab = "Residuals")
abline(0,0)

# i residui sembrano okay, modello approvato


# copia e incolla brutale




# proviamo a regredire per vedere se si ottiene un trend

ttrend <- 1:length(AI_IRL)

polyAI_IRL <- BestPolynomialFit(AI_IRL)


#plottiamo e vediamo

plot(ts(AI_IRL, start = c(2000,1), frequency = 1), ylab = "Affordability index Ireland")
lines(ts(fitted(polyAI_IRL), start = 2000), col = "blue", lwd = 2)


# solito plot dei residui

plot(residuals(polyAI_IRL), ylab = "Residuals")
abline(0,0)

# madonna che ciclicità nei residui vabbè teniamola








# proviamo a regredire per vedere se si ottiene un trend

ttrend <- 1:length(AI_DNK)

polyAI_DNK <- BestPolynomialFit(AI_DNK)


#plottiamo e vediamo

plot(ts(AI_DNK, start = c(2000,1), frequency = 1), ylab = "Affordability index Denmark")
lines(ts(fitted(polyAI_DNK), start = 2000), col = "blue", lwd = 2)


# solito plot dei residui

plot(residuals(polyAI_DNK), ylab = "Residuals")
abline(0,0)

# madonna che ciclicità nei residui vabbè teniamola


