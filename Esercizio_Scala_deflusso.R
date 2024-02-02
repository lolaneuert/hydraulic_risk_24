################################################################################
### Elaborazioni relative all'identificazione di una scala di 
# deflusso a partire da diverse campagne di misura          


# Carico i dati di portata
# (4 diverse campagne di misura, accuratezza Q: dev. std. 25%)
Dati <- read.csv(file = "es_scala_deflusso/Misure_portata.csv")

# h.mis: tirante in m
# Q.mis: corrispondente portata in m^3/s
# le campagne 3 e 4 sono costruite seguendo le direttative ISE mentre le campagne 1 e 2 mostrano solo 5 valori

## Q = a h^b
## ln(a) = ln(a) + b x ln(h)

# variable dipendente ln(a), variabile indipendente ln(h), quindi ln del tirante

# Costruisco la scala di deflusso con il primo set di dati (mis1)
# 1 - ripetere la costruzione per gli altri set
# 2 - confrontare le scale di deflusso ottenute e le corrispondenti stima di Q(h=2m))
h.mis <- Dati$h.mis1
Q.mis <- Dati$Q.mis1
# # Esempio con il secondo set di dati (mis2)
# h.mis<-Dati$h.mis2
# Q.mis<-Dati$Q.mis2


################################################################################
# Scala di deflusso: regressione lineare tra le trasformate logaritmiche
# Uso la funzione di R "lm" --> fits a linear model 
# lm calcola la distanza residua tra i misuramenti/dati e il ln(h) (tirante sulla y-assi), 
# e cerca a minimizzare la distanza sommaria tra i dati e il modello 
# b als steigung des modells, ln(a) als distanz zwischen knotenpunkt 0 und schnittpunkt des modells mit y

Sc.Def <- lm(formula = log(Q.mis) ~ log(h.mis), na.action = na.exclude)
# base syntax: lm (y ~ x)  (con y = q + mx + errore), y = dipendente
# quindi la formula contiene log naturale delle portate dipendente dal log naturale del tirante
# NAs are excluded to allow calculation of the model 

Sc.Def$coefficients[1] # intercept della retta
exp(Sc.Def$coefficients[1]) # esponente della scala di deflusso empirica

summary(Sc.Def) # coefficienti e statistiche di interesse
# Intervallo di confidenza della scala di deflusso
# Traccio le fasce per tiranti arbitrari da 0.1m a 2m con passo 0.1m
# la funzione "predict" richiede in ingresso una variabile "data.frame"
# intercept = log (a) 
# log(h.mis) = la pendenza, in questo caso b (log del tirante)
# the closer adjusted R-squared is to 1 the better the model: here 0.999

################################################################################
Tiranti <- data.frame(h.mis = seq(0.1, 2, .1)) # seq requested: numeri da 0.1 a 2, in passi di .1
# con altezza massima di 2m perchè richiesto cosi nel esercizio
# dataframe arbitrario con valori inventati (fake osservazioni), ci serve un df per le funzioni dopo, 
# chiamando la sequenza h.mis cosi R dopo sa sono la stessa cosa come il tirante prima (h.mis)

Confidenza <- signif(exp(predict(Sc.Def,Tiranti,interval = "confidence")), digits = 4)
# usando i valori veri e inventati con predict() si produce un modello con intervalli di confidenza al 95%
# fit = model --> ln(Q), lwr = fascia di confidenza inferiore, up = fascia di confidenza superiore
# il primo valore dello scala di deflusso (fit) e negativo, perché lui associa a un tirante di 0.1m und deflusso negativo/verso monte
# exp() per trasformare i risultati del modello: ln(Q) diventa una portata in m/s --> piu leggibile
# signif() toglie le ciffre insignifcative, cosi ci rimangono solo 4 digits
# quindi Confidenza mi valuta le fascie di confidenza, li trasforma in dati normali, con solo 4 digits
# questo formato viene utilizzato nelle vere pubblicazioni idrologiche

# Salvo la scala di deflusso e fasce di confidenza in un file txt
# per utilizzi successivi (ad es. importazione in excel)
Sc.Def.mis1 <- Tiranti # contiene h.mis
# ora aggiungiamo le confidenze nello stesso file (che fin'ora contiene solo tirante), creando nuove colonne 2 a 4
Sc.Def.mis1[,2:4] <- Confidenza # il piu stretto la fascia di confidenza, il piu preciso è il modello
colnames(Sc.Def.mis1) <- c("h (m)", "Q(m^3/s)", "Int.Conf-", "Int.Conf+")
write.table(Sc.Def.mis1,file = "ScalaDef_mis1.txt", sep = ";", dec = ".", row.names = F)
# ATTENZIONE AL NOME DEL FILE, RIFERIMENTO ESPLICITO A "mis1" 

# Rappresento i risultati (scala naturale) 
# con Q.mis (5 valori di portata misurati, da 10cm a 240m), h.mis (5 valori di tirante misurati, da 1cm a 2m)
plot(Q.mis, h.mis,
     xlim = c(0.1, 240), ylim = c(0.01, 2),
     xlab = expression(paste("Portata (",m^3/s,")", sep = "")),
     ylab = "Tirante (m)",
     main = "Misure di portata e scala di deflusso",
     type = "p", col = "red")

lines(Confidenza[,1], Tiranti[,], col = "blue", lwd = 2)
lines(Confidenza[,2], Tiranti[,], col = "red", lty = "dashed")
lines(Confidenza[,3], Tiranti[,], col = "red", lty = "dashed")
# aggiungiamo linee, in blu il modello (probabilita piu alta) 
# e in rosso la confidenza minima e massima

# Da cui la portata corrispondente ad un tirante di 2m risulta
Q.2m <- signif(Confidenza[which(Tiranti$h.mis == 2),], digits = 4) # signif ti da un numero di decimali significativi
# con la quale R trova la linea che soddisfa la condizione, qui tirante uguale a 2m
Q.2m # valori di portata
Q.2m/Q.2m[1] # rapporto tra stima e estremi dell'intervallo di confidenza (per capire meglio uso percentualità)
# risultati fit: 1 (perche I subtract it from itself), lwr:1.76 (quindi - ~25%), upr:1.3 (quindi + ~30%), non è simetrico

# al momento non si puo dire niente sulla accuratezza (manca il valore teorico, in realtà è sempre difficile)
# pero la precisione è abbastanza alta, visto che i misuramenti sono vicini alla linea blu, con le fascie di confidenza abbastanza schiacciate
# per arrivare a interpretazioni piu validi ci serve pero il confronto e la conferma con altri campionamenti
# per la relazione/legge teorica: Q = 42.42 h^5/3 (m³/s), con h in m -> risposta generale

Q.teo <- 42.42*Tiranti^(5/3) # moltiplico i nostri tiranti con il valore teorico, lo aggiungio in una nuova lista
colnames(Q.teo) <- "Portata" 
lines(Q.teo[,], Tiranti[,], col = "green", lwd = 3) # aggiungi linea al grafico
# potrei selezionare le colonne giuste anche scrivendo Q.teo$Portata e Tiranti$h.mis
# ora vediamo che la linea verde (teorica) rimane fuori dalla maggiorparte del nostro modello
# quindi l'accuratezza è molto bassa, la realtà sta fuori dalle nostre fascie di confidenza
# perchè siamo stati sfortunati a misurare dei valori soprastima 
# troppo pochi valori, le norme iso richiedono almeno 12-15 copie di misurazioni portata-tirante)


# Rappresento i risultati (scala logaritmica), aggiungo log, che specifica quale asse log viene usata, qui sia x che y (non cambio i valori, manipulizzo solo le assi)
plot(Q.mis, h.mis, log = "xy",
     xlim = c(0.05, 240), ylim = c(0.01, 2),
     xlab = expression(paste("Portata (",m^3/s,")", sep = "")),
     ylab = "Tirante (m)",
     main = "Misure di portata e scala di deflusso",
     type = "p", col = "red")

lines(Confidenza[,1], Tiranti[,], col = "blue", lwd = 2)
lines(Confidenza[,2], Tiranti[,], col = "red", lty = "dashed")
lines(Confidenza[,3], Tiranti[,], col = "red", lty = "dashed")
lines(Q.teo[,], Tiranti[,], col = "green", lwd = 3)
# ora le nostre fascie sono diventate simetriche, quindi vediamo che la precisione è alta (fascie parallele strette)
# le 5 copie di misuramenti sono allineate quasi perfettamente con il nostro modello, purtroppo abbiamo subito una soprastima
# in realta non possiamo conoscere la verità, il valore teorico di 42.42 funziona solo in un mondo perfetto
# per questa raggione dobbiamo provare a fare abbastanza misuramenti (massimo numero di valori) per arrivare il piu vicino alla realta
# quindi il nostro modello non è rappresentativo 


# per rifarlo con la seconda (etc) campagna di misura cambio solo h.mis e Q.mis
h.mis <- Dati$h.mis2
Q.mis <- Dati$Q.mis2

Sc.Def <- lm(formula = log(Q.mis) ~ log(h.mis), na.action = na.exclude)

Sc.Def$coefficients[1] # intercept della retta
exp(Sc.Def$coefficients[1]) # esponente della scala di deflusso empirica

summary(Sc.Def)
Tiranti <- data.frame(h.mis = seq(0.1, 2, .1))
Confidenza <- signif(exp(predict(Sc.Def,Tiranti,interval = "confidence")), digits = 4)

Sc.Def.mis2 <- Tiranti 
Sc.Def.mis2[,2:4] <- Confidenza 
colnames(Sc.Def.mis2) <- c("h (m)", "Q(m^3/s)", "Int.Conf-", "Int.Conf+")
write.table(Sc.Def.mis2,file = "ScalaDef_mis2.txt", sep = ";", dec = ".", row.names = F)

plot(Q.mis, h.mis,
     xlim = c(0.1, 240), ylim = c(0.01, 2),
     xlab = expression(paste("Portata (",m^3/s,")", sep = "")),
     ylab = "Tirante (m)",
     main = "Misure di portata e scala di deflusso",
     type = "p", col = "red")

lines(Confidenza[,1], Tiranti[,], col = "blue", lwd = 2)
lines(Confidenza[,2], Tiranti[,], col = "red", lty = "dashed")
lines(Confidenza[,3], Tiranti[,], col = "red", lty = "dashed")

Q.2m <- signif(Confidenza[which(Tiranti$h.mis == 2),], digits = 4) 
Q.2m 
Q.2m/Q.2m[1]

Q.teo <- 42.42*Tiranti^(5/3) 
colnames(Q.teo) <- "Portata" 
lines(Q.teo[,], Tiranti[,], col = "green", lwd = 3) 

plot(Q.mis, h.mis, log = "xy",
     xlim = c(0.05, 240), ylim = c(0.01, 2),
     xlab = expression(paste("Portata (",m^3/s,")", sep = "")),
     ylab = "Tirante (m)",
     main = "Misure di portata e scala di deflusso",
     type = "p", col = "red")

lines(Confidenza[,1], Tiranti[,], col = "blue", lwd = 2)
lines(Confidenza[,2], Tiranti[,], col = "red", lty = "dashed")
lines(Confidenza[,3], Tiranti[,], col = "red", lty = "dashed")
lines(Q.teo[,], Tiranti[,], col = "green", lwd = 3)
# per questo modello la precisione è molto piu bassa, perche i valori risultati (sono solo 5) 
# sono non utilizzabili (due tiranti vicino a 0, quindi in magra ovviamente anche la portata sarà 0, i valori piu grandi non lineari)
# la accuratezza è piu alta, perchè almeno la linea verde sta dentro nelle fascie di conifdenza


# terza campagna
h.mis <- Dati$h.mis3
Q.mis <- Dati$Q.mis3

Sc.Def <- lm(formula = log(Q.mis) ~ log(h.mis), na.action = na.exclude)

Sc.Def$coefficients[1] # intercept della retta
exp(Sc.Def$coefficients[1]) # esponente della scala di deflusso empirica

summary(Sc.Def)
Tiranti <- data.frame(h.mis = seq(0.1, 2, .1))
Confidenza <- signif(exp(predict(Sc.Def,Tiranti,interval = "confidence")), digits = 4)

Sc.Def.mis3 <- Tiranti 
Sc.Def.mis3[,2:4] <- Confidenza 
colnames(Sc.Def.mis3) <- c("h (m)", "Q(m^3/s)", "Int.Conf-", "Int.Conf+")
write.table(Sc.Def.mis3,file = "ScalaDef_mis2.txt", sep = ";", dec = ".", row.names = F)

plot(Q.mis, h.mis,
     xlim = c(0.1, 240), ylim = c(0.01, 2),
     xlab = expression(paste("Portata (",m^3/s,")", sep = "")),
     ylab = "Tirante (m)",
     main = "Misure di portata e scala di deflusso",
     type = "p", col = "red")

lines(Confidenza[,1], Tiranti[,], col = "blue", lwd = 2)
lines(Confidenza[,2], Tiranti[,], col = "red", lty = "dashed")
lines(Confidenza[,3], Tiranti[,], col = "red", lty = "dashed")

Q.2m <- signif(Confidenza[which(Tiranti$h.mis == 2),], digits = 4) 
Q.2m 
Q.2m/Q.2m[1]

Q.teo <- 42.42*Tiranti^(5/3) 
colnames(Q.teo) <- "Portata" 
lines(Q.teo[,], Tiranti[,], col = "green", lwd = 3) 

plot(Q.mis, h.mis, log = "xy",
     xlim = c(0.05, 240), ylim = c(0.01, 2),
     xlab = expression(paste("Portata (",m^3/s,")", sep = "")),
     ylab = "Tirante (m)",
     main = "Misure di portata e scala di deflusso",
     type = "p", col = "red")

lines(Confidenza[,1], Tiranti[,], col = "blue", lwd = 2)
lines(Confidenza[,2], Tiranti[,], col = "red", lty = "dashed")
lines(Confidenza[,3], Tiranti[,], col = "red", lty = "dashed")
lines(Q.teo[,], Tiranti[,], col = "green", lwd = 3)

# quarta campagna
h.mis <- Dati$h.mis4
Q.mis <- Dati$Q.mis4

plot(h.mis, Q.mis)
Sc.Def <- lm(formula = log(Q.mis) ~ log(h.mis), na.action = na.exclude)

Sc.Def$coefficients[1] # intercept della retta
exp(Sc.Def$coefficients[1]) # esponente della scala di deflusso empirica

summary(Sc.Def)
Tiranti <- data.frame(h.mis = seq(0.1, 2, .1))
Confidenza <- signif(exp(predict(Sc.Def,Tiranti,interval = "confidence")), digits = 4)

Sc.Def.mis4 <- Tiranti 
Sc.Def.mis4[,2:4] <- Confidenza 
colnames(Sc.Def.mis4) <- c("h (m)", "Q(m^3/s)", "Int.Conf-", "Int.Conf+")
write.table(Sc.Def.mis4,file = "ScalaDef_mis4.txt", sep = ";", dec = ".", row.names = F)

plot(Q.mis, h.mis,
     xlim = c(0.1, 240), ylim = c(0.01, 2),
     xlab = expression(paste("Portata (",m^3/s,")", sep = "")),
     ylab = "Tirante (m)",
     main = "Misure di portata e scala di deflusso",
     type = "p", col = "red")

lines(Confidenza[,1], Tiranti[,], col = "blue", lwd = 2)
lines(Confidenza[,2], Tiranti[,], col = "red", lty = "dashed")
lines(Confidenza[,3], Tiranti[,], col = "red", lty = "dashed")

Q.2m <- signif(Confidenza[which(Tiranti$h.mis == 2),], digits = 4) 
Q.2m 
Q.2m/Q.2m[1]

Q.teo <- 42.42*Tiranti^(5/3) 
colnames(Q.teo) <- "Portata" 
lines(Q.teo[,], Tiranti[,], col = "green", lwd = 3) 

plot(Q.mis, h.mis, log = "xy",
     xlim = c(0.05, 240), ylim = c(0.01, 2),
     xlab = expression(paste("Portata (",m^3/s,")", sep = "")),
     ylab = "Tirante (m)",
     main = "Misure di portata e scala di deflusso",
     type = "p", col = "red")

lines(Confidenza[,1], Tiranti[,], col = "blue", lwd = 2)
lines(Confidenza[,2], Tiranti[,], col = "red", lty = "dashed")
lines(Confidenza[,3], Tiranti[,], col = "red", lty = "dashed")
lines(Q.teo[,], Tiranti[,], col = "green", lwd = 3)