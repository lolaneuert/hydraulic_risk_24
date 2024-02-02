################################################################################
### Analisi di frequenza locale:    
# Esempio per Savio a San Vittore 


## STEP 1 - IDENTIFICAZIONE DEL CAMPIONE (gia' fatto)
# Carico il DB delle serie AMS
AMS <- read.csv(file = "es_stima_locale/Serie_AMS_ROMA.csv")
# Salvo i nomi delle stazioni in una variabile Nomi.Staz
Nomi.Staz <- colnames(AMS)

# Memorizzo i dati del SAVIO.S_Vittore ordinati in maniera crescente nella variabile "x"
# Trovo la posizione della stazione all’interno di Nomi.Staz
Sito <- which(Nomi.Staz == "SAVIO.S_Vittore") # 42 anni
# Altri esempi
# Sito <- which(Nomi.Staz == "RENO.Casalecchio") # 74 anni
# Sito <- which(Nomi.Staz == "LAMONE.Grattacoppa") # 15 anni

x <- sort(AMS[, Sito], na.last = NA); x
# prima parte lo ordina, seconda parte del comando mi fa vedere x
# x pare di essere normalissimo (di distribuzione)
plot(x) # visualizzo x

## STEP 2 - CALCOLO DELLA FREQUENZA CAMPIONARIA
# Calcolo le variabili per rappresentare la cdf campionaria
n <- length(x); n # Lunghezza della serie
pp <- (1:n)/(n+1); pp # Weibull plotting position 
# frequenza cumulata: c'è una probabilita di 2% di osservare un valore piu basso del minimo
# e una probabilita di 97% di non osservare un valore piu alto del massimo 

ppT <- 1/(1-pp); ppT # Stime empiriche del tempo di ritorno (usando il legame mathematico T = 1/1-F)
# il primo valore è poco piu di un anno (perchè devo aspettare almeno un anno se voglio osservare valori massimi annuali (AMS))
plot(ppT)
# la curva e molto lenta all'inizio, se T = 2 anni: 1/1-0.5 (osservazione che supera metta delle osservazioni, la mediana, 50%)
# dopo la stima della curva cresce molto piu rapidamente fino a 43 (con 42 osservazioni, arrivo a 43 anni di T)


# rappresentazione visuale della frequenza campionaria (non relativa, dovrei dividere per 42)
hist(x) # si vede che la moda delle osservazioni è 12 (valore piu frequente)
x[21:22] # la mediana è tra queste due (perche ho 42 campionati), quindi tra 311.0 e 339.6
mean(x[21:22]) # la mediana precisa: 325.3
mean(x) # la media è 378-4452

# frequenza cumulata (plot con x on the x, e pp on the y)
plot(x, pp, type = "l", xlab = "Portata", ylab = "CDF")


# Rappresentazione grafica (invece di probabilità ora uso il tempo di ritorno stimato)
plot(ppT, x, type = "b",
     main = Nomi.Staz[Sito],
     xlim = c(1,200), # Limite asse x
     ylim = c(0,1.5*max(x)), # Limite asse y
     xlab = "Tempo di ritorno (anni)", 
     ylab = expression(paste("Portata [", m^3, "/s]")),
     log = "x")

# type = b vuole dire entrambe, specifico io la lunghezza delle assi, usando log per la asse x

# il problema è pero che noi ci fermiamo a 43 anni, anche se in pratica abbiamo bisognio di sapere il tempo di ritorno fino a 100-200 anni
# quindi abbiamo bisognio di estrapolare: si crea un modello matematico: distribuzione di frequenza teorica


## STEP 3 - SCELTA DELLA DISTRIBUZIONE DI FREQUENZA
# Bonta' di adattamento (goodness of the fit) - distr. Gumbel
# Plotting-Position Correlation Coefficient (PPCC) test
# Carico il pacchetto "ppcc"
library(ppcc)

# Eseguo il test per
# - la distrib. di Gumbel
# - la pp di Weibull
ppccTest(x, qfn = "qgumbel", ppos = "Weibull")
# se p-value è maggiore della significativita' scelta: 0.05 la dist. di Gumbel e' idonea
# in caso contrario si possono considerare altre distribuzioni
# in questo caso il p-value e di 0.3767, quindi è maggiore di 5%: gumbel puo essere accetato!

# STEP 4 - STIMA DELLA DISTRIBUZIONE DI FREQUENZA
# Carico il pacchetto "extRemes" per la stima MLE dei parametri della distribuzione di Gumbel e GEV
library(extRemes) # usato solo per valori estremi
# il pacchetto extRemes usa oggetti "fevd" (Fitting extreme value distribution functions (EVDs: GEV, Gumbel, GP, Exponential, PP) to data (block maxima or threshold excesses))
fevd(x, type = "Gumbel")
# fa vedere parametri xi e alpha (posizione e scala), la scala è di ca 150 m³/s, e la media di ca 288 m³/s

str(fevd(x, type = "Gumbel")) # per vedere la struttura dell'ogetto: lista

# memorizzo le stime MLE (standard methodo, ma ci sono anche altri che potrei usare) dei parametri
Par_Gum <- fevd(x, type = "Gumbel")$results$par # extract par (parameters) from results, and results from object fevd


## Aggiungo al diagramma la distribuzione teorica (Gumbel)
T_teo <- c(1+(1:19)/20, 2:200) # vettore con tempi di ritorno arbitrari (teorico, costruito con tanti valori per avere una curva molto smooth)

# valuto i quanitili per i tempi di ritorno scelti (arbitrari): c'è una funzione specifica: rlevd (=return level extreme value distribution)
q <- rlevd(period = T_teo,
         loc = Par_Gum["location"],
         scale = Par_Gum["scale"],
         type = "Gumbel")

q # named num (vettore con etichette, in totale 200 come stabilito in T_teo)
q[100] # cercando il centessimo elemento/valore, che è 946.2169, a 82 anni
q["100"] # cercando l'etichetta 100, quindi a 100 anni di ritorno = piena secolare, che è 976.0332

# ora voglio creare un rappresentazione graphica di tipo linea, che aggiungo al diagramma gia esistente
lines(T_teo, q, col = "blue", lwd = 2) # aggiungo una nuova serie "linea"

## STEP 5 - CALCOLO LA PORTATA DI PROGETTO QT
# Valuto e rappresento il quantile per T = 100 anni, perchè ci concentriamo sulla piena secolare
Q100_Gum <- q["100"] # salvo il valore di anni 100

# rappresentazione graphica
points(100,Q100_Gum, col = "blue", pch = 19) # aggiungo il punto di 100 anni al diagramma
signif(as.numeric(Q100_Gum), 4) # questo mi da il valore di 100 anni con 4 ciffre significative come lo richiede arpa

## Intervalli di confidenza da pacchetto  extRemes
# Tracciati attraverso ricampionamento Bootstrap
Conf.Int.GUM <- ci(fevd(x, type = "Gumbel"),
                 alpha = 0.05, type = "return.level",
                 return.period = T_teo, R = 500, method = "boot")
# ci = conficence intervals, 5% è una scelta commune di alpha, R è il nr. di campioni sintetici
# mi fa vedere errori perchè io forzo r ad usare gumbel quando r vorrebe usare altre distribuzione (perchè shape non è zero)
# Conf.Int.GUM è una matrice con 3 colonne di valori con 218 valori

Conf.Int.GUM["100-year",] # mi calcola l'estimate per 100 anni: 976.0332
# mi esce il quantile associato al 2.5%: 781.9949, è il quantile di 97.5%: 1152.5465, entro questi numeri mi stanno 95% dei valori

# Rappresento l'intervallo di confidenza, rappresento la 1, 2, e 3 colonna (2.5%, estimate di 100 anni, 97.5%)
lines(T_teo, Conf.Int.GUM[, 1], type = "l", col = "orange", lwd = 1)
lines(T_teo, Conf.Int.GUM[, 2], type = "l", lty = "dashed", col = "orange", lwd = 2)
lines(T_teo, Conf.Int.GUM[, 3], type = "l", col = "orange", lwd = 1)

# Confronto dei quantili centennali
signif(as.numeric(Q100_Gum), 4)
signif(Conf.Int.GUM["100-year",], 4)
# Rapporto
signif(Conf.Int.GUM["100-year",], 4)/signif(as.numeric(Q100_Gum), 4)

# esiste una dipendenza dell'incertezza di stima dal numero di osservazioni disponibili (in teoria: meno osservazioni, fasce di confidenza piu larghi)
# in questo dataset: Reno casalecchio ha il piu grande nr di osservazioni: 74 in totale

# usando Reno Casalecchio le fasce di confidenza sono piu strette, pero la gumbel qui non puo essere accettata, (tanti valori fuori dalle fasce)
# perche il ppcc test ci da un p-value di 4%, e noi accettiamo solo 5%, quindi dobiamo scegliere un altra distribuzione

# usando LAMONE.Grattacoppa come stazione, il ppcc ci da 17% per il gumbel, quindi possiamo proseguire
# visto che sono solo 15 osserazioni, le fasce sono abbastanza ampie, con una certezza di circa 30% (97.5% = 1.3315 = 30%), quindi la incertezza è crescuta

# dobbiamo stare attenti a non sovra- o sotto-parametrizzare, 

# si potrebbe utilizzare un parametro in piu: forma

# ESEMPIO PER LA DISTRIBUZIONE GEV (3 PARAMETRI, PIU' FLESSIBILE, MENO ROBUSTA)
# Intervalli di confidenza dal pacchetto extRemes per la distribuzione GEV
Conf.Int.GEV <- ci(fevd(x, type = "GEV"),
                 alpha = 0.05, type = "return.level",
                 return.period = T_teo, R = 502, method = "boot")
lines(T_teo,Conf.Int.GEV[,1], type = "l", col = "red", lwd = 1)
lines(T_teo,Conf.Int.GEV[,2], type = "l", lty = "dashed", col = "red", lwd = 2)
lines(T_teo,Conf.Int.GEV[,3], type = "l", col = "red", lwd = 1)
# la maggiore flessiblita sembra un vantaggio, pero in realta la rende il modello meno robusto con i limiti di confidenza che escono dal grafico
# quindi i tre parametri non funzionano se ho solo poche osservazioni

# se uso GEV per savio san vittore: ottengo un quantile secolare che ce dentro l'intervallo di confidenza di gumbel 
# è in piu le fasce del GEV sono esplosi anche qui, quindi non ci serve: molta cautela quando si scelge il modello!!

# per analisi locali si usano modelli a due parametri (anche non gumbel, per esempio log-normale etc), 
# se pero invece di una seria sola ho tante (per esempio regionale) posso usare anche modelli piu flessibili (con piu parametri)

# come funziona il metodo bootstrap: costruisce tanti campioni sintetici (con numerosita steassa con la seria, per savio: 42, e che vengono dalla stessa distribuzione campionaria)
# partendo dalla seria originale, replico questa seria R volte, ogni valore c'e R (per esempio 42 per 500) volte, quindi la distr non cambia)
# poi vengono mescolati, e poi divido la seria totale in 500 vettori di 42 valori e ritiro una nuova seria di 42
# in questo nuovo campione potrei aver perso i valori piu alti e aggiunto piu valori centrali, quindi stimo la gumbel e ottengo una osservazione troppo bassa, o troppo alta etc
# ma lo ripeto 500 volte: quindi i risulati troppo estremi non li considero (sotto 2.5 e sopra 97.5) e mi rimane solo quello dentro le fasce di confidenza (95%)

