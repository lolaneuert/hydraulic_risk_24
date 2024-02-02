################################################################################
### Stima di danni economici:    
# applicazione di una curva di vulnerabilità di letteratura

# Carico i pacchetti necessari (che devo aver precedentemente installato)

# install.packages("terra")
library("terra")
# Shapefile contenente i dati di danno
Dati.Danno <- vect("es_stima_danno_geo/shapefile/Dati_danno.shp")
Dati.Danno                # dataset spaziale (troncato)
summary(Dati.Danno)       # sintesi dataset spaziale (completa)
geom(Dati.Danno)          # Coordinate geografiche per ogni punto, x e y
Dati.Danno$str_damage     # Campo del dataset (un vettore in R)
# Campi della tabella degli attributi
# "lon"          Longitudine (esadecimale)
# "lat"          Latitudine (esadecimale)
# "Municipali"   Comune di riferimento
# "POINT_X"      Longitudine UTM32N (m)
# "POINT_Y"      Latitudine UTM32N (m)
# "ID_1"         Identificativo alfanumerico
# "str_type"     Tipo di costruzione
# "area"         Area impronta in pianta (m^2)
# "econ_val_p"   Valore economico di riferimento (per m^2)
# "econ_val_t"   Valore economico complessivo (area*econ_val_p)
# "str_damage"   Danno osservato (Euro)
# "str_dama_1"   Danno percentuale (%)

# Carico il raster dei tiranti da modello 2D (altezza d'acqua max in m)
r <- rast("es_stima_danno_GEO/raster/max_tirante_10m_cm.asc") # contiene il tirante massimo in un raster di 10m
r                # struttura raster
plot(r, main = "Raster dei tiranti massimi con edifici sovrapposti")                              # rappresentazione del raster
points(Dati.Danno, pch = 19, cex = 0.2)  # sovrapposizione edifici

## (1) Si valuti il danno economico complessivo in milioni di Euro desumibile 
#     sulla base delle richieste di risarcimento realmente censite per gli 
#     edifici di interesse
signif(sum(Dati.Danno$str_damage)/1.0E+06, digits = 3)
# RISULTATO 14.7 milioni di euro

## (2) si confronti la valutazione del danno complessivo con la stima derivante
#     dall'applicazione di una curva di vulnerabilità di letteratura

# Tabella curva tirante-danno (JRC Other Countries) (perchè non c'è per l'italia quindi usiamo la media degli altri paesi)
# https://ec.europa.eu/jrc/en/publication/global-flood-depth-damage-functions-methodology-and-database-guidelines
JRC <- read.table("es_stima_danno_GEO/JRC_OC_curva_danno.csv", header = TRUE, sep = ",")
plot(JRC, main = "Global flood-depth damage functions", xlab = "Tirante massimo (m)", ylab = "Danno relativo (%)") 
# per vedere la curva tra tirante massimo e vulnerabilità (damages %)
# Estrazione dati di tirante dal raster della simulazione del modello
Dati.Danno$tirante <- extract(r, Dati.Danno)[, 2] 
# "extract" è una funzione di "terra", la 2a colonna contiene il dato estratto
summary(Dati.Danno) # sintesi del dataset completo

# Stima del danno complessivo con curva di letteratura 
# (attraverso interpolazione lineare)
Dati.Danno$Danno.JRC<-Dati.Danno$econ_val_t*approx(x = JRC$max_w_depth,
                                                   y = JRC$str_damages_perc,
                                                   xout = Dati.Danno$tirante)$y
# sto creando una nuova colonna nella quale metto la colonna di economic value (valore economico complessivo dell'edificio) da dati.danno
# e lo interpolisco linearmente con la vulnerabilità (%) con approx (usando la table di JRC)
# xout sono i valori del tirante per cui io voglio la stima della vulnerabilità, il $y serve per vedere la colonna di y della xout

# Danno complessivo stimato in milioni di euro
signif(sum(Dati.Danno$Danno.JRC)/1.0E+6, digits = 3)
# RISULTATO 52.3 milioni di euro

# Diagramma Tirante vs. Danno relativo
plot(Dati.Danno$tirante,Dati.Danno$str_dama_1, main = "Tirante vs. Danno relativo", 
     sub = "con la curva JRC in arancione e la curva del modello in blu",
     xlab = "Tirante (m)", ylab = "Danno relativo (%)")
# Sovrapposizione della curva JRC al diagramma tirante vs. Danno relativo
lines(x = JRC$max_w_depth, y = JRC$str_damages_perc*100, col = "orange", lwd = 2)

## (3) si predisponga un modello di danno semplificato mediante 
#     regressione quadratica, per quello uso sqrt
# Costruzione modello danno ("lm" linear model, con radice di danno, no intercetta) 
Modello <- lm(formula = Dati.Danno$str_dama_1 ~ 0 + sqrt(Dati.Danno$tirante))
# mette in relazione il danno relativo con la radice quadrata del tirante, ~0 forza la relazione a passare per 0 (quindi nessun'intercetta)


# ATTENZIONE: IL MODELLO VALUTA IL DANNO RELATIVO IN %
lines(x = JRC$max_w_depth,y = Modello$coefficients[1]*sqrt(JRC$max_w_depth), 
      col = "blue", lwd = 2)
# per comodità metto la linea della relazione del modello nel grafico del JRC 

# Calcolo il danno edificio per edificio in Euro (divido per 100 perchè sono % e poi lo moltiplico con i valori economici)
Dati.Danno$Danno.Modello <- Modello$fitted.values/100*Dati.Danno$econ_val_t # in una nuova colonna

# Danno complessivo stimato in milioni di Euro
signif(sum(Dati.Danno$Danno.Modello)/1.0E+6,digits = 3)
# RISULTATO: 14.7 milioni di euro

# IMPLICAZIONE: il modello riproduce il danno complessivo
# quanto è impreciso il mio modello? 
# Errore quadratico medio per singolo edificio danneggiato usando i residui
signif(sqrt(1/length(Modello$residuals)*sum(Modello$residuals^2)), digits = 3)
# RISULTATO: 12.5%, quindi su una scala di 100% sbaglio 12.5% in media, per ogni edifico (scala globale) -> errore importante
# IMPLICAZIONE: a livello di edificio il danno è tutt'altro che trascurabile
# se il modello empirico è costruito sulla mia realtà (non per altre zone) i risultati dovrebbero essere molto piu precisi
# ma comunque anche se non esiste un dataset/modello locale, in termini relative posso nonostante usare dataset di altre zone, per esempio vedere se un serbatoio può aiutare etc
# l'applicazione del modello per singoli edifici non ha senso fin'ora, ma puo ben'essere usato per communità etc

## (4) si valuti l’attendibilita' del modello per gruppi omogenei di edifici 
#     interessati (es. stessa tipologia costruttiva, stesso comune, ecc.)
#     a) il Modello applicato ad una sola tipologia costruttiva non riproduce 
#        il danno complessivo
unique(Dati.Danno$str_type) # tipologie costruttive, unique guarda i casi all'interno del vettore e tira fuori quelli tipologie diverse
length(Dati.Danno[which(Dati.Danno$str_type == "muratura"),]) # casi "muratura" in totale sono 865 casi che riportono questo tipo
Dati.Danno.Tipo <- Dati.Danno[which(Dati.Danno$str_type == "cemento armato"),] # crea una nuova colonna con il tipo cemento armato
length(Dati.Danno.Tipo) # 170 richieste di risarcimento per edifici in c.a.
signif(sum(Dati.Danno.Tipo$Danno.Modello/1.0E+06), digits = 3)
# Modello: 1.82 milioni di euro per questa tipologia di edifici (quelli cemento armato)
signif(sum(Dati.Danno.Tipo$str_damage/1.0E+06), digits = 3)
# Richieste: 1.34 milioni di euro -> questo è una sovrastima, ha da fare con il dataset che abbiamo usato, il modo nel quale l'abbiamo costruito, 
# perchè le case cemento armato sono molto piu resistenti del resto degli edifici, che comunque rappresentano la maggiorparte delle case nel modello, quindi sovrastimo i danni (perchè la media e piu vulnerabile che questo tipo di costruzione)
#     b) ecc. (si potrebbe continuare, vedere se la stima è giusta per esempio per un comune specifico etc)

unique(Dati.Danno$Municipali) # per vedere i diverse municipi
length(Dati.Danno[which(Dati.Danno$Municipali == "Bomporto"),])# Nel comune di Bomporto in totale sono 343
length(Dati.Danno[which(Dati.Danno$Municipali == "Bastiglia"),])# Nel comune di Bastiglia in totale sono 740
length(Dati.Danno[which(Dati.Danno$Municipali == "Modena"),])# Nel comune di modena in totale sono 43

Dati.Danno.Municipio <- Dati.Danno[which(Dati.Danno$Municipali == "Bomporto"),] # crea una nuova colonna con solo Bomporto
signif(sum(Dati.Danno.Municipio$Danno.Modello/1.0E+06), digits = 3) 
# solo per il municipio di Bomporto il Modello calcola 5.1 milioni di euro
signif(sum(Dati.Danno.Municipio$str_damage/1.0E+06), digits = 3)
# invece le richieste sono di solo 4.08 milioni -> di nuovo una sovrastima
# se si calcola la stessa cosa per Bastiglia i risultati sono di 8.84 mio di danni per il modello e 9.66 di richieste in realtà
# se si calcola la stessa cosa per Modena i risultati sono di 795.000€ per il modello e 992.000€ di richieste in realtà


# Salvataggio SHP file completo 
writeVector(Dati.Danno, "es_stima_danno_GEO/shapefile/Dati_danno_Modello.shp", overwrite = TRUE)
