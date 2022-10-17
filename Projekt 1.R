## Fallstudien I - Projekt 1 ##
## Gruppe 1 ##


## Pakete laden: --------------------------------------------------------------
library(readxl)
library(ggplot2)
library(xtable)
library(moments)


## Laden des Datensatzes: -----------------------------------------------------
khk <- read_excel("KHK_Studie_Demografie.xlsx")


## Strukturierung der Variablen: ----------------------------------------------
khk$sex[khk$sex == 1] <- "männlich"
khk$sex[khk$sex == 2] <- "weiblich"
khk$sex <- as.factor(khk$sex)

khk$myo_infarct[khk$myo_infarct == 1] <- "ja"
khk$myo_infarct[khk$myo_infarct == 2] <- "nein"
khk$myo_infarct <- as.factor(khk$myo_infarct)

khk$gruppe[khk$gruppe == 1] <- "aktiv"
khk$gruppe[khk$gruppe == 2] <- "Placebo"
khk$gruppe <- as.factor(khk$gruppe)

khk$saf[khk$saf == 1] <- "ja"
khk$saf[khk$saf == 0] <- "nein"
khk$saf <- as.factor(khk$saf)

khk$itt[khk$itt == 1] <- "ja"
khk$itt[khk$itt == 0] <- "nein"
khk$itt <- as.factor(khk$itt)

khk$ppa[khk$ppa == 1] <- "ja"
khk$ppa[khk$ppa == 0] <- "nein"
khk$ppa <- as.factor(khk$ppa)


## Erstellung der Unterdatensaetze: -------------------------------------------
pat <- subset(khk, !is.na(khk$patnr))
pat_ak <- subset(pat, pat$gruppe == "aktiv")
pat_pl <- subset(pat, pat$gruppe == "Placebo")


## Betrachtung der Datensaetze: -----------------------------------------------
sum(is.na(khk))
#[1] 86
# Dh. 86 fehlende Werte im Gesamtdatensatz aller gescreenten Patienten

sum(is.na(pat))
#[1] 5
sum(is.na(pat_ak))
#[1] 2
sum(is.na(pat_pl))
#[1] 3

# Dh. 5 fehlende Werte im Datensatz der randomisierten Patienten, wobei 
# 2 Werte aus der aktiven und 3 aus der Placebo-Gruppe fehlen

# in Zeile 179 


## Zusammenfassung des Datensatzes: -------------------------------------------
summary(khk[5:15])
#     sex           groesse         gewicht           gruppe     saf        itt     
# männlich:131   Min.   :146.0   Min.   : 46.00   aktiv  :84   ja  :163   ja  : 95  
# weiblich: 68   1st Qu.:164.0   1st Qu.: 66.50   Placebo:80   nein: 37   nein:105  
# NA's    :  1   Median :169.0   Median : 75.00   NA's   :36                        
#                Mean   :168.9   Mean   : 76.27                                     
#                3rd Qu.:175.0   3rd Qu.: 84.50                                     
#                Max.   :191.0   Max.   :132.00                                     
#                NA's   :1       NA's   :1 
#
#   ppa          alter            bmi          dauer_insuff     myo_infarct
# ja  : 84   Min.   :56.58   Min.   :17.79   Min.   :  0.5667   ja  : 72   
# nein:116   1st Qu.:68.21   1st Qu.:23.88   1st Qu.:  9.0333   nein:127   
#            Median :72.86   Median :25.95   Median : 25.2333   NA's:  1   
#            Mean   :73.00   Mean   :26.69   Mean   : 48.6696              
#            3rd Qu.:77.46   3rd Qu.:28.95   3rd Qu.: 69.6000              
#            Max.   :89.60   Max.   :41.20   Max.   :315.0333              
#            NA's   :1       NA's   :1       NA's   :8  


## Grundlegende Darstellung der Variablen: ------------------------------------
plot(khk$sex)
boxplot(khk$groesse)
boxplot(khk$gewicht)
plot(khk$gruppe)
plot(khk$saf)
plot(khk$itt)
plot(khk$ppa)
boxplot(khk$alter)
boxplot(khk$bmi)
boxplot(khk$dauer_insuff)
plot(khk$myo_infarct)


ggplot(khk, aes(sex)) +
  geom_bar(fill = "lightblue", colour = "black") +
  theme_bw() +
  xlab("Geschlecht") +
  ylab("Anzahl Personen")

ggplot(khk, aes(groesse)) +
  geom_boxplot(fill = "lightblue") +
  theme_bw() +
  xlab("Größe (in cm)") +
  coord_flip()

ggplot(khk, aes(gewicht)) +
  geom_boxplot(fill = "lightblue") +
  theme_bw() +
  xlab("Gewicht (in kg)") +
  coord_flip()

ggplot(khk, aes(gruppe)) +
  geom_bar(fill = "lightblue", colour = "black") +
  theme_bw() +
  xlab("Medikationsgruppe") +
  ylab("Anzahl Personen")

ggplot(khk, aes(alter)) +
  geom_boxplot(fill = "lightblue") +
  theme_bw() +
  xlab("Alter (in Jahren)") +
  coord_flip()

ggplot(khk, aes(bmi)) +
  geom_boxplot(fill = "lightblue") +
  theme_bw() +
  xlab("Body-Mass-Index") +
  coord_flip()

ggplot(khk, aes(dauer_insuff)) +
  geom_boxplot(fill = "lightblue") +
  theme_bw() +
  xlab("Dauer der Herzinsuffizienz (in Monaten)") +
  coord_flip()

ggplot(khk, aes(myo_infarct)) +
  geom_bar(fill = "lightblue", colour = "black") +
  theme_bw() +
  xlab("Herzinfarkt") +
  ylab("Anzahl Personen")


## zu Aufgabe 1(a): -----------------------------------------------------------

# Der Datensatz enthaelt 200 Beobachtungen/gescreente Personen
# mit 15 Variablen

# Skalenniveau:
# -> nominal/kategoriell: (gruppe,) sex, myo_infarct, (saf, itt, ppa)
# -> metrisch: groesse, gewicht, alter, bmi, dauer_insuff


## zu Aufgabe 1(b): -----------------------------------------------------------

# [siehe: Grundlegende Darstellung der Variablen]


## zu Aufgabe 1(c): -----------------------------------------------------------

# [siehe: Zusammenfassung des Datensatzes]

skewness(khk$groesse, na.rm = TRUE)  #Schiefe und ihre verwendete Formel

schiefe <- na.omit(khk$groesse)
((1/length(schiefe)) * sum( (schiefe - mean(schiefe))^3)) / 
  ( (1/length(schiefe)) * sum( (schiefe - mean(schiefe))^2 ) )^(3/2)


kurtosis(khk$groesse, na.rm = TRUE) # Woelbung und ihre verwendete Formel

((1/length(schiefe)) * sum( (schiefe - mean(schiefe))^4)) / 
  ( (1/length(schiefe)) * sum( (schiefe - mean(schiefe))^2 ) )^2




## zu Aufgabe 2(a): -----------------------------------------------------------

# Es gibt 164 randomisierte Patienten, die in die Studie aufgenommen wurden

table(pat$gruppe)
# aktiv Placebo 
#    84      80 


## zu Aufgabe 2(b): Masszahlen -------------------------------------------------

summary(pat[5:15])
#     sex           groesse         gewicht           gruppe     saf        itt    
# männlich:107   Min.   :146.0   Min.   : 46.00   aktiv  :84   ja  :163   ja  :95  
# weiblich: 57   1st Qu.:163.0   1st Qu.: 66.00   Placebo:80   nein:  1   nein:69  
#                Median :170.0   Median : 75.00                                    
#                Mean   :168.8   Mean   : 76.13                                    
#                3rd Qu.:175.0   3rd Qu.: 84.00                                    
#                Max.   :191.0   Max.   :132.00 
# 
# ppa         alter            bmi         dauer_insuff      myo_infarct
# ja  :84   Min.   :56.58   Min.   :17.79   Min.   :  0.5667   ja  : 62   
# nein:80   1st Qu.:68.15   1st Qu.:23.93   1st Qu.:  9.3333   nein:102   
#           Median :73.00   Median :25.79   Median : 25.5667              
#           Mean   :73.19   Mean   :26.65   Mean   : 47.9537              
#           3rd Qu.:77.53   3rd Qu.:28.89   3rd Qu.: 71.0333              
#           Max.   :89.60   Max.   :41.20   Max.   :311.2000              
#                                           NA's   :5        

# vlt noch zusaetzlich eine prozentuale Angabe pro Variable


# aufgeteilt nach Medikationsgruppe:
# aktiv:
summary(pat_ak[5:15])
#     sex          groesse         gewicht        gruppe       saf       itt    
# männlich:51   Min.   :146.0   Min.   : 46.0   aktiv  :84   ja  :83   ja  :50  
# weiblich:33   1st Qu.:161.0   1st Qu.: 64.0   Placebo: 0   nein: 1   nein:34  
#               Median :168.0   Median : 73.0                                   
#               Mean   :167.3   Mean   : 75.4                                   
#               3rd Qu.:173.2   3rd Qu.: 85.0                                   
#               Max.   :191.0   Max.   :132.0 
# 
#   ppa         alter            bmi         dauer_insuff     myo_infarct
# ja  :44   Min.   :56.58   Min.   :18.36   Min.   :  0.900   ja  :30    
# nein:40   1st Qu.:67.85   1st Qu.:23.67   1st Qu.:  8.608   nein:54    
#           Median :72.64   Median :25.79   Median : 21.033              
#           Mean   :72.85   Mean   :26.79   Mean   : 52.291              
#           3rd Qu.:76.92   3rd Qu.:29.14   3rd Qu.: 77.217              
#           Max.   :89.60   Max.   :41.20   Max.   :311.200              
#                                           NA's   :2  

# Placebo:
summary(pat_pl[5:15])
#     sex          groesse         gewicht         gruppe       saf       itt    
# männlich:56   Min.   :150.0   Min.   : 52.00   aktiv  : 0   ja  :80   ja  :45  
# weiblich:24   1st Qu.:166.0   1st Qu.: 68.75   Placebo:80   nein: 0   nein:35  
#               Median :170.0   Median : 75.50                                   
#               Mean   :170.3   Mean   : 76.89                                   
#               3rd Qu.:175.0   3rd Qu.: 82.00                                   
#               Max.   :189.0   Max.   :121.00 
# 
#   ppa         alter            bmi         dauer_insuff      myo_infarct
# ja  :40   Min.   :63.67   Min.   :17.79   Min.   :  0.5667   ja  :32    
# nein:40   1st Qu.:68.22   1st Qu.:24.21   1st Qu.: 10.8667   nein:48    
#           Median :73.31   Median :25.73   Median : 26.8333              
#           Mean   :73.55   Mean   :26.49   Mean   : 43.3342              
#           3rd Qu.:77.94   3rd Qu.:28.67   3rd Qu.: 63.2333              
#           Max.   :86.71   Max.   :36.93   Max.   :152.1000              
#                                           NA's   :3   

# relative Haeufigkeitstabellen:
prop.table(c(aktiv = table(pat_ak$sex), placebo = table(pat_pl$sex)))
# aktiv.männlich   aktiv.weiblich placebo.männlich placebo.weiblich 
#      0.3109756        0.2012195        0.3414634        0.1463415 

prop.table(c(aktiv = table(pat_ak$myo_infarct), 
             placebo = table(pat_pl$myo_infarct)))
#  aktiv.ja   aktiv.nein   placebo.ja placebo.nein 
# 0.1829268    0.3292683    0.1951220    0.2926829 



# Vergleich:
# alles recht aehnlich, einzig auffaellig ist, dass das max von dauer_insuff
# bei der Placebo-Gruppe bei 152.1 und bei der aktiven bei 331.2, also mehr als
# doppelt so hoch ist



## zu Aufgabe 2(b): graphisch -------------------------------------------------
ggplot(pat, aes(gruppe, fill = sex)) +
  geom_bar(position = "dodge") +
  theme_bw() +
  xlab("Medikationsgruppe") +
  ylab("Anzahl Personen")
# geringer Unterschied
# vlt noch zusaetzlich eine prozentuale Angabe

ggplot(pat, aes(gruppe, groesse)) +
  geom_boxplot() +
  theme_bw() +
  xlab("Medikationsgruppe") +
  ylab("Größe (in cm)")
# aehnlich, geringere Streuung bei Placebo

ggplot(pat, aes(gruppe, gewicht)) +
  geom_boxplot() +
  theme_bw() +
  xlab("Medikationsgruppe") +
  ylab("Gewicht (in kg)")
# aehnlich

ggplot(pat, aes(gruppe, alter)) +
  geom_boxplot() +
  theme_bw() +
  xlab("Medikationsgruppe") +
  ylab("Alter (in Jahren)")
# aehnlich, leicht geringere Streuung bei Placebo

ggplot(pat, aes(gruppe, bmi)) +
  geom_boxplot() +
  theme_bw() +
  xlab("Medikationsgruppe") +
  ylab("Body-Mass-Index")
# aehnlich

ggplot(pat, aes(gruppe, dauer_insuff)) +
  geom_boxplot() +
  theme_bw() +
  xlab("Medikationsgruppe") +
  ylab("Dauer der Herzinsuffizienz (in Monaten)")
# aehnlich, bis auf 4 extreme Faelle in der aktiv-Gruppe

ggplot(pat, aes(gruppe, fill = myo_infarct)) +
  geom_bar(position = "dodge") +
  theme_bw() +
  xlab("Medikationsgruppe") +
  ylab("Anzahl Personen")
# aehnlich


# Insgesamt: recht aehnlich, einzig die Dauer der bestehenden Herzinsuffizienz 
#            weist die 4 extremsten Faelle allein in der aktiv-Gruppe auf


## zu Aufgabe 2(c): -----------------------------------------------------------

# ......


## Erstellung von LaTeX-Tabellen: ---------------------------------------------

# Funktion zur Abspeicherung von:
# Median, Interquartilsabstand, 1. und 3. Quartil, Maximum, Minimum, Spannweite
erstell_tabelle <- function(daten){
  speicher <- numeric(7)
  speicher[1] <- median(daten)
  speicher[2] <- IQR(daten)
  speicher[3] <- quantile(daten, 1/4)
  speicher[4] <- quantile(daten, 3/4)
  speicher[5] <- max(daten)
  speicher[6] <- min(daten)
  speicher[7] <- speicher[5] - speicher[6]
  names(speicher) <- c("Median", "Interquartilsabstand",
                       "1.Quartil", "3.Quartil", "Maximum", "Minimum",
                       "Spannweite")
  # arithm. Mittel fehlt noch
  # evtl. noch Modalwert
  return(speicher)
}


# kategoriellen Variablen (gruppe, sex, myo_infarct) aus dem 
# Gesamtdatensatz (khk)
# Tabelle A1:
xtable(table(khk$gruppe), caption = "Medikationsgruppe")

# Tabelle A2:
xtable(table(khk$sex), caption = "Geschlecht")

# Tabelle A3:
xtable(table(khk$myo_infarct), caption = "Herzinfarkt")


# metrischen Variablen (groesse, gewicht, alter, bmi, dauer_insuff) aus dem 
# Gesamtdatensatz (khk)
# Tabelle B1:
xtable(cbind(groesse = erstell_tabelle(na.omit(khk$groesse)), 
             gewicht = erstell_tabelle(na.omit(khk$gewicht)),
             alter = erstell_tabelle(na.omit(khk$alter)),
             bmi = erstell_tabelle(na.omit(khk$bmi)),
             dauerinsuff = erstell_tabelle(na.omit(khk$dauer_insuff))), 
       caption = "aus dem Gesamtdatensatz", 
       digits = 2)


# kategoriellen Variablen (gruppe, sex, myo_infarct) aus dem randomisierten
# Datensatz (pat)
# Tabelle C1:
xtable(table(pat$gruppe), caption = "Medikationsgruppe")

# Tabelle C2:
xtable(table(pat$sex), caption = "Geschlecht")

# Tabelle C3:
xtable(table(pat$myo_infarct), caption = "Herzinfarkt")


# metrische Variablen (groesse, gewicht, alter, bmi, dauer_insuff) aus dem
# randomisierten Datensatz (pat)
# Tabelle D1:
xtable(cbind(groesse = erstell_tabelle(na.omit(pat$groesse)), 
             gewicht = erstell_tabelle(na.omit(pat$gewicht)),
             alter = erstell_tabelle(na.omit(pat$alter)),
             bmi = erstell_tabelle(na.omit(pat$bmi)),
             dauerinsuff = erstell_tabelle(na.omit(pat$dauer_insuff))), 
       caption = "aus dem randomisierten Datensatz", 
       digits = 2)


# kategoriellen Variablen (sex, myo_infarct) aus dem randomisierten
# Datensatz aufgeteilt nach Medikationsgruppen  (pat_ak, pat_pl)
# Tabelle E1:
xtable(cbind(aktiv = table(pat_ak$sex), placebo = table(pat_pl$sex)), 
       caption = "aus dem randomisiertem Datensatz")

# Tabelle E2:
xtable(cbind(aktiv = table(pat_ak$myo_infarct), 
             placebo = table(pat_pl$myo_infarct)), 
       caption = "aus dem randomisiertem Datensatz")


# metrischen Variablen (groesse, gewicht, alter, bmi, dauer_insuff) aus dem 
# randomisierten Datensatz aufgeteilt nach Medikationsgruppen  (pat_ak, pat_pl)
# Tabelle F1:
xtable(cbind(groesseak = erstell_tabelle(na.omit(pat_ak$groesse)),
             groessepl = erstell_tabelle(na.omit(pat_pl$groesse)),
             gewichtak = erstell_tabelle(na.omit(pat_ak$gewicht)),
             gewichtpl = erstell_tabelle(na.omit(pat_pl$gewicht)),
             alterak = erstell_tabelle(na.omit(pat_ak$alter)),
             alterpl = erstell_tabelle(na.omit(pat_pl$alter)),
             bmiak = erstell_tabelle(na.omit(pat_ak$bmi)),
             bmipl = erstell_tabelle(na.omit(pat_pl$bmi)),
             dauerinsuffak = erstell_tabelle(na.omit(pat_ak$dauer_insuff)),
             dauerinsuffpl = erstell_tabelle(na.omit(pat_pl$dauer_insuff))), 
       caption = "aus dem randomisierten Datensatz", 
       digits = 2)



