###Fallstudien 1, Projekt 1, R-Script, Julia Keiter

setwd("~/Documents/Uni/6. Semester/Fallstudien/Projekt 1")
library(readxl)
library(ggplot2)
library(xtable)
library(moments)

#Datenabruf ####
KHK <- read_xlsx("KHK_Studie_Demografie.xlsx")

## Strukturierung der interessierenden dichotomen Variablen: 
KHK$sex[KHK$sex == 1] <- "männlich"
KHK$sex[KHK$sex == 2] <- "weiblich"

KHK$myo_infarct[KHK$myo_infarct == 1] <- "ja"
KHK$myo_infarct[KHK$myo_infarct == 2] <- "nein"

KHK$gruppe[KHK$gruppe == 1] <- "aktiv"
KHK$gruppe[KHK$gruppe == 2] <- "placebo"

#Faktorisieren der nominalen Variablen
KHK$landnr <- as.factor(KHK$landnr)
KHK$zentrum <- as.factor(KHK$zentrum)
KHK$sex <- as.factor(KHK$sex)
KHK$gruppe <- as.factor(KHK$gruppe)
KHK$myo_infarct <- as.factor(KHK$myo_infarct)


str(KHK)
'tibble [200 × 15] (S3: tbl_df/tbl/dataktivframe)
$ landnr      : Factor w/ 1 level "10": 1 1 1 1 1 1 1 1 1 1 ...
$ zentrum     : Factor w/ 26 levels "1","2","4","5",..: 1 1 1 1 1 1 1 1 1 1 ...
$ screennr    : num [1:200] 1 2 3 4 5 6 7 8 9 10 ...
$ patnr       : num [1:200] 5 8 7 NA 6 NA 101 102 103 104 ...
$ sex         : Factor w/ 2 levels "1","2": 2 2 1 1 1 1 1 2 1 1 ...
$ groesse     : num [1:200] 163 166 170 173 172 168 168 163 169 180 ...
$ gewicht     : num [1:200] 64 65 80 92 76 78 77 60 76 80 ...
$ gruppe      : Factor w/ 2 levels "1","2": 2 2 1 NA 1 NA 1 1 2 2 ...
$ saf         : num [1:200] 1 1 1 0 1 0 1 1 1 1 ...
$ itt         : num [1:200] 1 1 1 0 1 0 1 1 1 0 ...
$ ppa         : num [1:200] 1 1 1 0 1 0 0 1 1 0 ...
$ alter       : num [1:200] 71 79.6 75.6 66.6 78 ...
$ bmi         : num [1:200] 24.1 23.6 27.7 30.7 25.7 ...
$ dauer_insuff: num [1:200] 22.2 16.1 111.7 22.4 34.6 ...
$ myo_infarct : Factor w/ 2 levels "1","2": 2 2 1 1 1 1 2 2 2 2 ...'

#Datenqualität####
#insgesamt
sum(is.na(KHK))
#[1] 86

#in wievielen Patienten
sum(rowSums(is.na(airquality))!=0)
#[1] 42

#Messfehler: zentrum=44, screeningnummer=2
pat <- subset(KHK, zentrum==44)
subset(pat, screennr==2)
'# A tibble: 1 × 15
landnr zentrum scree…¹ patnr   sex groesse gewicht gruppe   saf   itt   ppa alter   bmi dauer…² myo_i…³
<dbl>   <dbl>   <dbl> <dbl> <dbl>   <dbl>   <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>
  1     10      44       2    NA    NA      NA      NA     NA     0     0     0    NA    NA      NA      NA
# … with abbreviated variable names ¹screennr, ²dauer_insuff, ³myo_infarct
'

#unter den randomisierten Patienten
KHKrand <- subset(KHK, patnr!="NA")
sum(is.na(KHKrand))
#[1] 5

# Funktion zur Abspeicherung von metrischen Variablen
erstell_tabelle <- function(daten){
  speicher <- numeric(12)
  speicher[1] <- mean(daten) 
  speicher[2] <- median(daten)
  speicher[3] <- min(daten)
  speicher[4] <- max(daten)
  speicher[5] <- speicher[4] - speicher[3]
  speicher[6] <- quantile(daten, 1/4)
  speicher[7] <- quantile(daten, 3/4)
  speicher[8] <- IQR(daten)
  speicher[9] <- sd(daten)
  speicher[10] <- mad(daten)
  speicher[11] <- skewness(daten)
  speicher[12] <- kurtosis(daten)
  names(speicher) <- c("arithm. Mittel", "Median", "Minimum", "Maximum",
                       "Spannweite", "1.Quartil", "3.Quartil", "IQR",
                       "Standardabw.", "MAD","Schiefe", "Wölbung")
  return(speicher)
}

#Verteilung der interessierenden Variablen im Gesamtdatensatz####
#Größe
erstell_tabelle(na.omit(KHK$groesse))
'arithm. Mittel         Median        Minimum        Maximum     Spannweite      1.Quartil      3.Quartil 
168.8592965    169.0000000    146.0000000    191.0000000     45.0000000    164.0000000    175.0000000 
IQR   Standardabw.            MAD        Schiefe        Wölbung 
11.0000000      8.5581966      8.8956000     -0.2415198      3.0052879 '

#Gewicht
erstell_tabelle(na.omit(KHK$gewicht))
'arithm. Mittel         Median        Minimum        Maximum     Spannweite      1.Quartil      3.Quartil 
    76.2663317     75.0000000     46.0000000    132.0000000     86.0000000     66.5000000     84.5000000 
           IQR   Standardabw.            MAD        Schiefe        Wölbung 
    18.0000000     13.7533989     13.3434000      0.7354812      4.2823971 '

#Alter
erstell_tabelle(na.omit(KHK$alter))
'arithm. Mittel         Median        Minimum        Maximum     Spannweite      1.Quartil      3.Quartil 
   72.99818738    72.85968515    56.57768652    89.59890486    33.02121834    68.20670773    77.46475017 
           IQR   Standardabw.            MAD        Schiefe        Wölbung 
    9.25804244     6.19842213     6.92082957     0.05283509     2.81942605 '

#BMI
erstell_tabelle(na.omit(KHK$bmi))
'arithm. Mittel         Median        Minimum        Maximum     Spannweite      1.Quartil      3.Quartil 
    26.6926662     25.9515571     17.7897069     41.1972161     23.4075091     23.8752737     28.9481419 
           IQR   Standardabw.            MAD        Schiefe        Wölbung 
     5.0728683      4.0541759      3.6308739      0.7142287      3.4643458 '

#Dauer
erstell_tabelle(na.omit(KHK$dauer_insuff))
'arithm. Mittel         Median        Minimum        Maximum     Spannweite      1.Quartil      3.Quartil 
    48.6696181     25.2333333      0.5666667    315.0333333    314.4666667      9.0333333     69.6000000 
           IQR   Standardabw.            MAD        Schiefe        Wölbung 
    60.5666667     57.4510807     31.2087300      2.2966432      9.6143832 '


#Randomisierung in Medikamentengruppen####
aktiv <- subset(KHKrand, gruppe=="aktiv")
placebo <- subset(KHKrand, gruppe=="placebo")


#Verteilung der interessierenden Variablen nach Randomisierung####
#Größe
erstell_tabelle(aktiv$groesse)
'arithm. Mittel         Median        Minimum        Maximum     Spannweite 
   167.3095238    168.0000000    146.0000000    191.0000000     45.0000000 
     1.Quartil      3.Quartil            IQR   Standardabw.            MAD 
   161.0000000    173.2500000     12.2500000      9.7696064     10.3782000 
       Schiefe        Wölbung 
    -0.1439784      2.5719246 '

#Gewicht
erstell_tabelle(aktiv$gewicht)
'arithm. Mittel         Median        Minimum        Maximum     Spannweite 
    75.4047619     73.0000000     46.0000000    132.0000000     86.0000000 
     1.Quartil      3.Quartil            IQR   Standardabw.            MAD 
    64.0000000     85.0000000     21.0000000     16.0482098     14.8260000 
       Schiefe        Wölbung 
     0.8152484      3.9782228'

#Alter
erstell_tabelle(aktiv$alter)
'arithm. Mittel         Median        Minimum        Maximum     Spannweite 
   72.85248199    72.63928816    56.57768652    89.59890486    33.02121834 
     1.Quartil      3.Quartil            IQR   Standardabw.            MAD 
   67.85010267    76.91512663     9.06502396     6.33286467     7.02230801 
       Schiefe        Wölbung 
    0.03621685     2.98401344 '

#BMI
erstell_tabelle(aktiv$bmi)
'arithm. Mittel         Median        Minimum        Maximum     Spannweite 
    26.7928729     25.7883556     18.3593750     41.1972161     22.8378411 
     1.Quartil      3.Quartil            IQR   Standardabw.            MAD 
    23.6701678     29.1430110      5.4728433      4.3746408      3.9353306 
       Schiefe        Wölbung 
     0.7456754      3.4406941 '

#Dauer
erstell_tabelle(na.omit(aktiv$dauer_insuff))
'arithm. Mittel         Median        Minimum        Maximum     Spannweite 
     52.291463      21.033333       0.900000     311.200000     310.300000 
     1.Quartil      3.Quartil            IQR   Standardabw.            MAD 
      8.608333      77.216667      68.608333      65.758670      26.316150 
       Schiefe        Wölbung 
      2.094775       7.668345 '


########placebo
#Größe
erstell_tabelle(placebo$groesse)
'arithm. Mittel         Median        Minimum        Maximum     Spannweite 
76.887500      75.500000      52.000000     121.000000      69.000000 
1.Quartil      3.Quartil            IQR   Standardabw.            MAD 
68.750000      82.000000      13.250000      11.586760       9.636900 
Schiefe        Wölbung 
1.009238       5.115358'

#Gewicht
erstell_tabelle(placebo$gewicht)
'arithm. Mittel         Median        Minimum        Maximum     Spannweite 
     76.887500      75.500000      52.000000     121.000000      69.000000 
     1.Quartil      3.Quartil            IQR   Standardabw.            MAD 
     68.750000      82.000000      13.250000      11.586760       9.636900 
       Schiefe        Wölbung 
      1.009238       5.115358 '

#Alter
erstell_tabelle(placebo$alter)
'arithm. Mittel         Median        Minimum        Maximum     Spannweite 
    73.5456194     73.3086927     63.6741958     86.7132101     23.0390144 
     1.Quartil      3.Quartil            IQR   Standardabw.            MAD 
    68.2183436     77.9356605      9.7173169      6.0396982      7.2171466 
       Schiefe        Wölbung 
     0.2689228      2.2856380 '

#BMI
erstell_tabelle(placebo$bmi)
'arithm. Mittel         Median        Minimum        Maximum     Spannweite 
26.4946473     25.7308806     17.7897069     36.9341595     19.1444526 
1.Quartil      3.Quartil            IQR   Standardabw.            MAD 
24.2078806     28.6729555      4.4650749      3.4650266      2.7511228 
Schiefe        Wölbung 
0.6659965      3.8188236 '

#Dauer
erstell_tabelle(na.omit(placebo$dauer_insuff))
'arithm. Mittel         Median        Minimum        Maximum     Spannweite 
    43.3341991     26.8333333      0.5666667    152.1000000    151.5333333 
     1.Quartil      3.Quartil            IQR   Standardabw.            MAD 
    10.8666667     63.2333333     52.3666667     39.7122309     30.5909800 
       Schiefe        Wölbung 
     1.0593075      3.2433689 '






#Latex-Ausgabe#########

#Tabellen####

#dichotome interessierende Variablen

#Geschlecht
#Tabelle 1
xtable(caption = "Deskriptive Kenngrößen für Variable Geschlecht",
       cbind(Gesamt= c(table(KHK$sex),sum(is.na(KHK$sex)), table(KHK$sex)/length(KHK$sex),
                       sum(is.na(KHK$sex))/length(KHK$sex)),
             Randomisiert = c(table(KHKrand$sex),sum(is.na(KHKrand$sex)), table(KHKrand$sex)/length(KHKrand$sex),
                             sum(is.na(KHKrand$sex))/length(KHKrand$sex)),
             Aktiv = c(table(aktiv$sex),sum(is.na(aktiv$sex)), table(aktiv$sex)/length(aktiv$sex),
                       sum(is.na(aktiv$sex))/length(aktiv$sex)),
             Placebo =c(table(placebo$sex),sum(is.na(placebo$sex)), table(placebo$sex)/length(placebo$sex),
                       sum(is.na(placebo$sex))/length(placebo$sex))))

#Infarkt
#Tabelle 2
xtable(caption = "Deskriptive Kenngrößen für Variable Infarkt",
       cbind(Gesamt= c(table(KHK$myo_infarct),sum(is.na(KHK$myo_infarct)), table(KHK$myo_infarct)/length(KHK$myo_infarct),
                       sum(is.na(KHK$myo_infarct))/length(KHK$myo_infarct)),
             Randomisiert = c(table(KHKrand$myo_infarct),sum(is.na(KHKrand$myo_infarct)), table(KHKrand$myo_infarct)/length(KHKrand$myo_infarct),
                              sum(is.na(KHKrand$myo_infarct))/length(KHKrand$myo_infarct)),
             Aktiv = c(table(aktiv$myo_infarct),sum(is.na(aktiv$myo_infarct)), table(aktiv$myo_infarct)/length(aktiv$myo_infarct),
                       sum(is.na(aktiv$myo_infarct))/length(aktiv$myo_infarct)),
             Placebo =c(table(placebo$myo_infarct),sum(is.na(placebo$myo_infarct)), table(placebo$myo_infarct)/length(placebo$myo_infarct),
                        sum(is.na(placebo$myo_infarct))/length(placebo$myo_infarct))))

#metrische interessierende Variablen im Gesamtdatensatz
# Tabelle 4:
xtable(caption = "univariate Kenngrößen für metrische Variablen aus Gesamtdatensatz",
       cbind("Größe" = erstell_tabelle(na.omit(KHK$groesse)),
             "Gewicht" = erstell_tabelle(na.omit(KHK$gewicht)),
            "BMI" = erstell_tabelle(na.omit(KHK$bmi)),
          "Alter" = erstell_tabelle(na.omit(KHK$alter)),
             "Dauer" = erstell_tabelle(na.omit(KHK$dauer_insuff))),
       digits = 2)

#metrische interessierende Variablen im randomisierten Datensatz
#Tabelle 5:
xtable(caption = "univariate Kenngrößen für metrische Variablen aus randomisierten Datensatz",
       cbind("Größe (a.)" = erstell_tabelle(na.omit(aktiv$groesse)),
             "Größe (p.)" = erstell_tabelle(na.omit(placebo$groesse)),
             "Gewicht (a.)" = erstell_tabelle(na.omit(aktiv$gewicht)),
             "Gewicht (p.)"= erstell_tabelle(na.omit(placebo$gewicht)),
             "BMI (a.)" = erstell_tabelle(na.omit(aktiv$bmi)),
             "BMI (p.)" = erstell_tabelle(na.omit(placebo$bmi)),
             "Alter (a.)" = erstell_tabelle(na.omit(aktiv$alter)),
             "Alter (p.)" = erstell_tabelle(na.omit(placebo$alter)),
             "Dauer (a.)" = erstell_tabelle(na.omit(aktiv$dauer_insuff)),
             "Dauer (p.)" = erstell_tabelle(na.omit(placebo$dauer_insuff))),
       digits = 2)


#Grafiken####

#Gesamtdatensatz

#Histogramme
## Funktion die ein Histogramm erstellt und eine Normalverteilung drueber
## legt. Die Normalverteilung hat dabei den Mittelwert der Variable als
## mu und die Varainz/Standardabweichung als sigma^2 bzw. sigmaktiv

#im Gesamtdatensatz
comp_norm_g <- function(y, xlab, ...){
  x <- y
  hist(y, freq = FALSE, ylab = "Dichte", xlab = xlab, main = " ", cex.lab=1.4, cex.axis=1.3,...)
  curve(dnorm(x, mean= mean(y, na.rm = TRUE), sd= sd(y, na.rm = TRUE)), 
        col="darkblue", lwd=3.5, add=TRUE, yaxt="n")
  mea <- round(mean(y))
  var <- round(var(y), digits = 2)
  legend("topright", 
         legend = expr(paste(italic(N), group( "(", list(!!mea, !!var), ")" ) ) ),
         col = "darkblue", lty = 1, lwd = 3.5)
}

par(mfrow = c(1,1))

#Alter Abbildung 1
comp_norm_g(KHK$alter, "Alter in Jahren", col="slategray2" ,ylim=c(0,0.07), 
            xlim=c(50,100))

#Dauer Abbildung 2
comp_norm_g(na.omit(KHK$dauer_insuff), "Alter in Jahren", col="slategray2", 
            xlim=c(0,400), breaks=15)


#Randomisierter Datensatz 

#barplots
#zwei Plots in einer Grafik
par(mfrow = c(1,2))
# Rand vergroessern
rand <- par(mar = c(2, 4, 5, 2) + 0.2)

#Infarkt Abbildung 3
######aktiv
barplot(table(aktiv$myo_infarct)/length(aktiv$myo_infarct), ylim = c(0,1), beside = T, 
        names.arg = c("Infarkt", "Kein Infarkt"), ylab = "relative Häufigkeit",
        col = c("slategray3", "slategray4"), main="aktiv", cex.lab=1.4, cex.axis=1.3,
        cex.names = 0.9)
######placebo
barplot(table(placebo$myo_infarct)/length(placebo$myo_infarct), ylim = c(0,1), beside = T, 
        names.arg = c("Infarkt", "Kein Infarkt"),ylab = "relative Häufigkeit",
        col = c("slategray3", "slategray4"), main="placebo", cex.lab=1.4, cex.axis=1.3,
        cex.names = 0.9)

#Histogramme 
rand <- par(mar = c(5, 3.8, 7, 2) + 0.2)

#im randomiserten Datensatz
#für Vergleich
comp_norm_v <- function(y, xlab, ...){
  x <- y
  hist(y, freq = FALSE, ylab = "Dichte", xlab = xlab, cex.lab=1, cex.axis=0.7,...)
  curve(dnorm(x, mean= mean(y, na.rm = TRUE), sd= sd(y, na.rm = TRUE)), 
        col="darkblue", lwd=3.5, add=TRUE, yaxt="n")
  mea <- round(mean(y))
  var <- round(var(y), digits = 2)
  legend("topright", 
         legend = expr(paste(italic(N), group( "(", list(!!mea, !!var), ")" ) ) ),
         col = "darkblue", lty = 1, lwd = 3.5, cex=0.5)
}

#Größe Abbildung 4
######aktiv
comp_norm_v(aktiv$groesse, "Größe in cm",main="aktiv", col="slategray2", 
          ylim=c(0,0.07), xlim=c(140,200))
######placebo
comp_norm_v(placebo$groesse, "Größe in cm", main="placebo", col="slategray2",
          ylim=c(0,0.07), xlim=c(140,200))

#Boxplots
par(mfrow = c(1,1))
rand <- par(mar = c(5, 4, 2, 2) + 0.2)

#BMI Abbildung 5
boxplot(aktiv$bmi, placebo$bmi, col = "slategray2", names = c("aktiv", "placebo"),
        xlab = expression(BMI == frac(kg , m^2)), ylab = "Medikamentengruppe", 
        cex.lab=1.4, cex.axis=1.3, horizontal = T)
       
#Dauer Abbildung 6
boxplot(aktiv$dauer_insuff, placebo$dauer_insuff, col = "slategray2", 
        names = c("aktiv", "placebo"), xlab = "Dauer in Monaten", 
        ylab = "Medikamentengruppe", cex.lab=1.4, cex.axis=1.3, horizontal = T)

#empirische Verteilungsfunktion
#Dauer Abbildung 7
########aktiv
Fa <- ecdf(aktiv$dauer_insuff)
plot(Fa, lty=1, col="slategray3", main="", ylab = "Summenhäufigkeit",
     xlab="Dauer in Monaten", cex.lab=1.4, cex.axis=1.3)
#######placebo
Fp <- ecdf(placebo$dauer_insuff)
plot(Fp, lty=1, col="slategray4", add=T)
legend("right", legend=c("aktiv", 
                         "placebo"), 
       fill = c("slategray3", "slategray4"), cex = 1)
