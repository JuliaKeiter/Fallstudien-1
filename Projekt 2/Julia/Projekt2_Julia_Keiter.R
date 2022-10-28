setwd("~/Documents/Uni/6. Semester/Fallstudien/Projekt 2")

#benötigte Pakete
library(moments)
library(car)

#Einlesung der Daten####
MS <- read.csv("mietspiegel2015.csv", header = T, sep=" ", quote = "\"\"",
               col.names = c("nm", "nmqm", "wfl", "rooms", "bj", "bez","wohngut",
                             "wohnbest", "ww0", "zh0", "badkach0", "badextra",
                             "kueche"))

#Gruppe 1: Betrachtung der nm, nmqm fällt weg
MS <- MS[,-2]

#Strukturierung der Variablen
MS$wohngut[MS$wohngut == 1] <- "Gute Lage"
MS$wohngut[MS$wohngut == 0] <- "andere Lagekategorie"

MS$wohnbest[MS$wohnbest == 1] <- "Beste Lage"
MS$wohnbest[MS$wohnbest == 0] <- "andere Lagekategorie"

MS$ww0[MS$ww0 == 1] <- "nein"
MS$ww0[MS$ww0 == 0] <- "ja"

MS$zh0[MS$zh0 == 1] <- "nein"
MS$zh0[MS$zh0 == 0] <- "ja"

MS$badkach0[MS$badkach0 == 1] <- "nicht gefliest"
MS$badkach0[MS$badkach0 == 0] <- "gefliest"

MS$badextra[MS$badextra == 1] <- "gehoben"
MS$badextra[MS$badextra == 0] <- "normal"

MS$kueche[MS$kueche == 1] <- "gehoben"
MS$kueche[MS$kueche == 0] <- "normal"

#Faktorisierung der nominalen Variablen
MS$bez <- as.factor(MS$bez)
MS$wohngut <- as.factor(MS$wohngut)
MS$wohnbest <- as.factor(MS$wohnbest)
MS$ww0 <- as.factor(MS$ww0)
MS$zh0 <- as.factor(MS$zh0)
MS$badkach0 <- as.factor(MS$badkach0)
MS$badextra <- as.factor(MS$badextra)
MS$kueche <- as.factor(MS$kueche)


str(MS)
'data.frame:	3065 obs. of  12 variables:
 $ nm      : num  608 780 823 500 595 ...
 $ wfl     : int  48 60 110 58 70 81 97 50 71 76 ...
 $ rooms   : int  2 2 5 2 3 3 3 2 3 3 ...
 $ bj      : num  1958 1983 1958 1958 1972 ...
 $ bez     : Factor w/ 25 levels "Allach-Untermenzing",..: 25 6 15 20 4 20 8 11 25 25 ...
 $ wohngut : Factor w/ 2 levels "andere Lagekategorie",..: 1 2 1 1 1 1 2 2 1 1 ...
 $ wohnbest: Factor w/ 2 levels "andere Lagekategorie",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ ww0     : Factor w/ 2 levels "ja","nein": 1 1 1 1 1 1 1 1 1 1 ...
 $ zh0     : Factor w/ 2 levels "ja","nein": 1 1 2 1 1 1 1 1 1 2 ...
 $ badkach0: Factor w/ 2 levels "gefliest","nicht gefliest": 2 2 2 2 1 2 2 1 2 2 ...
 $ badextra: Factor w/ 2 levels "gehoben","normal": 2 2 1 2 2 2 1 2 2 1 ...
 $ kueche  : Factor w/ 2 levels "gehoben","normal": 2 1 2 1 2 2 1 1 2 2 ...'

#Datenqualität####
#insgesamt
sum(is.na(MS))
#[1] 0

#Betrachtung Variablenverteilung
#metrische Variablen
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

lapply(MS[,1:4], erstell_tabelle)
'$nm
arithm. Mittel         Median        Minimum        Maximum     Spannweite      1.Quartil 
    763.063396     700.000000     174.750000    6000.000000    5825.250000     550.000000 
     3.Quartil            IQR   Standardabw.            MAD        Schiefe        Wölbung 
    910.460000     360.460000     338.162672     261.901290       2.589191      25.474844 

$wfl
arithm. Mittel         Median        Minimum        Maximum     Spannweite      1.Quartil 
     71.977814      70.000000      15.000000     300.000000     285.000000      55.000000 
     3.Quartil            IQR   Standardabw.            MAD        Schiefe        Wölbung 
     85.000000      30.000000      25.742312      22.239000       1.351719       8.334088 

$rooms
arithm. Mittel         Median        Minimum        Maximum     Spannweite      1.Quartil 
     2.7014682      3.0000000      1.0000000      8.0000000      7.0000000      2.0000000 
     3.Quartil            IQR   Standardabw.            MAD        Schiefe        Wölbung 
     3.0000000      1.0000000      0.9788633      1.4826000      0.4600509      3.5977241 

$bj
arithm. Mittel         Median        Minimum        Maximum     Spannweite      1.Quartil 
  1964.2122349   1957.5000000   1918.0000000   2012.5000000     94.5000000   1957.5000000 
     3.Quartil            IQR   Standardabw.            MAD        Schiefe        Wölbung 
  1983.0000000     25.5000000     26.5054412     27.4281000     -0.1823192      2.3144530 '

#nominale Variablen
lapply(MS[,6:12], table)
'$bez
        Allach-Untermenzing              Altstadt-Lehel               Au-Haidhausen 
                         25                          47                         167 
                  Aubing...                Berg am Laim                 Bogenhausen 
                         56                         101                         159 
    Fledmoching-Hasenbergel                      Hadern                        Laim 
                         78                          79                         100 
Ludwigvorstadt-Isarvorstadt                 Maxvorstadt       Milbersthofen-Am Hart 
                        154                         168                         134 
                    Moosach       Neuhausen-Nymphenburg                 Obergiesing 
                         92                         234                         145 
         Pasing-Obermenzing          Ramersdorf-Perlach              Schwabing West 
                        118                         181                         165 
         Schwabing-Freimann            Schwanthalerhöhe                    Sendling 
                        140                          87                         126 
          Sendling-Westpark              Thalkirchen...              Trudering-Riem 
                        122                         175                          81 
               Untergiesing 
                        131 

$wohngut
andere Lagekategorie            Gute Lage 
                1980                 1085 

$wohnbest
andere Lagekategorie           Beste Lage 
                2955                  110 

$ww0
  ja nein 
3039   26 

$zh0
  ja nein 
2861  204 

$badkach0
      gefliest nicht gefliest 
           380           2685 

$badextra
gehoben  normal 
    361    2704 

$kueche
gehoben  normal 
    767    2298'

#erster Blick auf mögliche Abhängigkeiten von nm zu anderen Variablen
MS.lm <- lm(nm ~ ., data = MS) # Regressant: nm, Regressor: alle anderen Variablen

carPalette()

scatterplotMatrix(~ . | nm, data=MS, col="slategray3")
'Warning message:
In scatterplotMatrix.default(X[, -ncol], groups = X[, ncol], ...) :
  number of groups exceeds number of available colors
colors are recycled' 
#funktioniert trotzdem
