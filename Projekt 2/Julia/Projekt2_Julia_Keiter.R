setwd("~/Documents/Uni/6. Semester/Fallstudien/Projekt 2")

#benötigte Pakete
library(moments)
library(car)
library(xtable)
install.packages("DAAG")
library(DAAG)

#Einlesung der Daten####
miete <- read.csv("mietspiegel2015.csv", header = T, sep=" ", quote = "\"\"",
               col.names = c("nm", "nmqm", "wfl", "roomiete", "bj", "bez","wohngut",
                             "wohnbest", "ww0", "zh0", "badkach0", "badextra",
                             "kueche"))

#Strukturierung der Variablen
miete$wohngut[miete$wohngut == 1] <- "Gute Lage"
miete$wohngut[miete$wohngut == 0] <- "andere Lagekategorie"

miete$wohnbest[miete$wohnbest == 1] <- "Beste Lage"
miete$wohnbest[miete$wohnbest == 0] <- "andere Lagekategorie"

miete$ww0[miete$ww0 == 1] <- "nein"
miete$ww0[miete$ww0 == 0] <- "ja"

miete$zh0[miete$zh0 == 1] <- "nein"
miete$zh0[miete$zh0 == 0] <- "ja"

miete$badkach0[miete$badkach0 == 1] <- "nicht gefliest"
miete$badkach0[miete$badkach0 == 0] <- "gefliest"

miete$badextra[miete$badextra == 1] <- "gehoben"
miete$badextra[miete$badextra == 0] <- "normal"

miete$kueche[miete$kueche == 1] <- "gehoben"
miete$kueche[miete$kueche == 0] <- "normal"

#Faktorisierung der nominalen Variablen
miete$bez <- as.factor(miete$bez)
miete$wohngut <- as.factor(miete$wohngut)
miete$wohnbest <- as.factor(miete$wohnbest)
miete$ww0 <- as.factor(miete$ww0)
miete$zh0 <- as.factor(miete$zh0)
miete$badkach0 <- as.factor(miete$badkach0)
miete$badextra <- as.factor(miete$badextra)
miete$kueche <- as.factor(miete$kueche)


str(miete)
'data.frame:	3065 obs. of  13 variables:
$ nm      : num  608 780 823 500 595 ...
$ nmqm    : num  12.67 13 7.48 8.62 8.5 ...
$ wfl     : int  48 60 110 58 70 81 97 50 71 76 ...
$ roomiete   : int  2 2 5 2 3 3 3 2 3 3 ...
$ bj      : num  1958 1983 1958 1958 1972 ...
$ bez     : Factor w/ 25 levels "Allach-Untermenzing",..: 25 6 15 20 4 20 8 11 25 25 ...
$ wohngut : Factor w/ 2 levels "0","1": 1 2 1 1 1 1 2 2 1 1 ...
$ wohnbest: Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
$ ww0     : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
$ zh0     : Factor w/ 2 levels "0","1": 1 1 2 1 1 1 1 1 1 2 ...
$ badkach0: Factor w/ 2 levels "0","1": 2 2 2 2 1 2 2 1 2 2 ...
$ badextra: Factor w/ 2 levels "0","1": 1 1 2 1 1 1 2 1 1 2 ...
$ kueche  : Factor w/ 2 levels "0","1": 1 2 1 2 1 1 2 2 1 1 ...'

#Datenqualität####
#insgesamt
sum(is.na(miete))
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

lapply(miete[,1:5], erstell_tabelle)
'$nm
arithm. Mittel         Median        Minimum        Maximum     Spannweite      1.Quartil 
    763.063396     700.000000     174.750000    6000.000000    5825.250000     550.000000 
     3.Quartil            IQR   Standardabw.            MAD        Schiefe        Wölbung 
    910.460000     360.460000     338.162672     261.901290       2.589191      25.474844 

$nmqm
arithm. Mittel         Median        Minimum        Maximum     Spannweite      1.Quartil      3.Quartil            IQR 
   10.73159869    10.84000000     2.47000000    22.13000000    19.66000000     9.03000000    12.45000000     3.42000000 
  Standardabw.            MAD        Schiefe        Wölbung 
    2.67451983     2.50559400     0.04389988     3.33967955 

$wfl
arithm. Mittel         Median        Minimum        Maximum     Spannweite      1.Quartil 
     71.977814      70.000000      15.000000     300.000000     285.000000      55.000000 
     3.Quartil            IQR   Standardabw.            MAD        Schiefe        Wölbung 
     85.000000      30.000000      25.742312      22.239000       1.351719       8.334088 

$roomiete
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
lapply(miete[,6:13], table)
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


#Ausreißer: nm=6000 entfernen
#miete <- subset(miete, nm!=6000.00)


#Modellbildung
mietelm1 <- lm(nm ~ ., data = miete[,-2]) # Regressant: nm, Regressor: alle anderen Variablen

summary(mietelm1)

par(mfrow=c(2,3))
plot(mietelm1, which = 1) #residual vs fitted
plot(mietelm1, which = 2) #q-q-plots
plot(mietelm1, which = 3) #scale-location
plot(mietelm1, which = 4) #cooks distance
plot(mietelm1, which = 5) #leverage
plot(mietelm1, which=6)

#Multikollinearität
#über Determinante
det(t(model.matrix(mietelm1))%*%model.matrix(mietelm1))

#über VIF
DAAG::vif(mietelm1)

#Latex Ausgabe

#Tabelle 1: metrische Variablen 
xtable(caption = "univariate Kenngrößen für metrische Variablen",
       cbind("Nettomiete (€)" = erstell_tabelle(na.omit(miete$nm)),
             "Wohnfläche (qm)" = erstell_tabelle(na.omit(miete$wfl)),
             "Zimmeranzahl" = erstell_tabelle(na.omit(miete$roomiete)),
             "Baujahr" = erstell_tabelle(na.omit(miete$bj))),
       digits = 2)

#Tabelle 2: dichotome  Variablen

xtable(caption = "Deskriptive Kenngrößen für dichotome Variablen",
       cbind("Gute Lage"= c(table(miete$wohngut), table(miete$wohngut)/length(miete$wohngut)),
             "Beste Lage" = c(table(miete$wohnbest), table(miete$wohnbest)/length(miete$wohnbest)),
             "zentrale Warmwasserversorgung" = c(table(miete$ww0), table(miete$ww0)/length(miete$ww0)),
             "Zentralheizung" =c(table(miete$zh0), table(miete$zh0)/length(miete$zh0)),
             "Gefliestes Bad" =c(table(miete$badkach0), table(miete$badkach0)/length(miete$badkach0)),
             "Badausstattung" =c(table(miete$badextra), table(miete$badextra)/length(miete$badextra)),
             "Küchenaustattung" =c(table(miete$kueche), table(miete$kueche)/length(miete$kueche))))

#Abbildung 1: Boxplot Nettomiete
rand <- par(mar = c(20, 4, 2, 2) + 0.2)
boxplot(miete$nm, col = "slategray2",xlab = "Nettomiete in Euro", horizontal = T)
par(rand)


