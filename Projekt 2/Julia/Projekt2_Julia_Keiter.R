setwd("~/Documents/Uni/6. Semester/Fallstudien/Projekt 2")

#benötigte Pakete
library(moments)
library(car)
library(broom)
library(xtable)
library(klaR)
library(glmnet)
library(olsrr)
library(UsingR)
library(corrplot)
library(Metrics)

#Einlesung der Daten####
miete <- read.csv("mietspiegel2015.csv", header = T, sep=" ", quote = "\"\"",
               col.names = c("nm", "nmqm", "wfl", "roomiete", "bj", "bez","wohngut",
                             "wohnbest", "ww0", "zh0", "badkach0", "badextra",
                             "kueche"))
str(miete)
'data.frame:	3065 obs. of  13 variables:
 $ nm      : num  608 780 823 500 595 ...
 $ nmqm    : num  12.67 13 7.48 8.62 8.5 ...
 $ wfl     : int  48 60 110 58 70 81 97 50 71 76 ...
 $ roomiete: int  2 2 5 2 3 3 3 2 3 3 ...
 $ bj      : num  1958 1983 1958 1958 1972 ...
 $ bez     : chr  "Untergiesing" "Bogenhausen" "Obergiesing" "Schwanthalerhöhe" ...
 $ wohngut : int  0 1 0 0 0 0 1 1 0 0 ...
 $ wohnbest: int  0 0 0 0 0 0 0 0 0 0 ...
 $ ww0     : int  0 0 0 0 0 0 0 0 0 0 ...
 $ zh0     : int  0 0 1 0 0 0 0 0 0 1 ...
 $ badkach0: int  1 1 1 1 0 1 1 0 1 1 ...
 $ badextra: int  0 0 1 0 0 0 1 0 0 1 ...
 $ kueche  : int  0 1 0 1 0 0 1 1 0 0 ...'

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

#Betrachtung Variablenverteilung####
#metrische Variablen

#Funktion zur Abspeicherung von metrischen Variablen
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

#Modellbildung####

#volles Modell
mietelm1 <- lm(nm ~ ., data = miete[,-c(2)]) # Regressant: nm, Regressor: alle anderen Variablen
summary(mietelm1)
'
Call:
lm(formula = nm ~ ., data = miete[, -c(2)])

Residuals:
    Min      1Q  Median      3Q     Max 
-975.11  -95.77    4.80   93.97 2454.63 

Coefficients:
                                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)                    -3.291e+03  2.930e+02 -11.234  < 2e-16 ***
wfl                             1.172e+01  2.483e-01  47.193  < 2e-16 ***
roomiete                       -4.642e+01  6.487e+00  -7.157 1.03e-12 ***
bj                              1.649e+00  1.473e-01  11.195  < 2e-16 ***
bezAltstadt-Lehel               7.912e-01  4.746e+01   0.017  0.98670    
bezAu-Haidhausen                6.966e+01  4.019e+01   1.733  0.08318 .  
bezAubing...                   -5.324e+01  4.455e+01  -1.195  0.23211    
bezBerg am Laim                -3.491e+01  4.140e+01  -0.843  0.39910    
bezBogenhausen                  1.587e+00  4.005e+01   0.040  0.96840    
bezFledmoching-Hasenbergel     -8.195e+01  4.258e+01  -1.924  0.05440 .  
bezHadern                      -3.054e+01  4.250e+01  -0.719  0.47246    
bezLaim                        -2.704e+01  4.149e+01  -0.652  0.51467    
bezLudwigvorstadt-Isarvorstadt  1.103e+02  4.048e+01   2.726  0.00645 ** 
bezMaxvorstadt                  1.037e+02  4.044e+01   2.563  0.01042 *  
bezMilbersthofen-Am Hart        4.118e+00  4.045e+01   0.102  0.91891    
bezMoosach                     -1.248e+01  4.178e+01  -0.299  0.76509    
bezNeuhausen-Nymphenburg        4.623e+01  3.931e+01   1.176  0.23963    
bezObergiesing                 -1.632e+01  4.017e+01  -0.406  0.68456    
bezPasing-Obermenzing          -6.919e-02  4.093e+01  -0.002  0.99865    
bezRamersdorf-Perlach          -7.216e+01  3.950e+01  -1.827  0.06784 .  
bezSchwabing West               4.065e+01  4.029e+01   1.009  0.31311    
bezSchwabing-Freimann           6.876e+01  4.045e+01   1.700  0.08925 .  
bezSchwanthalerhöhe             4.503e+01  4.222e+01   1.067  0.28628    
bezSendling                     3.032e+01  4.064e+01   0.746  0.45568    
bezSendling-Westpark           -1.863e+01  4.070e+01  -0.458  0.64720    
bezThalkirchen...              -2.339e+01  3.970e+01  -0.589  0.55567    
bezTrudering-Riem              -3.490e+01  4.259e+01  -0.819  0.41264    
bezUntergiesing                 3.597e+01  4.056e+01   0.887  0.37517    
wohngut1                        4.494e+01  8.779e+00   5.119 3.27e-07 ***
wohnbest1                       1.014e+02  2.013e+01   5.039 4.96e-07 ***
ww01                           -1.789e+02  3.782e+01  -4.730 2.35e-06 ***
zh01                           -7.861e+01  1.434e+01  -5.480 4.59e-08 ***
badkach01                       5.233e+01  1.047e+01   4.997 6.15e-07 ***
badextra1                       3.290e+01  1.095e+01   3.004  0.00269 ** 
kueche1                         8.557e+01  8.168e+00  10.476  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 184.9 on 3030 degrees of freedom
Multiple R-squared:  0.7043,	Adjusted R-squared:  0.701 
F-statistic: 212.3 on 34 and 3030 DF,  p-value: < 2.2e-16'
#nur drei von 24 Bezirken signifikant -> Variable Bezirk entfernen

#Modell 2####
#Modellbildung ohne Variable Bezirk
mietelm2 <- lm(nm ~ ., data = miete[,-c(2,6)]) # Regressant: nm, Regressor: alle anderen Variablen
summary(mietelm2)
'
Call:
lm(formula = nm ~ ., data = miete[, -c(2, 6)])

Residuals:
     Min       1Q   Median       3Q      Max 
-1022.92  -102.16     1.81    96.59  2491.59 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -2218.7611   275.0541  -8.067 1.03e-15 ***
wfl            11.9284     0.2509  47.541  < 2e-16 ***
roomiete      -55.5408     6.5566  -8.471  < 2e-16 ***
bj              1.1074     0.1401   7.905 3.70e-15 ***
wohngut1       82.4023     7.3415  11.224  < 2e-16 ***
wohnbest1     118.4039    18.9315   6.254 4.55e-10 ***
ww01         -184.5388    38.6394  -4.776 1.87e-06 ***
zh01          -67.2810    14.6376  -4.596 4.47e-06 ***
badkach01      52.3544    10.6974   4.894 1.04e-06 ***
badextra1      30.6062    11.0553   2.768  0.00567 ** 
kueche1        85.3459     8.3193  10.259  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 189.5 on 3054 degrees of freedom
Multiple R-squared:  0.6869,	Adjusted R-squared:  0.6859 
F-statistic:   670 on 10 and 3054 DF,  p-value: < 2.2e-16
'
#alle Variablen sind signifikant, Interpretation von Zimmerkoeffizient fragwürdig

#Korrelationsanalyse
cor(model.matrix(mietelm2)[,-1], method = "spearman")

#Modell 3####
#Modellbildung ohne Variable Bezirk
mietelm3 <- lm(nm ~ ., data = miete[,-c(2,4,6)]) # Regressant: nm, Regressor: alle anderen Variablen
summary(mietelm3)

'Call:
lm(formula = nm ~ ., data = miete[, -c(2, 4, 6)])

Residuals:
     Min       1Q   Median       3Q      Max 
-1033.47  -100.95     3.34    95.54  2690.19 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -2341.6908   277.8337  -8.428  < 2e-16 ***
wfl            10.1633     0.1414  71.886  < 2e-16 ***
bj              1.1553     0.1416   8.161 4.81e-16 ***
wohngut1       87.4565     7.4015  11.816  < 2e-16 ***
wohnbest1     131.3731    19.0868   6.883 7.09e-12 ***
ww01         -187.5855    39.0826  -4.800 1.67e-06 ***
zh01          -65.2646    14.8042  -4.409 1.08e-05 ***
badkach01      53.4245    10.8198   4.938 8.33e-07 ***
badextra1      29.3194    11.1815   2.622  0.00878 ** 
kueche1        95.4618     8.3279  11.463  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 191.7 on 3055 degrees of freedom
Multiple R-squared:  0.6795,	Adjusted R-squared:  0.6786 
F-statistic: 719.8 on 9 and 3055 DF,  p-value: < 2.2e-16
'
#alle Variablen sind signifikant, Interpretation aller Koeffizienten sinnvoll

#Modellauswahl####
#nur Radjsqr, AIC und BIC werden betrachtet
#xtable(ols_step_all_possible(mietelm3)[,-c(1,4,6,7,9,11:14)])

#es wird das volle Modell gewählt

#Modellauswahl mit Vorwärts-Selektion
lm.forw <- step(mietelm3, scope = formula(miete[,-c(2,4,6)]), direction = "both")
'Start:  AIC=32229.39
nm ~ wfl + bj + wohngut + wohnbest + ww0 + zh0 + badkach0 + badextra + 
    kueche

           Df Sum of Sq       RSS   AIC
<none>                  112287686 32229
- badextra  1    252713 112540399 32234
- zh0       1    714339 113002025 32247
- ww0       1    846746 113134432 32250
- badkach0  1    896116 113183802 32252
- wohnbest  1   1741276 114028962 32275
- bj        1   2447830 114735516 32293
- kueche    1   4829572 117117258 32356
- wohngut   1   5131760 117419446 32364
- wfl       1 189936643 302224329 35262'

#es wird das volle Modell gewählt

#Modellauswahl mit Rückwärts-Elimination
lm.back <- step(mietelm3, scope = formula(miete[,-c(2,4,6)]), direction = "both")
'Start:  AIC=32229.39
nm ~ wfl + bj + wohngut + wohnbest + ww0 + zh0 + badkach0 + badextra + 
  kueche

Df Sum of Sq       RSS   AIC
<none>                  112287686 32229
- badextra  1    252713 112540399 32234
- zh0       1    714339 113002025 32247
- ww0       1    846746 113134432 32250
- badkach0  1    896116 113183802 32252
- wohnbest  1   1741276 114028962 32275
- bj        1   2447830 114735516 32293
- kueche    1   4829572 117117258 32356
- wohngut   1   5131760 117419446 32364
- wfl       1 189936643 302224329 35262'
#es wird das volle Modell gewählt

#Diagnostikplots 
par( mfrow = c(2,2))
par(mar = c(5, 4, 2, 2) + 0.2)
#Residualplot
plot(mietelm3, which = 1, main = "d", sub="", caption = "", ann=F)
title(xlab= expression(paste(, hat(y),)), ylab=expression(paste(, epsilon,)))
#QQ-Plot
plot(mietelm3, which = 2, main = "d", sub="", caption = "", ann=F)
title(xlab = "Theoretische Quantile der Normalverteiung", ylab=expression(paste(, tilde(epsilon),)))
#Cooks-Distance
plot(mietelm3, which = 4, main = "d", sub="", caption = "", ann=F)
title(xlab = " Beobachtungsnummer", ylab="Cook's Distance")
length(which(cooks.distance(mietelm1) > 4/3065))
#Leverage
plot(mietelm3, which=5, main = "d", sub="", caption = "", ann=F,extend.ylim.f=1.5)
title(xlab = "Leverage Scores", ylab=expression(paste(, tilde(epsilon),)))

#Beobachtung 1975 auffällig
sum(cooks.distance(mietelm3)>0.5)
cooks.distance(mietelm3)[1975] 
#0.9275647 nah an 1
hatvalues(mietelm3)[1975]
#0.03765257 
hatvalues(mietelm3)[1975] > 2*9/3065 #hat Beobachtung #1975 high leverage?
#TRUE 

##Model 4####
#Modellbildung ohne high leverage point 1975
mietelm4 <- lm(nm ~ ., data = miete[-1975,-c(2,4,6)]) # Regressant: nm, Regressor: alle anderen Variablen
summary(mietelm4)
'
Call:
lm(formula = nm ~ ., data = miete[-1975, -c(2, 4, 6)])

Residuals:
    Min      1Q  Median      3Q     Max 
-980.93  -99.73    1.77   93.58 1278.88 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -2162.9552   268.6839  -8.050 1.17e-15 ***
wfl             9.8263     0.1385  70.963  < 2e-16 ***
bj              1.0760     0.1369   7.861 5.23e-15 ***
wohngut1       88.8216     7.1511  12.421  < 2e-16 ***
wohnbest1     111.6310    18.4877   6.038 1.75e-09 ***
ww01         -184.1406    37.7580  -4.877 1.13e-06 ***
zh01          -68.0905    14.3035  -4.760 2.02e-06 ***
badkach01      54.6290    10.4532   5.226 1.85e-07 ***
badextra1      37.5936    10.8168   3.475 0.000517 ***
kueche1        90.5553     8.0523  11.246  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 185.2 on 3054 degrees of freedom
Multiple R-squared:  0.6756,	Adjusted R-squared:  0.6746 
F-statistic: 706.7 on 9 and 3054 DF,  p-value: < 2.2e-16'

#Voraussetzungen erfüllt?

#Spaltenrang von Designmatrix X
Design <- model.matrix(mietelm4)
ncol(model.matrix(mietelm4))
#[1] 10
qr(model.matrix(mietelm4))$rank
#[1] 10
#Voraussetzung voller Spaltenrang erfüllt

#Diagnostikplots 
par( mfrow = c(2,2))
par(mar = c(5, 4, 2, 2) + 0.2)
#Residualplot
plot(mietelm4, which = 1, main = "d", sub="", caption = "", ann=F)
title(xlab= expression(paste(, hat(y),)), ylab=expression(paste(, epsilon,)))
#QQ-Plot
plot(mietelm4, which = 2, main = "d", sub="", caption = "", ann=F)
title(xlab = "Theoretische Quantile der Normalverteiung", ylab=expression(paste(, tilde(epsilon),)))
#Cooks-Distance
plot(mietelm4, which = 4, main = "d", sub="", caption = "", ann=F)
title(xlab = " Beobachtungsnummer", ylab="Cook's Distance")
#Leverage
plot(mietelm4, which=5, main = "d", sub="", caption = "", ann=F,extend.ylim.f=1.5)
title(xlab = "Leverage Scores", ylab=expression(paste(, tilde(epsilon),)))

#Multikolinearität
#über VIF
vif(mietelm4)
'wfl      bj       wohngut  wohnbest ww0      zh0      badkach0 badextra kueche 
 1.105795 1.174469 1.044665 1.047383 1.071356 1.135626 1.060280 1.086190 1.085868 '
#kein VIF über 5, Voraussetzung keine Multikolinearität unter Regressoren


######################Latex Ausgabe#########################
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

#Tabelle 3: Korrelationsanalyse
xtable(cor(model.matrix(mietelm2)[,-1], method = "spearman"))

#Abbildung 1: Boxplot Nettomiete
par( mfrow = c(1,1))
par(mar = c(5, 4, 6, 2) + 0.2)
boxplot(miete$nm, col = "slategray2",xlab = "Nettomiete in Euro", horizontal = T)

#Abbildung 2: Diagnostikplots Modell 3
par( mfrow = c(2,2))
par(mar = c(5, 4, 2, 2) + 0.2)
#Residualplot
plot(mietelm3, which = 1, main = "d", sub="", caption = "", ann=F)
title(xlab= expression(paste(, hat(y),)), ylab=expression(paste(, epsilon,)))
#QQ-Plot
plot(mietelm3, which = 2, main = "d", sub="", caption = "", ann=F)
title(xlab = "Theoretische Quantile der Normalverteiung", ylab=expression(paste(, tilde(epsilon),)))
#Cooks-Distance
plot(mietelm3, which = 4, main = "d", sub="", caption = "", ann=F)
title(xlab = " Beobachtungsnummer", ylab="Cook's Distance")
length(which(cooks.distance(mietelm1) > 4/3065))
#Leverage
plot(mietelm3, which=5, main = "d", sub="", caption = "", ann=F,extend.ylim.f=1.5)
title(xlab = "Leverage Scores", ylab=expression(paste(, tilde(epsilon),)))


#Abbildung 3: Diagnostikplots Modell 4
par( mfrow = c(2,2))
par(mar = c(5, 4, 2, 2) + 0.2)
#Residualplot
plot(mietelm4, which = 1, main = "d", sub="", caption = "", ann=F)
title(xlab= expression(paste(, hat(y),)),
      ylab=expression(paste(, epsilon,)))
#QQ-Plot
plot(mietelm4, which = 2, main = "d", sub="", caption = "", ann=F)
title(xlab = "Theoretische Quantile der Normalverteiung", ylab=expression(paste(, tilde(epsilon),)))
#Cooks-Distance
plot(mietelm4, which = 4, main = "d", sub="", caption = "", ann=F)
title(xlab = "Beobachtungsnummer", ylab="Cook's Distance")
#Leverage
plot(mietelm4, which=5, main = "d", sub="", caption = "", ann=F,extend.ylim.f=1.5)
title(xlab = "Leverage Scores", ylab=expression(paste(, tilde(epsilon),)))

#Abbildung 4: Residualplot bei Homoskedastizität der Residuen
par( mfrow = c(1,1))
par(mar = c(10, 4, 2, 2) + 0.2)
set.seed(1714)
x <- 1:50
y <- 1 + x + rnorm(50, 0, 1)
M2 <- lm ( y ~ x )
plot(M2, which = 1, caption = " ", ann = FALSE, sub="") # Nv Annnahme 
title(xlab= expression(paste(, hat(y),)),ylab=expression(paste(, epsilon,)))

#Abbildung 5: Residualplot Homoskedastizität verletzt:
y <- 1 + x + rnorm(50, 0, 1:50)
M3 <- lm ( y ~ x )
plot(M3, which = 1, caption = " ", ann = FALSE, sub="") # Heterosk. 
title(xlab= expression(paste(, hat(y),)),ylab=expression(paste(, epsilon,)))

#Abbildung 6: Residualplots andere Scenarien
par( mfrow = c(2, 2))
par(mar = c(5, 4, 2, 2) + 0.2)
# (i) autoregressive Fehler mit positiver Korrelation:
e <- rnorm( 1, 0, 1)
for( i in 2:50){
  e[i] <- 0.9 * e[i-1] + rnorm( 1, 0, 1)
}
y <- 1 + x + e
M6 <- lm (y ~ x)
plot(M6, which = 1, caption = " ",
     ann = FALSE, sub = "") # positive Korr
title(xlab= expression(paste(, hat(y),)),
                       ylab=expression(paste(, epsilon,)),
      main="positive Korrelation", )

# (ii) autoregressive Fehler mit neagtiver Korrelation:
e <- rnorm( 1, 0, 1)
for( i in 2:50){
  e[i] <- -0.9 * e[i-1] + rnorm( 1, 0, 1)
}
y <- 1 + x + e
M7 <- lm (y ~ x)
plot(M7, which = 1, caption = " ",
     ann = FALSE, sub = "") # negative Korr
title(xlab= expression(paste(, hat(y),)),
                       ylab=expression(paste(, epsilon,)),
      main="negative Korrelation", )

# (iii) Ausreisser:
y <- 1 + x + rnorm(50, 0, 1)
y[20] <- 50
M5 <- lm( y ~ x)
plot(M5, which = 1, caption = " ",
     ann = FALSE, sub = "") # Ausreisser
title(xlab= expression(paste(, hat(y),)),
      ylab=expression(paste(, epsilon,)),
      main= "Ausreißer")

# (vi) Nichtlinearer Zusammenhang:
y <- log( 1 + x + rnorm(50, 0, 1) )
M8 <- lm( y ~ x )
plot(M8, which = 1, caption = " ",
     ann = FALSE, sub = "") # nichtlinearer Zusammenhang
title(xlab= expression(paste(, hat(y),)),
      ylab=expression(paste(, epsilon,)),
      main="nichtlinearer Zusammenhang")

par( mfrow = c(1,1))
par(mar = c(5, 4, 2, 2) + 0.2)

#Abbildung 7: #QQ-Plots Beispiel Normalverteilung
  qqnorm(rnorm(1000, 0, 1),ylab=expression(paste(, tilde(epsilon),)), 
         xlab = "Theoretische Quantile der Normalverteilung", main="") 
  qqline(rnorm(1000, 0, 1),lty=2)
  abline(a = 0, b = 1, col="red")

#Abbildung 8: absolute prozentuale Residuen vs. fitted values  
  plot(mietelm4$fitted.values, abs(mietelm4$fitted.values - miete[-1975,]$nm)/miete[-1975,]$nm,
       ylab=expression(paste(, dot(epsilon),)), xlab= expression(paste(, hat(y),)))  
  
#Abbildung 9: #QQ-Plots Beispiele andere 
  par(mfrow = c(2,2))
#rechtsschiefe Verteilung
  qqnorm(rexp(1000, 2), main="Extrem rechtsschiefe Verteilung", 
         ylab=expression(paste(, tilde(epsilon),)), 
          xlab = "Theoretische Quantile der Normalverteilung")
  abline(a = 0, b = 1, col="red")
#linksschiefe Verteilung 
  load("skew.RData") #Beispieldatensatz
  qqnorm(dat[,9],  main="Extrem linksschiefe Verteilung", 
         ylab=expression(paste(, tilde(epsilon),)), 
         xlab = "Theoretische Quantile der Normalverteilung")
  abline(a = 0, b = 1,col="red")
#spitze Verteilung
  qqnorm(rnorm(1000, 0, 0.2),main="Extrem spitze Verteilung", 
         ylab=expression(paste(, tilde(epsilon),)), 
         xlab = "Theoretische Quantile der Normalverteilung") 
  abline(a = 0, b = 1, col="red")
#flache Verteilung
  qqnorm(rnorm(1000, 0, 5), main="Extrem flache Verteilung", 
         ylab=expression(paste(, tilde(epsilon),)), 
         xlab = "Theoretische Quantile der Normalverteilung") 
  abline(a = 0, b = 1, col="red")

  