setwd("~/Documents/Uni/6. Semester/Fallstudien/Projekt 1")
library(readxl)
library(ggplot2)
library(xtable)
library(moments)

#Daten###############################################
KHK <- read_xlsx("KHK_Studie_Demografie.xlsx")

## Strukturierung der interessierenden binären Variablen: ----------------------------------------------
KHK$sex[KHK$sex == 1] <- "männlich"
KHK$sex[KHK$sex == 2] <- "weiblich"

KHK$myo_infarct[KHK$myo_infarct == 1] <- "ja"
KHK$myo_infarct[KHK$myo_infarct == 2] <- "nein"

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

#Wie viele fehlende Werte?
#insgesamt
sum(is.na(KHK))
#[1] 86
#unter den randomisierten Patienten
KHKrand <- subset(KHK, patnr!="NA")
sum(is.na(KHKrand))
#[1] 5
KHKrand <- subset(KHKrand, dauer_insuff!="NA")

#interessierende Variablen
KHKint <- KHKrand[,-c(1:3,9:11)]

#Aufteilung in Medikamentengruppen####
aktiv <- subset(KHKint, gruppe=="1")
placebo <- subset(KHKint, gruppe=="2")

#univariate Kenngrößen###################################

#p-Quantil 
?quantile
##########aktiv
lapply(aktiv[,c(3,4,6:8)],IQR, type=2)
'$groesse
[1] 13

$gewicht
[1] 21

$alter
[1] 8.824093

$bmi
[1] 5.534989

$dauer_insuff
[1] 69.23333'

###########placebo
lapply(placebo[,c(3,4,6:8)],IQR, type=2)
'$groesse
[1] 9

$gewicht
[1] 13

$alter
[1] 9.697467

$bmi
[1] 4.20654

$dauer_insuff
[1] 52.36667'

#arithmetisches Mittel
?mean
######aktiv
lapply(aktiv[,c(3,4,6:8)],mean)
'$groesse
[1] 167.4878

$gewicht
[1] 75.93902

$alter
[1] 72.87331

$bmi
[1] 26.93664

$dauer_insuff
[1] 52.29146
'
#######placebo
lapply(placebo[,c(3,4,6:8)],mean)
'$groesse
[1] 170.026

$gewicht
[1] 76.16883

$alter
[1] 73.60755

$bmi
[1] 26.35273

$dauer_insuff
[1] 43.3342'

#Varianz
?var
##########aktiv
lapply(aktiv[,c(3,4,6:8)],var)
'$groesse
[1] 96.45047

$gewicht
[1] 250.7246

$alter
[1] 40.06225

$bmi
[1] 18.57202

$dauer_insuff
[1] 4324.203'

###########placebo
lapply(placebo[,c(3,4,6:8)],var)
'$groesse
[1] 59.57826

$gewicht
[1] 124.6159

$alter
[1] 36.51825

$bmi
[1] 11.75392

$dauer_insuff
[1] 1577.061'

#Standardabweichung
?sd
###########aktiv
lapply(aktiv[,c(3,4,6:8)],sd)
'$groesse
[1] 9.82092

$gewicht
[1] 15.83429

$alter
[1] 6.329475

$bmi
[1] 4.309527

$dauer_insuff
[1] 65.75867'
###########placebo
lapply(placebo[,c(3,4,6:8)],sd)
'$groesse
[1] 170.026

$gewicht
[1] 76.16883

$alter
[1] 73.60755

$bmi
[1] 26.35273

$dauer_insuff
[1] 43.3342'

#Schiefe
schiefe <- function(x){
  (1/length(x) * sum( (x - mean(x))^3)) / 
  (1/length(x) * sum( (x - mean(x))^2 ) )^(3/2)
  }
########aktiv
lapply(aktiv[,c(3,4,6:8)],schiefe)
lapply(aktiv[,c(3,4,6:8)],skewness)
'$groesse
[1] -0.1898123

$gewicht
[1] 0.8510832

$alter
[1] 0.03761941

$bmi
[1] 0.7975859

$dauer_insuff
[1] 2.094775'

########placebo
lapply(placebo[,c(3,4,6:8)],schiefe)
lapply(placebo[,c(3,4,6:8)],skewness)
'$groesse
[1] -0.2217842

$gewicht
[1] 1.148107

$alter
[1] 0.2538661

$bmi
[1] 0.7480215

$dauer_insuff
[1] 1.059307'

#Wölbung
wölbung <- function(x){
  ((1/length(x)) * sum( (x - mean(x))^4)) / 
    ( (1/length(x)) * sum( (x - mean(x))^2 ) )^2
}
########aktiv
lapply(aktiv[,c(3,4,6:8)],wölbung)
lapply(aktiv[,c(3,4,6:8)],kurtosis)
'$groesse
[1] 2.585734

$gewicht
[1] 4.039405

$alter
[1] 3.035474

$bmi
[1] 3.442971

$dauer_insuff
[1] 7.668345'

########placebo
lapply(placebo[,c(3,4,6:8)],wölbung)
lapply(placebo[,c(3,4,6:8)],kurtosis)
'$groesse
[1] 3.158889

$gewicht
[1] 6.006524

$alter
[1] 2.321684

$bmi
[1] 4.115006

$dauer_insuff
[1] 3.243369'

# Funktion zur Abspeicherung von:
# Median, Interquartilsabstand, 1. und 3. Quartil, ,arithm. Mittel,
#Maximum, Minimum, Spannweite
erstell_tabelle <- function(daten){
  speicher <- numeric(10)
  speicher[1] <- quantile(daten, 1/4)
  speicher[2] <- quantile(daten, 3/4)
  speicher[3] <- median(daten)
  speicher[4] <- IQR(daten)
  speicher[5] <- skewness(daten)
  speicher[6] <- sd(daten)
  speicher[7] <- kurtosis(daten)
  speicher[8] <- min(daten)
  speicher[9] <- max(daten)
  speicher[10] <- speicher[9] - speicher[8]
  names(speicher) <- c("1.Quartil", "3.Quartil", "Median", "IQR", "emplacebo Schiefekoef.",
                       "Standardabw.","Wölbung", "Minimum","Maximum", "Spannweite")
  return(speicher)
}

# kategoriellen Variablen (sex, myo_infarct) aus dem randomisierten
# Datensatz aufgeteilt nach Medikationsgruppen  (aktiv, placebo)

# Tabelle 1:
xtable(cbind(aktiv = c(table(aktiv$sex), length(aktiv$sex)), 
             placebo = c(table(placebo$sex), length(placebo$sex))), 
       caption = "Vierfeldertafel für Geschlecht aus randomisierten Datensatz")

'# Tabelle E2:
xtable(cbind(aktiv = table(aktiv$myo_infarct), 
             placebo = table(placebo$myo_infarct)), 
       caption = "Vierfeldertafel für Herzinfarkt aus randomisierten Datensatz")'


# metrischen Variablen (groesse, gewicht, alter, bmi, dauer_insuff) aus dem 
# randomisierten Datensatz aufgeteilt nach Medikationsgruppen  (aktiv, placebo)
# Tabelle 2:
xtable(caption = "univariate Kenngrößen für metrische interessierende Variablen aus randomisierten Datensatz (a.=aktiv, p.=placebo)",
       cbind("Größe (a.)" = erstell_tabelle(na.omit(aktiv$groesse)),
             "Größe (p.)" = erstell_tabelle(na.omit(placebo$groesse)),
             "Gewicht (a.)" = erstell_tabelle(na.omit(aktiv$gewicht)),
             "Gewicht (p.)"= erstell_tabelle(na.omit(placebo$gewicht)),
             "Alter (a.)" = erstell_tabelle(na.omit(aktiv$alter)),
             "Alter (p.)" = erstell_tabelle(na.omit(placebo$alter)),
            "BMI (a.)" = erstell_tabelle(na.omit(aktiv$bmi)),
             "BMI (p.)" = erstell_tabelle(na.omit(placebo$bmi)),
             "Dauer (a.)" = erstell_tabelle(na.omit(aktiv$dauer_insuff)),
             "Dauer (p.)" = erstell_tabelle(na.omit(placebo$dauer_insuff))),
       digits = 2)

#Grafiken##########################################
pdf("Grafiken.pdf", width =20, height = 15)
'
#barplots
?barplot
#Medikamentengruppe
barplot(table(KHKrand$gruppe)/length(KHKrand$gruppe), ylim = c(0,1), beside = T, 
        names.arg = c("aktiv", "placebo"), ylab = "relative Häufigkeit",
        col = c("slategray3", "slategray4"), cex.axis = 2, cex.names = 2)
Geschlecht
######aktiv
barplot(table(aktiv$sex)/length(aktiv$sex), ylim = c(0,1), beside = T, 
        names.arg = c("Männlich", "Weiblich"), ylab = "relative Häufigkeit",
        col = c("slategray3", "slategray4"), main="aktiv", cex.axis = 2, cex.names = 2,
        cex.main=2)
######placebo

barplot(table(placebo$sex)/length(placebo$sex), ylim = c(0,1), beside = T, 
        names.arg = c("Männlich", "Weiblich"),ylab = "relative Häufigkeit",
        col = c("slategray3", "slategray4"), main="placebo", cex.axis = 2, cex.names = 2,
        cex.main=2)
table(aktiv$sex)/length(aktiv$sex) - table(placebo$sex)/length(placebo$sex)'

#Infarkt
par(mfrow = c(1,2))
rand <- par(mar = c(5, 6, 4, 2) + 0.2)
######aktiv
barplot(table(aktiv$myo_infarct)/length(aktiv$myo_infarct), ylim = c(0,1), beside = T, 
        names.arg = c("Infarkt", "Kein Infarkt"), ylab = "relative Häufigkeit",
        col = c("slategray3", "slategray4"), main="aktiv", cex.axis = 2.5, cex.names = 2,
        cex.main=3, cex.lab=3.5)
######placebo

barplot(table(placebo$myo_infarct)/length(placebo$myo_infarct), ylim = c(0,1), beside = T, 
        names.arg = c("Infarkt", "Kein Infarkt"),ylab = "relative Häufigkeit",
        col = c("slategray3", "slategray4"), main="placebo", cex.axis = 2.5, cex.names = 2,
        cex.main=3, cex.lab=3.5)
table(aktiv$myo_infarct)/length(aktiv$myo_infarct)-table(placebo$myo_infarct)/length(placebo$myo_infarct)

par(rand)
#Histogramme
?hist

## Funktion die ein Histogramm erstellt und eine Normalverteilung drueber
## legt. Die Normalverteilung hat dabei den Mittelwert der Variable als
## mu und die Varainz/Standardabweichung als sigma^2 bzw. sigmaktiv

comp_norm <- function(y, xlab, ...){
  x <- y
  hist(y, freq = FALSE, ylab = "Dichte", xlab = xlab, main = " ", cex.axis = 2.5,
       cex.main=3, cex.lab=3.5,...)
  curve(dnorm(x, mean= mean(y, na.rm = TRUE), sd= sd(y, na.rm = TRUE)), 
        col="darkblue", lwd=3.5, add=TRUE, yaxt="n")
  mea <- round(mean(y))
  var <- round(var(y), digits = 2)
  legend("topright", 
         legend = expr(paste(italic(N), group( "(", list(!!mea, !!var), ")" ) ) ),
         col = "darkblue", lty = 1, lwd = 3.5, cex=2)
}

#Größe
par(mfrow = c(1,2))
rand <- par(mar = c(5, 6, 4, 2) + 0.2)
######aktiv

comp_norm(aktiv$groesse, "Größe in cm in Gruppe aktiv", col="slategray2", 
          ylim=c(0,0.06), xlim=c(140,200))
######placebo
comp_norm(placebo$groesse, "Größe in cm in Gruppe placebo", col="slategray2",
          ylim=c(0,0.06), xlim=c(140,200))

#Gewicht
######aktiv
comp_norm(aktiv$gewicht, "Gewicht in kg in Gruppe aktiv", col="slategray2",
          ylim=c(0,0.04), xlim=c(40,140))
######placebo
comp_norm(placebo$gewicht, "Gewicht in kg in Gruppe placebo", col="slategray2",
          ylim=c(0,0.04), xlim=c(40,140))

#Alter
######aktiv
comp_norm(aktiv$alter, "Alter in Jahren in Gruppe aktiv", col="slategray2"
          ,ylim=c(0,0.07), xlim=c(55,95))
######placebo
comp_norm(placebo$alter, "Alter in Jahren in Gruppe placebo", col="slategray2"
          ,ylim=c(0,0.07), xlim=c(55,95))

#boxplot
?boxplot
par(mfrow = c(1,1))
#BMI
# Rand vergroessern
rand <- par(mar = c(5, 6, 4, 2) + 0.1)
########aktiv
boxplot(aktiv$bmi, placebo$bmi, col = "slategray2", names = c("aktiv", "placebo"),
        ylab = expression(BMI == frac(kg, m^2)), xlab = "Medikamentengruppe")
par(rand)        

#dauer
########aktiv
boxplot(aktiv$dauer_insuff, placebo$dauer_insuff, col = "slategray2", 
        names = c("aktiv", "placebo"), ylab = "Dauer in Monaten", xlab = "Medikamentengruppe")

#empirische Verteilungsfunktion
par(mfrow = c(1,1))

#Alter
########aktiv
Fa <- ecdf(aktiv$alter)
plot(Fa, lty=0.2, col="slategray4", cex=0.8, main="", ylab = "empirische Verteilung",
     xlab="Alter in Jahren")
#######placebo
Fp <- ecdf(placebo$alter)
plot(Fp, lty=0.2, col="slategray3", add=T)
legend("right", legend=c("aktiv", 
                         "placebo"), 
       fill = c("slategray4", "slategray3"), cex = 3)

#dauer
########aktiv
Fa <- ecdf(aktiv$dauer_insuff)
plot(Fa, lty=0.2, col="slategray4", cex=0.8, main="", ylab = "empirische Verteilung",
     xlab="Dauer in Monaten")
#######placebo
Fp <- ecdf(placebo$dauer_insuff)
plot(Fp, lty=0.2, col="slategray3", add=T)
legend("right", legend=c("aktiv", 
                         "placebo"), 
       fill = c("slategray4", "slategray3"), cex = 3)
dev.off()
