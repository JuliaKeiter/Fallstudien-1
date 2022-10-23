setwd("~/Documents/Uni/6. Semester/Fallstudien/Projekt 1")
library(readxl)
library(ggplot2)
library(moments)

#Daten####
KHK <- read_xlsx("KHK_Studie_Demografie.xlsx")

KHK$landnr <- as.factor(KHK$landnr)
KHK$zentrum <- as.factor(KHK$zentrum)
KHK$sex <- as.factor(KHK$sex)
KHK$gruppe <- as.factor(KHK$gruppe)
KHK$myo_infarct <- as.factor(KHK$myo_infarct)
str(KHK)
'tibble [200 × 15] (S3: tbl_df/tbl/data.frame)
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

#univariate Kenngrößen###

#p-Quantil 
?quantile
##########aktiv
lapply(aktiv[,c(3,4,6:8)],quantile, type=2)
'$groesse
0%  25%  50%  75% 100% 
146  163  170  175  191 

$gewicht
0%  25%  50%  75% 100% 
46   66   75   82  132 

$alter
0%      25%      50%      75%     100% 
56.57769 68.18344 73.01027 77.52772 89.59890 

$bmi
0%      25%      50%      75%     100% 
17.78971 23.93899 25.71220 28.84153 41.19722 

$dauer_insuff
0%         25%         50%         75%        100% 
0.5666667   9.3000000  25.5666667  71.6666667 311.2000000 '

###########placebo
lapply(placebo[,c(3,4,6:8)],quantile, type=2)
'$groesse
0%  25%  50%  75% 100% 
150  166  170  175  189 

$gewicht
0%  25%  50%  75% 100% 
52   68   75   81  121 

$alter
0%      25%      50%      75%     100% 
63.67420 68.22998 73.60164 77.92745 86.71321 

$bmi
0%      25%      50%      75%     100% 
17.78971 24.16716 25.51020 28.37370 36.93416 

$dauer_insuff
0%         25%         50%         75%        100% 
0.5666667  10.8666667  26.8333333  63.2333333 152.1000000 '

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

#Grafiken####

#barplots
?barplot
#Medikamentengruppe
barplot(table(KHKrand$gruppe)/length(KHKrand$gruppe), ylim = c(0,1), beside = T, 
        names.arg = c("aktiv", "placebo"), ylab = "relative Häufigkeit",
        col = c("slategray3", "slategray4"))

par(mfrow = c(1,2))
#Geschlecht
######aktiv
barplot(table(aktiv$sex)/length(aktiv$sex), ylim = c(0,1), beside = T, 
        names.arg = c("Männlich", "Weiblich"), ylab = "relative Häufigkeit",
        col = c("slategray3", "slategray4"), main="aktiv")
######placebo
barplot(table(placebo$sex)/length(placebo$sex), ylim = c(0,1), beside = T, 
        names.arg = c("Männlich", "Weiblich"),ylab = "relative Häufigkeit",
        col = c("slategray3", "slategray4"), main="placebo")

#Infarkt
######aktiv
barplot(table(aktiv$myo_infarct)/length(aktiv$myo_infarct), ylim = c(0,1), beside = T, 
        names.arg = c("Männlich", "Weiblich"), ylab = "relative Häufigkeit",
        col = c("slategray3", "slategray4"), main="aktiv")
######placebo
barplot(table(placebo$myo_infarct)/length(placebo$myo_infarct), ylim = c(0,1), beside = T, 
        names.arg = c("Männlich", "Weiblich"),ylab = "relative Häufigkeit",
        col = c("slategray3", "slategray4"), main="placebo")

#Histogramme
?hist

#Größe
######aktiv
hist(aktiv$groesse, ylab = "absolute Häufigkeit",
     col = "slategray2", xlab = "Größe in cm", main = "aktiv", xlim = c(140, 200), 
     ylim = c(0,15), breaks = 20)
######placebo
hist(placebo$groesse, ylab = "absolute Häufigkeit",
     col = "slategray2", xlab = "Größe in cm", main = "placebo", xlim = c(140, 200), 
     ylim = c(0,15), breaks = 20)

#Gewicht
######aktiv
hist(aktiv$gewicht, ylab = "absolute Häufigkeit",
     col = "slategray2", xlab = "Größe in cm", main = "aktiv", 
     ylim = c(0,15), breaks = 20)
######placebo
hist(placebo$gewicht, ylab = "absolute Häufigkeit",
     col = "slategray2", xlab = "Größe in cm", main = "placebo", 
     ylim = c(0,15), breaks = 20)

#Alter
######aktiv
hist(aktiv$alter, ylab = "absolute Häufigkeit",
     col = "slategray2", xlab = "Größe in cm", main = "aktiv", 
     ylim = c(0,15), breaks = 20)
######placebo
hist(placebo$alter, ylab = "absolute Häufigkeit",
     col = "slategray2", xlab = "Größe in cm", main = "placebo", 
     ylim = c(0,15), breaks = 20)

#BMI
######aktiv
hist(aktiv$bmi, ylab = "absolute Häufigkeit",
     col = "slategray2", xlab = "Größe in cm", main = "aktiv", 
     ylim = c(0,15), breaks = 20)
######placebo
hist(placebo$bmi, ylab = "absolute Häufigkeit",
     col = "slategray2", xlab = "Größe in cm", main = "placebo", 
     ylim = c(0,15), breaks = 20)

#dauer
######aktiv
hist(aktiv$dauer_insuff, ylab = "absolute Häufigkeit",
     col = "slategray2", xlab = "Größe in cm", main = "aktiv", 
     ylim = c(0,15), breaks = 20)
######placebo
hist(placebo$dauer_insuff, ylab = "absolute Häufigkeit",
     col = "slategray2", xlab = "Größe in cm", main = "placebo", 
     ylim = c(0,15), breaks = 20)

#boxplot
?boxplot
#Größe
########aktiv
boxplot(aktiv$groesse, col = "slategray2", main="aktiv")
#######placebo
boxplot(placebo$groesse, col = "slategray2", main="placebo")

#Gewicht
########aktiv
boxplot(aktiv$gewicht, col = "slategray2", main="aktiv")
#######placebo
boxplot(placebo$gewicht, col = "slategray2", main="placebo")

#Alter
########aktiv
boxplot(aktiv$alter, col = "slategray2", main="aktiv")
#######placebo
boxplot(placebo$alter, col = "slategray2", main="placebo")

#BMI
########aktiv
boxplot(aktiv$bmi, col = "slategray2", main="aktiv")
#######placebo
boxplot(placebo$bmi, col = "slategray2", main="placebo")

#dauer
########aktiv
boxplot(aktiv$dauer_insuff, col = "slategray2", main="aktiv")
#######placebo
boxplot(placebo$dauer_insuff, col = "slategray2", main="placebo")


#empirische Verteilungsfunktion
par(mfrow = c(1,1))

#Größe
########aktiv
Fa <- ecdf(aktiv$groesse)
plot(Fa, lty=0.2, col="slategray4", cex=0.8)
#######placebo
Fp <- ecdf(placebo$groesse)
plot(Fp, lty=0.2, col="slategray3", add=T)
legend("right", legend=c("aktiv", 
                            "placebo"), 
       fill = c("slategray4", "slategray3"), cex = 1)

#Gewicht
########aktiv
Fa <- ecdf(aktiv$gewicht)
plot(Fa, lty=0.2, col="slategray4", cex=0.8)
#######placebo
Fp <- ecdf(placebo$gewicht)
plot(Fp, lty=0.2, col="slategray3", add=T)
legend("right", legend=c("aktiv", 
                           "placebo"), 
       fill = c("slategray4", "slategray3"), cex = 1)

#Alter
########aktiv
Fa <- ecdf(aktiv$alter)
plot(Fa, lty=0.2, col="slategray4", cex=0.8)
#######placebo
Fp <- ecdf(placebo$alter)
plot(Fp, lty=0.2, col="slategray3", add=T)
legend("right", legend=c("aktiv", 
                         "placebo"), 
       fill = c("slategray4", "slategray3"), cex = 1)

#BMI
########aktiv
Fa <- ecdf(aktiv$bmi)
plot(Fa, lty=0.2, col="slategray4", cex=0.8)
#######placebo
Fp <- ecdf(placebo$bmi)
plot(Fp, lty=0.2, col="slategray3", add=T)
legend("right", legend=c("aktiv", 
                         "placebo"), 
       fill = c("slategray4", "slategray3"), cex = 1)

#dauer
########aktiv
Fa <- ecdf(aktiv$dauer_insuff)
plot(Fa, lty=0.2, col="slategray4", cex=0.8)
#######placebo
Fp <- ecdf(placebo$dauer_insuff)
plot(Fp, lty=0.2, col="slategray3", add=T)
legend("right", legend=c("aktiv", 
                         "placebo"), 
       fill = c("slategray4", "slategray3"), cex = 1)
