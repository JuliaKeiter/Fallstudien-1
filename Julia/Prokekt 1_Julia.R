setwd("~/Documents/Uni/6. Semester/Fallstudien/Projekt 1")
library(readxl)
library(ggplot2)

KHK <- read_xlsx("KHK_Studie_Demografie.xlsx")

#Aufgabe 1 
#b)
#Geschlecht
barplot(table(KHK$sex)/length(KHK$sex), ylim = c(0,1), beside = T, 
                      names.arg = c("Männlich", "Weiblich"),
                      ylab = "relative Häufigkeit",col = c("slategray2", 
                                                                "slategray4"))
#Größe
hist(KHK$groesse, ylab = "absolute Häufigkeit",col = rep(c("slategray2","slategray4"),
                                                         5),
     xlab = "Größe in cm", main = "", xlim = c(140, 200), ylim = c(0,25), 
     breaks = 20)

#in Geschlechtern aufgeteilt
hist(subset(KHK, sex=="1")$groesse, ylab = "absolute Häufigkeit",
     col = rep(c("slategray2","slategray4"),5), xlab = "Größe der Männer in cm", 
     main = "", xlim = c(150, 200), ylim = c(0,20), breaks = 20)
hist(subset(KHK, sex=="2")$groesse, ylab = "absolute Häufigkeit",
     col = rep(c("slategray2","slategray4"),5), xlab = "Größe der Frauen in cm", 
     main = "", xlim = c(145, 180), ylim = c(0,15), breaks = 20)

#Gewicht
hist(KHK$gewicht, ylab = "absolute Häufigkeit",col = rep(c("slategray2","slategray4"),
                                                         5),
     xlab = "Gewicht in kg", main = "", xlim = c(40, 140), ylim = c(0,40), 
     breaks = 20)

#in Geschlechtern aufgeteilt
hist(subset(KHK, sex=="1")$gewicht, ylab = "absolute Häufigkeit",
     col = rep(c("slategray2","slategray4"),5), xlab = "Gewicht der Männer in kg", 
     main = "", xlim = c(40, 140), ylim = c(0,30), breaks = 20)
hist(subset(KHK, sex=="2")$gewicht, ylab = "absolute Häufigkeit",
     col = rep(c("slategray2","slategray4"),5), xlab = "Gewicht der Frauen in kg", 
     main = "", xlim = c(40, 100), ylim = c(0,10), breaks = 20)

#Alter
hist(KHK$alter, ylab = "absolute Häufigkeit",col = rep(c("slategray2","slategray4"),
                                                       5),
     xlab = "Alter", main = "", xlim = c(55, 90), ylim = c(0,30), breaks = 20)

#in Geschlechtern aufgeteilt
hist(subset(KHK, sex=="1")$alter, ylab = "absolute Häufigkeit",
     col = rep(c("slategray2","slategray4"),5), xlab = "Alter der Männer", 
     main = "", xlim = c(55, 90), ylim = c(0,20), breaks = 20)
hist(subset(KHK, sex=="2")$alter, ylab = "absolute Häufigkeit",
     col = rep(c("slategray2","slategray4"),5), xlab = "Alter der Frauen", 
     main = "", xlim = c(55, 90), ylim = c(0,15), breaks = 20)

#BMI
par(mfrow = c(1,3))
boxplot(KHK$bmi, col = "slategray2", main="alle")

boxplot(subset(KHK, sex=="1")$bmi, col = "slategray2", main="Männer")
boxplot(subset(KHK, sex=="2")$bmi, col = "slategray2", main="Frauen", 
        ylim=c(18,41.5))
par(mfrow = c(1,1))

#Dauer 
par(mfrow = c(1,3))
boxplot(KHK$dauer_insuff, col = "slategray2", main="alle")

boxplot(subset(KHK, sex=="1")$dauer_insuff, col = "slategray2", main="Männer")
boxplot(subset(KHK, sex=="2")$dauer_insuff, col = "slategray2", main="Frauen", 
        ylim=c(0,315))
par(mfrow = c(1,1))

#Herzinfarkt
barplot(table(KHK$myo_infarct)/length(KHK$myo_infarct), ylim = c(0,1), beside = T, 
        names.arg = c("Ja", "Nein"),ylab = "relative Häufigkeit",
        col = c("slategray2", "slategray4"), main = "alle")

barplot(table(subset(KHK, sex=="1")$myo_infarct)/length(subset(KHK, sex=="1")$myo_infarct),
        ylim = c(0,1), beside = T, names.arg = c("Ja", "Nein"),
        ylab = "relative Häufigkeit", col = c("slategray2", "slategray4"), main = "Männer")

barplot(table(subset(KHK, sex=="2")$myo_infarct)/length(subset(KHK, sex=="2")$myo_infarct),
        ylim = c(0,1), beside = T, names.arg = c("Ja", "Nein"),
        ylab = "relative Häufigkeit", col = c("slategray2", "slategray4"), main = "Frauen")

#c)
se <- function(x) sqrt(var(x, na.rm = T) / length(x))
Kenngrößen <- data.frame(Variablen=c("Männlich", "Weiblich", "Größe", "Gewicht",
                                     "Alter", "BMI", "Dauer", "Herzinfarkt",
                                     "kein Herzinfarkt"),
                         Summe=c(length(subset(KHK, sex=="1")$sex), 
                                   length(subset(KHK, sex=="2")$sex), 
                                   "", "", "", "","", 
                                   length(subset(KHK, myo_infarct=="1")$myo_infarct),
                                   length(subset(KHK, myo_infarct=="2")$myo_infarct)),
                         Mittelwert=c("", "", mean(KHK$groesse, na.rm = T), 
                                        mean(KHK$gewicht, na.rm = T),
                                        mean(KHK$alter, na.rm = T), 
                                        mean(KHK$bmi, na.rm = T), 
                                        mean(KHK$dauer_insuff, na.rm = T), "", ""),
                         Median=c("", "", median(KHK$groesse, na.rm = T), 
                                  median(KHK$gewicht, na.rm = T),
                                  median(KHK$alter, na.rm = T), 
                                  median(KHK$bmi, na.rm = T), 
                                  median(KHK$dauer_insuff, na.rm = T), "", ""),
                         Minimum=c("", "", range(KHK$groesse, na.rm = T)[1], 
                                      range(KHK$gewicht, na.rm = T)[1],
                                      range(KHK$alter, na.rm = T)[1], 
                                      range(KHK$bmi, na.rm = T)[1],
                                      range(KHK$dauer_insuff, na.rm = T)[1], 
                                      "", ""),
                         Maximum=c("", "", range(KHK$groesse, na.rm = T)[2], 
                                   range(KHK$gewicht, na.rm = T)[2],
                                   range(KHK$alter, na.rm = T)[2], 
                                   range(KHK$bmi, na.rm = T)[2],
                                   range(KHK$dauer_insuff, na.rm = T)[2], 
                                   "", ""),
                         Standardabweichung=c("", "", sd(KHK$groesse, na.rm = T), 
                                                sd(KHK$gewicht, na.rm = T),
                                                sd(KHK$alter, na.rm = T), 
                                                sd(KHK$bmi, na.rm = T),
                                                sd(KHK$dauer_insuff, na.rm = T), 
                                                "", ""),
                         Standardfehler=c("", "", se(KHK$groesse), se(KHK$gewicht),
                                            se(KHK$alter), se(KHK$bmi),
                                            se(KHK$dauer_insuff), "", ""),
                         Varianz=c("", "", var(KHK$groesse, na.rm = T), 
                                    var(KHK$gewicht, na.rm = T),
                                    var(KHK$alter, na.rm = T), 
                                    var(KHK$bmi, na.rm = T),
                                    var(KHK$dauer_insuff, na.rm = T), "", ""))

#Aufgabenteil 2
KHKrand <- subset(KHK, patnr!="NA")

#a)
sum(KHKrand$gruppe=="1") #Anzahl aktiv
sum(KHKrand$gruppe=="2") #Anzahl Placebo

#b)
#aktiv
aktiv <- subset(KHKrand, gruppe=="1")

Kenngrößenaktiv <- data.frame(Variablen=c("Männlich", "Weiblich", "Größe", "Gewicht",
                                     "Alter", "BMI", "Dauer", "Herzinfarkt",
                                     "kein Herzinfarkt"),
                         Summe=c(length(subset(aktiv, sex=="1")$sex), 
                                 length(subset(aktiv, sex=="2")$sex), 
                                 "", "", "", "","", 
                                 length(subset(aktiv, myo_infarct=="1")$myo_infarct),
                                 length(subset(aktiv, myo_infarct=="2")$myo_infarct)),
                         Mittelwert=c("", "", mean(aktiv$groesse, na.rm = T), 
                                      mean(aktiv$gewicht, na.rm = T),
                                      mean(aktiv$alter, na.rm = T), 
                                      mean(aktiv$bmi, na.rm = T), 
                                      mean(aktiv$dauer_insuff, na.rm = T), "", ""),
                         Median=c("", "", median(aktiv$groesse, na.rm = T), 
                                  median(aktiv$gewicht, na.rm = T),
                                  median(aktiv$alter, na.rm = T), 
                                  median(aktiv$bmi, na.rm = T), 
                                  median(aktiv$dauer_insuff, na.rm = T), "", ""),
                         Minimum=c("", "", range(aktiv$groesse, na.rm = T)[1], 
                                   range(aktiv$gewicht, na.rm = T)[1],
                                   range(aktiv$alter, na.rm = T)[1], 
                                   range(aktiv$bmi, na.rm = T)[1],
                                   range(aktiv$dauer_insuff, na.rm = T)[1], 
                                   "", ""),
                         Maximum=c("", "", range(aktiv$groesse, na.rm = T)[2], 
                                   range(aktiv$gewicht, na.rm = T)[2],
                                   range(aktiv$alter, na.rm = T)[2], 
                                   range(aktiv$bmi, na.rm = T)[2],
                                   range(aktiv$dauer_insuff, na.rm = T)[2], 
                                   "", ""),
                         Standardabweichung=c("", "", sd(aktiv$groesse, na.rm = T), 
                                              sd(aktiv$gewicht, na.rm = T),
                                              sd(aktiv$alter, na.rm = T), 
                                              sd(aktiv$bmi, na.rm = T),
                                              sd(aktiv$dauer_insuff, na.rm = T), 
                                              "", ""),
                         Standardfehler=c("", "", se(aktiv$groesse), se(aktiv$gewicht),
                                          se(aktiv$alter), se(aktiv$bmi),
                                          se(aktiv$dauer_insuff), "", ""),
                         Varianz=c("", "", var(aktiv$groesse, na.rm = T), 
                                   var(aktiv$gewicht, na.rm = T),
                                   var(aktiv$alter, na.rm = T), 
                                   var(aktiv$bmi, na.rm = T),
                                   var(aktiv$dauer_insuff, na.rm = T), "", ""))

#Placebo
Placebo <- subset(KHKrand, gruppe=="2")

Kenngrößenplacebo <- data.frame(Variablen=c("Männlich", "Weiblich", "Größe", "Gewicht",
                                          "Alter", "BMI", "Dauer", "Herzinfarkt",
                                          "kein Herzinfarkt"),
                              Summe=c(length(subset(Placebo, sex=="1")$sex), 
                                      length(subset(Placebo, sex=="2")$sex), 
                                      "", "", "", "","", 
                                      length(subset(Placebo, myo_infarct=="1")$myo_infarct),
                                      length(subset(Placebo, myo_infarct=="2")$myo_infarct)),
                              Mittelwert=c("", "", mean(Placebo$groesse, na.rm = T), 
                                           mean(Placebo$gewicht, na.rm = T),
                                           mean(Placebo$alter, na.rm = T), 
                                           mean(Placebo$bmi, na.rm = T), 
                                           mean(Placebo$dauer_insuff, na.rm = T), "", ""),
                              Median=c("", "", median(Placebo$groesse, na.rm = T), 
                                       median(Placebo$gewicht, na.rm = T),
                                       median(Placebo$alter, na.rm = T), 
                                       median(Placebo$bmi, na.rm = T), 
                                       median(Placebo$dauer_insuff, na.rm = T), "", ""),
                              Minimum=c("", "", range(Placebo$groesse, na.rm = T)[1], 
                                        range(Placebo$gewicht, na.rm = T)[1],
                                        range(Placebo$alter, na.rm = T)[1], 
                                        range(Placebo$bmi, na.rm = T)[1],
                                        range(Placebo$dauer_insuff, na.rm = T)[1], 
                                        "", ""),
                              Maximum=c("", "", range(Placebo$groesse, na.rm = T)[2], 
                                        range(Placebo$gewicht, na.rm = T)[2],
                                        range(Placebo$alter, na.rm = T)[2], 
                                        range(Placebo$bmi, na.rm = T)[2],
                                        range(Placebo$dauer_insuff, na.rm = T)[2], 
                                        "", ""),
                              Standardabweichung=c("", "", sd(Placebo$groesse, na.rm = T), 
                                                   sd(Placebo$gewicht, na.rm = T),
                                                   sd(Placebo$alter, na.rm = T), 
                                                   sd(Placebo$bmi, na.rm = T),
                                                   sd(Placebo$dauer_insuff, na.rm = T), 
                                                   "", ""),
                              Standardfehler=c("", "", se(Placebo$groesse), se(Placebo$gewicht),
                                               se(Placebo$alter), se(Placebo$bmi),
                                               se(Placebo$dauer_insuff), "", ""),
                              Varianz=c("", "", var(Placebo$groesse, na.rm = T), 
                                        var(Placebo$gewicht, na.rm = T),
                                        var(Placebo$alter, na.rm = T), 
                                        var(Placebo$bmi, na.rm = T),
                                        var(Placebo$dauer_insuff, na.rm = T), "", ""))

#Schiefe & Wölbung
install.packages("moments")
library(moments)
skewness(na.omit(KHK$groesse))
kurtosis(na.omit(KHK$groesse))
