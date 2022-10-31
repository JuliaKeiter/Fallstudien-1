library("moments")
library("xtable")

#Einlesen der Daten
miete <- read.csv("mietspiegel2015.csv", sep = " ")

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

miete$wohngut <- as.factor(miete$wohngut)
miete$wohnbest <- as.factor(miete$wohnbest)
miete$ww0 <- as.factor(miete$ww0)
miete$zh0 <- as.factor(miete$zh0)
miete$badkach0 <- as.factor(miete$badkach0)
miete$badextra <- as.factor(miete$badextra)
miete$kueche <- as.factor(miete$kueche)
miete$bez <- gsub("Ã¶", "ö", miete$bez)
miete$bez <- as.factor(miete$bez)

# Deskriptive Kenngroessen zu den numerischen Variablen:
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

# Verteilung der interessierenden Variablen:

# Nettomiete pro Monat (rechtsschief)
summary(miete$nm)
hist(miete$nm, freq = FALSE)
boxplot(miete$nm)

# (ohne) Ausreisser
miete[1975, ]
hist(miete$nm[-1975], freq = FALSE)
boxplot(miete$nm[-1975])


# Nettomiete pro Monat und Quadratmeter (symmetrisch)
summary(miete$nmqm)
hist(miete$nmqm, freq = FALSE)
boxplot(miete$nmqm)

# (ohne) Ausreisser
miete[1975, ]
hist(miete$nmqm[-1975], freq = FALSE)
boxplot(miete$nmqm[-1975])

# Wohnflaeche in qm     (rechtssschief)
summary(miete$wfl)
hist(miete$wfl, freq = FALSE)
boxplot(miete$wfl)


# Anzahl Raueme        (meisten Raeume zwischen 2 und 3 zimmer)
summary(miete$rooms)
hist(miete$rooms, freq = FALSE)
boxplot(miete$rooms)
barplot(table(miete$rooms))


# Baujahr 
# Umwandeln in eine Zeitdifferenz?
summary(miete$bj)
hist(miete$bj, freq = FALSE)
boxplot(miete$bj)

zeitdiff <- 2022 - miete$bj
summary(zeitdiff)
hist(zeitdiff, freq = FALSE)
boxplot(zeitdiff)

# gute Wohnlage              (nicht oft vorhanden)
summary(miete$wohngut)
table(miete$wohngut)/length(miete$wohngut)
barplot(table(miete$wohngut))


# beste Wohnlage             (bei nur einem sehr kleinen Anteil gegebn)
summary(miete$wohnbest)
table(miete$wohnbest)/length(miete$wohnbest)
barplot(table(miete$wohnbest))


# Warmwasserversorgung       (bei nur einem sehr kleinen Anteil gegebn)
summary(miete$ww0)
table(miete$ww0)/length(miete$ww0)
barplot(table(miete$ww0))


# Zentralheizung             (bei nur einem sehr kleinen Anteil gegebn)
summary(miete$zh0)
table(miete$zh0)/length(miete$zh0)
barplot(table(miete$zh0))

# Badezimmerkacheln           
summary(miete$badkach0)
table(miete$badkach0)/length(miete$badkach0)
barplot(table(miete$badkach0))


# Badausstattung              (nicht oft vorhanden)
summary(miete$badextra)
table(miete$badextra)/length(miete$badextra)
barplot(table(miete$badextra))

# Kuechenausstattung        (nicht oft vorhanden, etwa 1/4)
summary(miete$kueche)
table(miete$kueche)/length(miete$kueche)
barplot(table(miete$kueche))


# Bezirksname               (meisten in Neuhausen-Nymphenburg)
summary(miete$bez)
table(miete$bez)/length(miete$bez)
par( mar = c(12, 3.5, 2, 2))
barplot(sort(table(miete$bez)/length(miete$bez)), las=2)

dev.off()

#limo schlimo

mietelm1 <- lm(nm ~ ., data = miete[ ,-2])
plot(mietelm1, which = 1) #residual vs fitted
plot(mietelm1, which = 2) #q-q-plots
plot(mietelm1, which = 3) #scale-location
plot(mietelm1, which = 4) #cooks distance
plot(mietelm1, which = 5) #leverage

det(t(model.matrix(mietelm1))%*%model.matrix(mietelm1)) #multikollineariteat

step(mietelm1)
#AIC, BIC, adjustiertes Bestimmtheitsmaß, Mallow Cp
#vorwaertsselektion, rueckwaertselimination, schrittweise regression