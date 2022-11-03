## Uebungblatt 9
## Louisa Poggel


# 9.1 (a) Datenbeispiele ------------------------------------------------------

set.seed(1714)

x <- 1:50
# beta_0 = beta_1 = 1

# Normalverteilung der Residuen:
y <- 1 + x + rnorm(50, 0, 1)

M2 <- lm ( y ~ x )
M2

plot(M2)



# (i) Heteroskedastizität verletzt:
y <- 1 + x + rnorm(50, 0, 1:50)

M3 <- lm ( y ~ x )
M3

plot(M3)



# (ii) Verletzung Normalverteilungsannahme:

y <- 1 + x + rexp(50, rate = 0.5)

M4 <- lm( y ~ x)

plot(M4)


# (iii) Ausreisser:

y <- 1 + x + rnorm(50, 0, 1)
plot(y)
y[20] <- 50


M5 <- lm( y ~ x)

plot(M5, which = 4)

?plot.lm

# (iv) autoregressive Fehler mit positiver Korrelation:

#set.seed( 0906 )
set.seed(1714)
e <- rnorm( 1, 0, 1)
for( i in 2:50){
  e[i] <- 0.9 * e[i-1] + rnorm( 1, 0, 1)
}
e

y <- 1 + x + e

M6 <- lm (y ~ x)
plot(M6)


# (v) autoregressive Fehler mit neagtiver Korrelation:

e <- rnorm( 1, 0, 1)
for( i in 2:50){
  e[i] <- -0.9 * e[i-1] + rnorm( 1, 0, 1)
}
e

y <- 1 + x + e

M7 <- lm (y ~ x)
plot(M7)


# (vi) Nichtlinearer Zusammenhang:

y <- log( 1 + x + rnorm(50, 0, 1) )

M8 <- lm( y ~ x )
plot(M8)



# (b) Vergleich Modell mit NV-Annahme und verletzten Annahmen -------------

# Residuen gegen die Fitted values
# Optimalfall: Residuen streuen zufaellig ohne Strukur um Null

op <- par( mfcol = c(2, 4) )

plot(M2, which = 1, main = "NV-Annahme")
# Optimalfall

plot(M3, which = 1, main = "Heteroskedastizitaet")
# Residuen werden immer groesser (Da Varianzen immer groesser gewaehlt)

plot(M4, which = 1, main = "Verletzung NV-Annahme")
# Mehr negative, kleinerer Residuen und wenige grosse, positive Residuen.

plot(M5, which = 1, main = "Ausreisser")
# Ein Wert der stark von allen anderen Residuen abweicht.

plot(M6, which = 1, main = "positive Korrelation")
# wellige, periodische, zick-zack-maessige Struktur

plot(M7, which = 1, main = "negative Korrelation")
# nahzu symmetrische Strukturen der Residuen um Null rum
# dennoch ergeben die Residuen im Mittel Null

plot(M8, which = 1, main = "nichtlinear")
# deutliche nichtlineare Struktur zu erkennen



# QQ-plot:
# Potimalfall: Die Standardisierten Residuen liegen moeglichst nahe an der Winkel-
#              halbierenden.

# st.Residuen = standardisierte Residuen

# In rot die Winkelhalbierende

op <- par( mfcol = c(2, 4) )

plot(M2, which = 2, main = "NV-Annahme")
abline( 0, 1 , col = "red", lwd = 2)
# Optimalfall (nahezu die Winkelhalbierende getroffen)

plot(M3, which = 2, main = "Heteroskedastizitaet")
abline( 0, 1 , col = "red", lwd = 2)
# st.Residuen weichen gerade im negativen Bereich des theoretischen Quantil
# ab (groessere negative st.Residuen).

plot(M4, which = 2, main = "Verletzung NV-Annahme")
abline( 0, 1 , col = "red", lwd = 2)
# Grosse Abweichungen der st.Residuen im postiven Bereich

plot(M5, which = 2, main = "Ausreisser")
abline( 0, 1 , col = "red", lwd = 2)
# Ein deutlich abweichendes st.Residuum erkennbar (oberer Quantilbereich)

plot(M6, which = 2, main = "positive Korrelation")
abline( 0, 1 , col = "red", lwd = 2)
# st.Residuen weichen im oberen/unteren Quantilsbereich etwas mehr ab

plot(M7, which = 2, main = "negative Korrelation")
abline( 0, 1 , col = "red", lwd = 2)
# Sieht dem Optimalfall sehr aehnlich. Jedoch sind Abweichungen im oberen
# und unteren Bereich des theoretischen Quantil erkennbar

plot(M8, which = 2, main = "nichtlinear")
abline( 0, 1 , col = "red", lwd = 2)
# nichtlinearer Verlauf der st.Residuen



# (ab hier war ich mir nicht mehr sicher bei der Interpretation:)

# Wurzel der st.Residuen in Abhaengigkeit der angepassten y-Werte
# Optimalfall: Zufaellige Struktur, im Mittel zwischen 0.5 und 1

op <- par( mfcol = c(2, 4) )

plot(M2, which = 3, main = "NV-Annahme")
plot(M3, which = 3, main = "Heteroskedastizitaet")
plot(M4, which = 3, main = "Verletzung NV-Annahme")
plot(M5, which = 3, main = "Ausreisser")
plot(M6, which = 3, main = "positive Korrelation")
plot(M7, which = 3, main = "negative Korrelation")
plot(M8, which = 3, main = "nichtlinear")


# Cooks distance
# Optimalfall: Grosse und kleine Werte sind gleichmaessig ueber die Beobachtungen
#              verteilt.

op <- par( mfcol = c(2, 4) )

plot(M2, which = 4, main = "NV-Annahme")
plot(M3, which = 4, main = "Heteroskedastizitaet")
plot(M4, which = 4, main = "Verletzung NV-Annahme")
plot(M5, which = 4, main = "Ausreisser")
plot(M6, which = 4, main = "positive Korrelation")
plot(M7, which = 4, main = "negative Korrelation")
plot(M8, which = 4, main = "nichtlinear")

# Standardisierte Residuen in Abhaengigkeit des Leverage
# Optimalfall: st. Residuen sind gleichmaessig ueber grosse und kleine Leverage-Werte
#              verteilt

op <- par( mfcol = c(2, 4) )

plot(M2, which = 5, main = "NV-Annahme")
plot(M3, which = 5, main = "Heteroskedastizitaet")
plot(M4, which = 5, main = "Verletzung NV-Annahme")
plot(M5, which = 5, main = "Ausreisser")
plot(M6, which = 5, main = "positive Korrelation")
plot(M7, which = 5, main = "negative Korrelation")
plot(M8, which = 5, main = "nichtlinear")


# Cooks distance in Abhaengigkeit vom Leverage

op <- par( mfcol = c(2, 4) )

plot(M2, which = 6, main = "NV-Annahme")
plot(M3, which = 6, main = "Heteroskedastizitaet")
plot(M4, which = 6, main = "Verletzung NV-Annahme")
plot(M5, which = 6, main = "Ausreisser")
plot(M6, which = 6, main = "positive Korrelation")
plot(M7, which = 6, main = "negative Korrelation")
plot(M8, which = 6, main = "nichtlinear")




# (9.2) -------------------------------------------------------------------

# Bei (9.2) 

# (a) ---------------------------------------------------------------------

library( MASS )

# Fuer jede Automarke wird versucht ein Parameter zu schaetzten (da Make ein Faktor
# mt 93 leveln ist). Jedoch wird fuer jede Marke NA geschaetzt.
# Zu viele level um eine Schaetzung durchzufuehren (?)
# Ausserdem sind qualitative Faktoren nicht fuer Regressionsanalyse 
# geeignet -> quantitative Regressoren noetig
L1 <- lm( Price ~ . , data = Cars93)
str(Cars93)
# nur Na beim Luggage.room und Rear.seat.room vorhanden
sapply( Cars93 , function(x){ sum(is.na(x)) } )


# (b) ---------------------------------------------------------------------

cars <- Cars93[ ,-c(1:3,9:11,16,26:27) ]
str(cars)
# noch Luggage.room und Rear.seat.room entfernen:
cars <- cars[ , -c(16:17)]
# Nun werden alle Parameter geschaetzt und keine NA entstehen
L2 <- lm( Price ~ ., data  = cars)



# (c) ---------------------------------------------------------------------


step( L2, direction = "forward" )
step( L2, direction = "backward" )
step( L2, direction = "both" )



# (d) ---------------------------------------------------------------------


step( L2, direction = "forward", citerion = "BIC" )
step( L2, direction = "backward", citerion = "BIC" )
step( L2, direction = "both", citerion = "BIC" )



# (e) ---------------------------------------------------------------------

# install.packages( "olsrr")

library( "olsrr")

L3 <- lm(Price ~ MPG.highway + Horsepower + Passengers + Length + Wheelbase 
         + Width + Weight, data = cars)
k <- ols_step_all_possible( L3 )
k

str(k)

# Idee/Ansatz:
# ein optimales Modell sollte eim Cp kleiner als 8 haben
# denn bei der maximalen Anzahl an Variablen, also 7, ist ein Wert
# kleiner als 8 noch okay.
op <- k[ which( k$cp <  8 ) , ]
op

# kleinste Wert am besten, denn wenn das cp sehr nahe an
# n leigt, ist der Bruch klein und dann die Anpassung am besten
which.min( op$cp/ op$n)
which.min( op$n / op$cp )

# 
op[11, ]
# Index N                                               Predictors  R-Square
# 120 6 MPG.highway Horsepower Passengers Length Wheelbase Width 0.7186251
# Adj. R-Square Mallow's Cp
#  0.6989943    6.512295
# 
















