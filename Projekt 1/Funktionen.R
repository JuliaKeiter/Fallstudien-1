## Funktionen

## Zusammenfassung einer Variablen: deskriptive Kennzahlen
## Sowohl Tabellen in R als auch Latex Quellcode der Tabellen.

library(moments)
library(xtable)

kennz_stet <- function(x, latex = FALSE){
  M1 <- matrix(nrow = 6, ncol = 1)
  rownames(M1) <- c("Minimum", "0.25-Quartil", "Arithmetische Mittel", 
                    "Median","0.75-Quartil", "Maximum")
  M2 <- matrix(nrow = 5, ncol = 1)
  rownames(M2) <- c("Varianz","Standardabweichung","Spannweite",
                    "Interquartilsabstand", "MAD")
  M3 <- matrix(nrow = 2, ncol = 1)
  rownames(M3) <- c("Schiefe", "Kurtosis")
  
  # Lagemasse:
  M1[1,1] <- min(x, na.rm = TRUE)
  M1[2,1] <- quantile(x, probs = 0.25, na.rm = TRUE, type = 2) 
  M1[3,1] <- mean(x, na.rm = TRUE)
  M1[4,1] <- median(x, na.rm = TRUE)
  M1[5,1] <- quantile(x, probs = 0.75, na.rm = TRUE, type = 2) 
  M1[6,1] <- max(x, na.rm = TRUE)
  
  # Streuungsmasse:
  M2[1,1] <- var(x, na.rm = TRUE)
  M2[2,1] <- sd(x, na.rm = TRUE)
  M2[3,1] <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  M2[4,1] <- IQR(x, na.rm = TRUE) 
  M2[5,1] <- mad(x, na.rm = TRUE)
  # Schiefe und Woelbung:
  M3[1,1] <- skewness(x, na.rm = TRUE)
  M3[2,1] <- kurtosis(x, na.rm = TRUE)
  
  M4 <- rbind(M1, M2, M3)
  
  if(latex == FALSE){
    return(list("Lagemasse" = M1, "Streuungsmasse" = M2, "Schiefe und Kurtosis" = M3))
  }
  else{
    return(list("Lagemasse" = M1, "Streuungsmasse" = M2, "Schiefe und Kurtosis" = M3,
                xtable(M1), xtable(M2), xtable(M3), xtable(M4) ))
  }
  
}




## Funktion die ein Histogramm erstellt und eine Normalverteilung drueber
## legt. Die Normalverteilung hat dabei den Mittelwert der Variable als
## mu und die Varainz/Standardabweichung als sigma^2 bzw. sigma.



library(rlang)
comp_norm <- function(y, xlab, ...){
  x <- y
  hist(y, freq = FALSE, ylab = "Dichte", xlab = xlab, main = " ", ...)
  curve(dnorm(x, mean= mean(y, na.rm = TRUE), sd= sd(y, na.rm = TRUE)), 
        col="darkblue", lwd=3.5, add=TRUE, yaxt="n")
  mea <- round(mean(y))
  var <- round(var(y), digits = 2)
  legend("topright", 
         legend = expr(paste(italic(N), group( "(", list(!!mea, !!var), ")" ) ) ),
         col = "darkblue", lty = 1, lwd = 3.5)
  
}



