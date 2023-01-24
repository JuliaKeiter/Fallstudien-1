setwd("~/Documents/Uni/6. Semester/Fallstudien/Projekt 5")
library(factoextra)
library(cluster)
library(scatterplot3d)
library(NbClust)
library(xtable)
library(readr)

#Einlesen der Daten####
Exo <- read_csv("Exoplaneten.csv")[,-1]

#fehlende Werte?
sum(is.na(Exo))
#[1] 0

#Beobachtung 56 
Exo$mass[56]
#[1] 0.05
#laut S. 236 in Mayor fehlerhaft => Korrektur
Exo$mass[56] <- 2.05

#Deskriptive Analyse####
#univariate Analyse
erstell_tabelle <- function(daten){
  speicher <- numeric(10)
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
  names(speicher) <- c("arithm. Mittel", "Median", "Minimum", "Maximum",
                       "Spannweite", "1.Quartil", "3.Quartil", "IQR",
                       "Standardabw.", "MAD")
  return(speicher)
}

lapply(Exo, erstell_tabelle)
'$mass
arithm. Mittel         Median        Minimum        Maximum     Spannweite      1.Quartil      3.Quartil 
      3.347089       1.830000       0.120000      17.500000      17.380000       0.990000       4.140000 
           IQR   Standardabw.            MAD 
      3.150000       3.668151       1.779120 

$period
arithm. Mittel         Median        Minimum        Maximum     Spannweite      1.Quartil      3.Quartil 
      666.5313       337.1100         2.9850      5360.0000      5357.0150        44.2800      1089.0000 
           IQR   Standardabw.            MAD 
     1044.7200       873.7498       483.6375 

$eccen
arithm. Mittel         Median        Minimum        Maximum     Spannweite      1.Quartil      3.Quartil 
     0.2815475      0.2700000      0.0000000      0.9270000      0.9270000      0.1000000      0.4100000 
           IQR   Standardabw.            MAD 
     0.3100000      0.2109435      0.2357334  '

#grafisch
pairs(Exo)

#drei dimensionale Darstellung
#von rechts
scatterplot3d(Exo)
#von links
scatterplot3d(Exo, angle=-200)
#um 90 grad gedreht
scatterplot3d(Exo, angle=200)
#von oben
scatterplot3d(Exo, angle=100)

#Skalierung####
#Skalieren Mean = 0, SD=1, für euklidischen Abstand, da Gewichtung der Daten sonst verschieden
Exo2 <- scale(Exo)
head(Exo2)
'          mass     period        eccen
[1,] -0.8797590 -0.7571747 -1.334706067
[2,] -0.8587675 -0.7582951 -1.334706067
[3,] -0.8552235 -0.7121618  0.277100193
[4,] -0.8524973 -0.6760874 -0.007336206
[5,] -0.8497711 -0.7555117 -0.955457535
[6,] -0.8443188 -0.7593790 -1.239893934'

#Distanzmatrix
#Euklidisch 
d_eu <- dist(Exo2, method = "euclidean")
#Manhattan 
d_man <- dist(Exo2, method = "manhattan")

########################################################################################

#Hierarchische Clusteranalyse####

#euklidische Distanz

#Evaluation: Kein Einpunkt Cluster, Clustergruppen sollen gleich groß sein, subjektives Kriterium von Svetlana
#hängt von Kontext ab, Homogenität und Heterogenitätskriterium wichtiger
# aus Statistik und Datascience Sicht, deswegen k=2 relativieren, in Clusteranalyse keine beste Lösung
#Ausreißerbegriff vermeiden (besser: Extremwert)
#argumentieren warum k=2 nicht so schön ist
# wegen Ellenbogenplot k=4, Varianz verändert sich nach k=2 nochmal stark, im Plot
#mit Ward: keine große Veränderung wenn Ausreißer entfernt, Svetlana entscheidet k=4

#3 dimensionale Darstellung mit scatter3D 

# Berechnen Sie den Agglomerationskoeffizienten für jede Clustering-Verknüpfungsmethode
#mit euklidischer Distanz
agnes(d_eu, method="average")
#Agglomerative coefficient:  0.9124792
agnes(d_eu, method="single")
#Agglomerative coefficient:  0.8735438
agnes(d_eu, method="complete")
#Agglomerative coefficient:  0.9324591
agnes(d_eu, method="ward")
#Agglomerative coefficient:  0.9609799 *
#Agglomerationskoeffizient mit Ward Methode am höchsten

#mit manhattan
agnes(d_man, method="average")
#Agglomerative coefficient:  0.9058054
agnes(d_man, method="single")
#Agglomerative coefficient:  0.9357305
agnes(d_man, method="complete")
#Agglomerative coefficient:  0.9357305
agnes(d_man, method="ward")
#Agglomerative coefficient:  0.9661593 *
#Agglomerationskoeffizient mit Ward Methode am höchsten, höher als bei euklidischer
#Distanz, Manhattan Distanz wird gewählt

#hierarchisches Clustering mit der Ward-Methode durchführen
final_hier <- agnes(d_man, method="ward")

#Dendogram
par(mfrow=c(1,1))
par(mar = c(5, 4, 6, 0) + 0.2)
plot(final_hier, which.plots = 2)

#Bestimmung der optimalen Anzahl an Clustern

#Methode 1: Anzahl der Cluster im Vergleich zur Summe innerhalb der Quadratsumme
fviz_nbclust(Exo2, hcut, method = "wss")
#4

#Methode 2: 
#Silhouettenkoeffizient

sc_agnes <- function(x){
  final_hier <- agnes(d_man, method="ward")
  cut_hier <- vector("list", 9)
  for(i in 2:10){
    cut_hier[[i]] <- cutree(final_hier, k = i)
  }
  
  #a vor average silhouette width 
  a <- rep(0, 9)
  for(i in 2:10){
    a[i] <- mean(silhouette(cut_hier[[i]], d_man)[,3])
  }
  av <- data.frame(k= c(1:10), av = a)
  
  #sort by average silhouette width
  df <-av[order(av$av, decreasing = T),]
  return( df[1,])
}

sc_agnes(Exo2)
'  k        av
   5 0.3710683'

#uneinig

#Übersicht
NbClust(Exo2, distance = "manhattan", method = "ward.D2", index="alllong", max.nc=10)$Best.nc

'* Among all indices:                                                
* 5 proposed 2 as the best number of clusters 
* 3 proposed 3 as the best number of clusters 
* 7 proposed 4 as the best number of clusters 
* 4 proposed 5 as the best number of clusters 
* 1 proposed 7 as the best number of clusters 
* 2 proposed 9 as the best number of clusters 
* 5 proposed 10 as the best number of clusters 

                   ***** Conclusion *****                            
 
* According to the majority rule, the best number of clusters is  4 
 
 
******************************************************************* 
                     KL      CH Hartigan     CCC  Scott  Marriot   TrCovW  TraceW Friedman  Rubin Cindex      DB
Number_clusters  4.0000  4.0000   4.0000 10.0000  3.000      4.0    3.000  4.0000  10.0000  4.000 7.0000 10.0000
Value_Index     12.7575 55.7066  28.2125  0.2327 95.416 110878.1 1928.688 33.5576   3.7901 -0.427 0.2177  0.9394
                Silhouette   Duda PseudoT2 Beale Ratkowsky    Ball PtBiserial     Gap Frey McClain   Gamma
Number_clusters     5.0000 2.0000   2.0000 2.000    4.0000  3.0000     5.0000  2.0000    1  2.0000 10.0000
Value_Index         0.3711 0.5265  35.9799 1.494    0.3974 52.7617     0.6068 -0.4007   NA  0.6027  0.8361
                 Gplus      Tau   Dunn Hubert SDindex Dindex   SDbw
Number_clusters 10.000   5.0000 9.0000      0  5.0000      0 9.0000
Value_Index     49.284 925.4671 0.0792      0  2.2887      0 0.3002'


#Entscheidung für k=4

#Schneiden des Dendrogramms in 2 Cluster
cut_hier <- cutree(final_hier, k = 4)
table(cut_hier)
' 1  2  3  4 
 53 29  6 13'

#Silhouettenplot
par(mfrow=c(1,1))
plot(silhouette(cut_hier, d))
#keine negative silhoutte width, scheinbar keine falschen Zuordnungen

#geschnittenes Dendogram
par(mfrow=c(1,1))
plot(final_hier, which.plots = 2)
rect.hclust(as.hclust(final_hier), k=4, border = 6)

#alternative grafische Darstellung: endgültigen hier-Modell plotten, jeweils 2 dimensional
#von vorne
fviz_cluster(list(data =Exo, cluster = cut_hier, repel = TRUE), axes = c(1,2))
#von rechts 
fviz_cluster(list(data =Exo, cluster = cut_hier, repel = TRUE), axes = c(3,2))
#von rechts unten
fviz_cluster(list(data =Exo, cluster = cut_hier, repel = TRUE), axes = c(3,1))

#Clusterbezeichnung zu Exo hinzufügen
Exo <- cbind(Exo, cluster1 = cut_hier)

#Univariate Kenngrößen pro Variable und Cluster nach hierarchischem Verfahren####
aggregate(Exo$mass, by = list(cluster1=Exo$cluster1), erstell_tabelle)
'  cluster1 x.arithm. Mittel  x.Median x.Minimum x.Maximum x.Spannweite x.1.Quartil x.3.Quartil     x.IQR
1        1         1.665396  1.120000  0.120000  6.292000     6.172000    0.680000    2.080000  1.400000
2        2         3.292759  3.030000  0.400000  8.640000     8.240000    1.550000    4.290000  2.740000
3        3         1.725000  1.080000  0.760000  4.000000     3.240000    0.912500    2.200000  1.287500
4        4        11.073077 10.370000  7.390000 17.500000    10.110000    8.000000   11.980000  3.980000
  x.Standardabw.     x.MAD
1       1.483467  1.037820
2       2.273590  2.179422
3       1.291770  0.385476
4       3.369892  3.513762'
aggregate(Exo$period, by = list(cluster1=Exo$cluster1), erstell_tabelle)
'  cluster1 x.arithm. Mittel    x.Median   x.Minimum   x.Maximum x.Spannweite x.1.Quartil x.3.Quartil       x.IQR
1        1       312.746061   71.487000    2.985000 1444.500000  1441.515000    6.403000  454.000000  447.597000
2        2       523.679552  401.100000   10.901000 1764.000000  1753.099000  221.600000  743.000000  521.400000
3        3      2868.833333 2556.000000 1942.000000 5360.000000  3418.000000 2268.250000 2609.000000  340.750000
4        4      1411.108323 1582.000000    8.428198 3030.000000  3021.571802  653.220000 2115.200000 1461.980000
  x.Standardabw.       x.MAD
1     417.973385  101.402427
2     444.718712  317.572920
3    1248.688659  318.017700
4     930.598105  899.938200'
aggregate(Exo$eccen, by = list(cluster1=Exo$cluster1), erstell_tabelle)
'  cluster1 x.arithm. Mittel  x.Median x.Minimum x.Maximum x.Spannweite x.1.Quartil x.3.Quartil     x.IQR
1        1        0.1359113 0.1243000 0.0000000 0.3500000    0.3500000   0.0300000   0.2300000 0.2000000
2        2        0.5111034 0.5110000 0.3100000 0.9270000    0.6170000   0.4000000   0.6200000 0.2200000
3        3        0.2400000 0.1700000 0.0000000 0.6000000    0.6000000   0.1150000   0.3450000 0.2300000
4        4        0.3823846 0.3700000 0.2200000 0.6200000    0.4000000   0.3140000   0.4290000 0.1150000
  x.Standardabw.     x.MAD
1      0.1117333 0.1546352
2      0.1454494 0.1645686
3      0.2201817 0.1779120
4      0.1193472 0.0874734'


Exo_hier_1 <- subset(Exo, cluster1==1)
Exo_hier_2 <- subset(Exo, cluster1==2)
Exo_hier_3 <- subset(Exo, cluster1==3)
Exo_hier_4 <- subset(Exo, cluster1==4)

############################################################################################################


#Partitionierende Clusteranalyse####

# 2 zu niedrig laut Justin, 4 lokales Maximum 
#verschiedene wählen, anhand von Silhouetenplot vergleichen

#k-means Clusteranalyse####

#optimale Anzahl an Clustern finden
#Methode 1: Scree Plot
fviz_nbclust(Exo2, kmeans, method = "wss", k.max = 10)
#4

#Methode 2: Silhouettenkoeffizient
sc_kmeans <- function(x, d){
  final_kmeans <- vector("list", 9)
  for(i in 2:10){
    set.seed(111625)
    final_kmeans[[i]] <- kmeans(x, centers=i, algorithm = "MacQueen", nstart=55)
  }
  #a vor average silhouette width 
  a <- rep(0, 9)
  for(i in 2:10){
  a[i] <- mean(silhouette(final_kmeans[[i]]$cluster, d)[,3])
  }
  av <- data.frame(k= c(1:10), av = a)
  
  #sort by average silhouette width
  df <-av[order(av$av, decreasing = T),]
  return(df[1,])
}
#Euklidische Distanz
sc_kmeans(Exo2, d_eu)
' k        av
2 2 0.4154583'
#2
#Manhattan Distanz
sc_kmeans(Exo2, d_man)
' k        av
2 2 0.4265462'*
#2, Entscheidung für Manhattan Distanz

#Uneinig über Clusteranzahl
#Übersicht
NbClust(Exo2, distance = "manhattan", method = "kmeans", index="alllong", max.nc=10)$Best.nc
'* Among all indices:                                                
* 10 proposed 2 as the best number of clusters 
* 5 proposed 3 as the best number of clusters 
* 3 proposed 4 as the best number of clusters 
* 1 proposed 5 as the best number of clusters 
* 1 proposed 7 as the best number of clusters 
* 1 proposed 9 as the best number of clusters 
* 7 proposed 10 as the best number of clusters 

                   ***** Conclusion *****                            
 
* According to the majority rule, the best number of clusters is  2 
 
 
******************************************************************* 
                    KL      CH Hartigan     CCC    Scott  Marriot   TrCovW  TraceW Friedman   Rubin Cindex
Number_clusters 4.0000 10.0000  10.0000 10.0000   3.0000      3.0    3.000  4.0000   9.0000  5.0000 3.0000
Value_Index     2.6263 57.0808  17.5443  1.6619 106.3136 225375.3 2481.581 16.2127   4.5473 -0.2723 0.1734
                     DB Silhouette   Duda PseudoT2  Beale Ratkowsky    Ball PtBiserial    Gap   Frey McClain
Number_clusters 10.0000     2.0000 2.0000    2.000 2.0000    2.0000  3.0000     2.0000  2.000 2.0000  2.0000
Value_Index      0.9844     0.4265 0.9279    5.132 0.1278    0.4173 48.5364     0.6061 -0.337 1.6477  0.3625
                 Gamma   Gplus      Tau   Dunn Hubert SDindex Dindex    SDbw
Number_clusters 10.000 10.0000   2.0000 7.0000      0  4.0000      0 10.0000
Value_Index      0.885 35.0176 919.2531 0.0858      0  2.2608      0  0.2466'

#k-means Verfahren mit k=2
set.seed(111625)
final_kmeans <- kmeans(Exo2, centers=2, algorithm = "MacQueen", nstart=98)
table(final_kmeans$cluster)
'1  2 
71 30 '

#average silhouette width für k=2
par(mfrow=c(1,1))
plot(silhouette(final_kmeans$cluster, d_man)) 

#endgültigen k-Means-Modell plotten, jeweils 2 dimensional
#von vorne
fviz_cluster(final_kmeans, data = Exo, axes = c(1,2))
#von rechts 
fviz_cluster(final_kmeans, data = Exo, axes = c(3,2))
#von rechts unten
fviz_cluster(final_kmeans, data = Exo, axes = c(3,1))

#k-mean Clusterzuordnung zu Exo hinzufügen
Exo <- cbind(Exo, cluster2 = final_kmeans$cluster)

#univariate Kenngrößen pro Variable pro Cluster nach k-means Verfahren#####
aggregate(Exo$mass, by = list(cluster2=Exo$cluster2), erstell_tabelle)
'  cluster2 x.arithm. Mittel  x.Median x.Minimum x.Maximum x.Spannweite x.1.Quartil x.3.Quartil     x.IQR x.Standardabw.
1        1         1.677690  1.230000  0.120000  6.292000     6.172000    0.790000    2.160000  1.370000       1.369017
2        2         7.298000  7.405000  0.880000 17.500000    16.620000    4.157500    9.925000  5.767500       4.347725
      x.MAD
1  0.993342
2  4.633125'
aggregate(Exo$period, by = list(cluster2=Exo$cluster2), erstell_tabelle)
' cluster2 x.arithm. Mittel    x.Median   x.Minimum   x.Maximum x.Spannweite x.1.Quartil x.3.Quartil       x.IQR
1        1       399.491158  228.520000    2.985000 2614.000000  2611.015000   14.485000  449.300000  434.815000
2        2      1298.526140 1209.450000    8.428198 5360.000000  5351.571802  556.987500 1757.875000 1200.887500
  x.Standardabw.       x.MAD
1     571.818929  320.360208
2    1120.497587  957.944925'
aggregate(Exo$eccen, by = list(cluster2=Exo$cluster2), erstell_tabelle)
'   cluster2 x.arithm. Mittel  x.Median x.Minimum x.Maximum x.Spannweite x.1.Quartil x.3.Quartil     x.IQR x.Standardabw.
1        1        0.2004408 0.1900000 0.0000000 0.6490000    0.6490000   0.0480000   0.3200000 0.2720000      0.1673528
2        2        0.4735000 0.4395000 0.1600000 0.9270000    0.7670000   0.3432500   0.6150000 0.2717500      0.1778022
      x.MAD
1 0.2075640
2 0.1823598'

################################################################################

#k-Medoid Clusteranalyse####

#optimale Anzahl an Clustern finden

#Methode 1: scree plot
fviz_nbclust(Exo2, pam, method = "wss")
#5

#Methode 2: Silhouettenkoeffizient
sc_kmed <- function(x,d){
  final_kmed <- vector("list", 9)
  for(i in 2:10){
    final_kmed[[i]] <- pam(x, k=i)
  }
  #a vor average silhouette width 
  a <- rep(0, 9)
  for(i in 2:10){
    a[i] <- mean(silhouette(final_kmed[[i]]$clustering,d)[,3])
  }
  av <- data.frame(k= c(1:10), av = a)
  
  #sort by average silhouette width
  df <-av[order(av$av, decreasing = T),]
  return(df[1,])
}
#euklidische Distanz
sc_kmed(Exo2, d_eu)
'  k        av
2  2 0.3959484'

#manhattan Distanz
sc_kmed(Exo2, d_man)
'  k        av
2 2 0.3961962'*
#SC mit Manhattan Distanz höher

#k=2 oder k=5?
#K-Medoid Verfahren für k=2
final_kmed_2 <- pam(Exo2, k=2, metric = "manhattan", stand = FALSE)
par(mfrow=c(1,2))
plot(silhouette(final_kmed_2$clustering,d))

#K-Medoid Verfahren für k=5
final_kmed_5 <- pam(Exo2, k=5, metric = "manhattan", stand = FALSE)
plot(silhouette(final_kmed_5$clustering,d))

#Entscheidung für k=2
table(final_kmed_2$clustering)
'1  2 
75 26'

#endgültigen k-Means-Modell plotten, jeweils 2 dimensional
#von vorne
fviz_cluster(final_kmed_2, data = Exo, axes = c(1,2))
#von rechts 
fviz_cluster(final_kmed_2, data = Exo, axes = c(3,2))
#von rechts unten
fviz_cluster(final_kmed_2, data = Exo, axes = c(3,1))

#k-med Clusterzuordnung zu Exo hinzufügen
Exo <- cbind(Exo, cluster3 = final_kmed_2$clustering)

#univariate Kenngrößen pro Variable pro Cluster nach k-med Verfahren#####
aggregate(Exo$mass, by = list(cluster3=Exo$cluster3), erstell_tabelle)
'   cluster3 x.arithm. Mittel  x.Median x.Minimum x.Maximum x.Spannweite x.1.Quartil x.3.Quartil     x.IQR
1        1           1.946213  1.240000  0.120000 14.400000    14.280000    0.845000    2.390000  1.545000
2        2           7.388077  7.405000  0.880000 17.500000    16.620000    4.342500    9.435000  5.092500
  x.Standardabw.     x.MAD
1       2.203992  1.008168
2       4.070967  4.084563'
aggregate(Exo$period, by = list(cluster3=Exo$cluster3), erstell_tabelle)
'  cluster3 x.arithm. Mittel  x.Median x.Minimum x.Maximum x.Spannweite x.1.Quartil x.3.Quartil     x.IQR
1         1         387.1179  221.6000    2.9850 2614.0000    2611.0150     14.4850    449.3000  434.8150
2         2        1472.5314 1272.7500   58.1160 5360.0000    5301.8840    727.6650   1897.5000 1169.8350
  x.Standardabw.     x.MAD
1       557.9029  312.6507
2      1105.7130  955.3726'
aggregate(Exo$eccen, by = list(cluster3=Exo$cluster3), erstell_tabelle)
'  cluster3 x.arithm. Mittel  x.Median x.Minimum x.Maximum x.Spannweite x.1.Quartil x.3.Quartil     x.IQR
1         1        0.2273907 0.2000000 0.0000000 0.9270000    0.9270000   0.0600000   0.3450000 0.2850000
2         2        0.4377692 0.4195000 0.0110000 0.7100000    0.6990000   0.3432500   0.5522500 0.2090000
  x.Standardabw.     x.MAD
1      0.1954437 0.2075640
2      0.1753324 0.1593795'


#Endgültige Daten anzeigen
head(Exo)
'   mass period eccen cluster1 cluster2 cluster3
1 0.120  4.950  0.00        1        1        1
2 0.197  3.971  0.00        1        1        1
3 0.210 44.280  0.34        1        1        1
4 0.220 75.800  0.28        1        1        1
5 0.230  6.403  0.08        1        1        1
6 0.250  3.024  0.02        1        1        1'

#############################################################################

#Vergleich der Clusteringmethoden#### 

#hierarchisch und k-means
table(Exo$cluster1, Exo$cluster2)
'Verfahren 2   1  2 
 Verfahren 1
  1            53  0
  2            15 14
  3             3  3
  4             0 13'

#hierarchisch und k-med
table(Exo$cluster1, Exo$cluster3)
'Verfahren 2    1  2
 Verfahren 1
  1            52  1
  2            18 11
  3             3  3
  4             2 11'

#hierarchisch und k-med
table(Exo$cluster2, Exo$cluster3)
'Verfahren 2    1  2
 Verfahren 1
  1            70  1
  2             5 25'

#es scheint die selbe Nummerierungsreihenfolge verwendet worden sein

#wie oft gleiche Clusterisierung?
table(Exo$cluster1 == Exo$cluster2)
'FALSE  TRUE 
   34    67'

table(Exo$cluster1 == Exo$cluster3)
'FALSE  TRUE 
   38    63'

table(Exo$cluster2 == Exo$cluster3)
'FALSE  TRUE 
    6    95'

#########################################################################################################
#Latex Ausgabe

#Tabelle 1: Deskriptive Auswertung
xtable(caption = "Univariate Kenngrößen für \textit{Jupitermasse}, 
       \textit{Umlaufzeit}, \textit{Umlaufform}",
       cbind("Jupitermasse" = erstell_tabelle(Exo$mass),
             "Umlaufzeit" = erstell_tabelle(Exo$period),
             "Umlaufform" = erstell_tabelle(Exo$eccen)),
       digits = 3)

#Tabelle 3: NbClust Ausgabe hierarchisch
xtable(NbClust(Exo2, distance = "manhattan", method = "ward.D2", index="alllong", max.nc=10)$Best.nc)

#Tabelle4: Deskription von 4 Clustern auf hierarchischer Methode

xtable(caption = "Univariate Kenngrößen \textit{Jupitermasse}, 
       \textit{Umlaufzeit}, \textit{Umlaufform} des Cluster Nr 1. aus hierarchischer Clusterung",
       cbind("Jupitermasse" = c(length(Exo_hier_1$mass),erstell_tabelle(Exo_hier_1$mass)),
             "Umlaufzeit" = c(length(Exo_hier_1$period), erstell_tabelle(Exo_hier_2$period)),
             "Umlaufform" = c(length(Exo_hier_1$eccen),erstell_tabelle(Exo_hier_1$eccen))),
       digits = 3)

xtable(caption = "Univariate Kenngrößen \textit{Jupitermasse}, 
       \textit{Umlaufzeit}, \textit{Umlaufform} des Cluster Nr 1. aus hierarchischer Clusterung",
       cbind("Jupitermasse" = erstell_tabelle(Exo_hier_2$mass),
             "Umlaufzeit" = erstell_tabelle(Exo_hier_2$period),
             "Umlaufform" = erstell_tabelle(Exo_hier_2$eccen)),
       digits = 3)

xtable(caption = "Univariate Kenngrößen \textit{Jupitermasse}, 
       \textit{Umlaufzeit}, \textit{Umlaufform} des Cluster Nr 1. aus hierarchischer Clusterung",
       cbind("Jupitermasse" = erstell_tabelle(Exo_hier_3$mass),
             "Umlaufzeit" = erstell_tabelle(Exo_hier_3$period),
             "Umlaufform" = erstell_tabelle(Exo_hier_3$eccen)),
       digits = 3)

xtable(caption = "Univariate Kenngrößen \textit{Jupitermasse}, 
       \textit{Umlaufzeit}, \textit{Umlaufform} des Cluster Nr 1. aus hierarchischer Clusterung",
       cbind("Jupitermasse" = erstell_tabelle(Exo_hier_4$mass),
             "Umlaufzeit" = erstell_tabelle(Exo_hier_4$period),
             "Umlaufform" = erstell_tabelle(Exo_hier_4$eccen)),
       digits = 3)

#Abbildung 1:
#Veranschaulichung
datad <- data.frame(x = c(-0.55, 0.1, -1.48, -1.3, 1.15, -0.85, 1.2, 0.65, 0), 
                   y= c(-1, -0.8, -0.4, -1.5, -0.45, -1.2, -0.2, -0.38, 0.8))
dis <- dist(datad, method = "euclidean")

final_clust <- agnes(dis, method = "ward")
#Dendogram
par(mfrow=c(1,2))
par(mar = c(5, 4, 6, 0) + 0.2)
plot(final_clust, xlab="", which.plots = 2, main="")

datas <- data.frame(n = c(1:9),
                   x = c(-0.55, 0.1, -1.48, -1.3, 1.15, -0.85, 1.2, 0.65, 0), 
                   y= c(-1, -0.8, -0.4, -1.5, -0.45, -1.2, -0.2, -0.38, 0.8))
par(mar = c(4, 4, 10, 0) + 0.2)
plot(datas$x, datas$y, type="n", ylab = "y", xlab = "x")
text(x = datas$x,
     y = datas$y,
     labels = datas$n)

#Abbildung 2:
#Banner
par(mfrow=c(1,1))
plot(final_clust, xlab="Länge", ylab="Objekt i", which.plots = 1, main="", )

#Abbildung 3:
fviz_nbclust(datad, hcut, method = "wss", k.max = 8)

#Abbildung 4:
#Dendogram
par(mfrow=c(1,1))
par(mar = c(5, 4, 6, 0) + 0.2)
plot(final_clust, xlab="", which.plots = 2, main="")
rect.hclust(as.hclust(final_clust), k=2, border = 6)

