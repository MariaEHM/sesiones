#................................................
NumVar <- 150
NumMuestras <- 30
Datos <- matrix(0,NumVar,NumMuestras); view(Datos)
seed(666)
#...................................................
set.seed(550)
#...................................................
for(i in 1:NumMuestras){
  Datos[,i] <- round(runif(NumVar,30,50))
}
view(Datos)
Ind <- abs(round(rnorm(150,24,15))); View(Ind)
MiBDD <- cbind(Ind,Datos); View(MiBDD)
#..................................................
head(MiBDD,5)
#..................................................
X <- MiBDD[,1]; head(X,15)
Y <- MiBDD[,2]; head(Y,15)
#.....................................................
contando <- table(X); print(contando)
barplot(contando)

barplot(contando, 
        col=rainbow(15,0,55))

barplot(contando, 
        col=rainbow(15,0,55),
        ylim = c(0,20))

barplot(contando,
        col=rainbow(15,0,55)
        ylim = c(0,17))

barplot(contando,
        col=rainbow(15,0,55),
        ylim = c(0,15),
        xlab =?Indices?,
        ylab =?Frecuencias?,
        main = ?Mi Primer grafica?)
grid(1,5,10)

barplot(contando,
        col= ?pink?, #rainbow(15,0,55),
        ylim = c(0,15),
        xlim = c(-1,55),
        xlab =?Indices?,
        ylab =?Frecuencias?,
        border = "red",
        main = ?Mi Primer grafica?,
        density = 15)
grid(1,5,10)
#......................................
nbreaks = 15
hist (Y)
hist (Y,breaks=nbreaks)
hist (Y,breaks=nbreaks,
      col =rainbow(15,0.35))
hist (Y,breaks=nbreaks,
      col =rainbow(15,0.35),
      main = ?Mediciones de Tiempo de traslado?,
      xlab =?Frecuencias?,
      ylab =?Tiempos?,
      ylim = c(0,15))

hist(Y,breaks=nbreaks,
     col =rainbow(15,0.35),
     main = ?Mediciones de Tiempo de traslado?,
     xlab =?Frecuencias?,
     ylab =?Tiempos?,
     ylim = c(0,15),
     col = "darkmagenta",
     freq =TRUE)
Temperature <- Y


h <- hist(Termperature,breaks=nbreaks,
          col =rainbow(15,0.35),
          main = (Mediciones de Tiempo de traslado),
          xlab =?Frecuencias?,
          ylab =?Tiempos?,
          ylim = c(0,15),
          col = "darkmagenta",
          freq =TRUE))
summar(h)
text(h$mids,h$counts,
     labels=h$counts, adj=c(0.5, -0.5))


h <- hist(Termperature,breaks=nbreaks,
          col =rainbow(15,0.35),
          main = ?Mediciones de Tiempo de traslado?,
          xlab =?Frecuencias?,
          ylab =?Tiempos?,
          ylim = c(0,15),
          col = "darkmagenta",
          freq =TRUE))
summar(h)
text(h$mids,h$counts,
     labels=h$counts, adj=c(0.5, -0.5))

