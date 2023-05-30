                   # SESION 5 
---------------------------------------------------
  #<<===>>> <<====>>>  <<<===>>> <<<<====>>> <<<<<====>>>>
  <<====>> #<<===>>   <<==============>>>>  <<========>>> <<===========>>> <<=======>>>
 <<========================>>>
                            #Distrubucion Normal
 #<<===>>  <<=====>>  <<============>> <<=======>> <<=====>> <<=====>> <<===>>
                      <<=======>>
 numvar <- 100; media <-0; varianza <-1; a<-5
#Generar datos aleatorios de una distribucion normal
                      datos <-rnorm(numvar, mean = media, sd =varianza)
                      #Calcular la densidad de la distribución normal en un rango de valores 
                      rango <- seq(-a, a, length.out=100)
                      densidad <-dnorm(rango, mean= media, sd= varianza)
                      #Dibujar la grafica de la distribucion normal 
                      plot(rango, densidad, type= "l",lwd=2,
                           xlab="Valores",ylab ="Densidad",
                           main= "Distribución normal estándar")
                      #Agregar los datos aleatorios a la grafica
                      hist(datos, breaks =30, add=TRUE, col ="yellow",
                           border='green', freq=FALSE)
                      # <<====>> <<========>> <<====>> <<=====>>> <<<====>>> <<<=====>>> <<=====>>>
                      #<<=======>
                        #Distribucion Exponencial 
                        #<<=====>> <<======>>> <<===>>> <<==========>>> <<<========>>> <<<=====>>>
                        # <<====>> <<========>> <<====>> <<=====>>> <<<====>>> <<<=====>>> <<=====>>>
                       # <<=======>
                        lambda <- 0.6; ValMax <-10;
                      rango <- seq (0, ValMax, length.out =100)
                      datos<- rexp (numvar, lambda);
                      densidad<- dexp(rango,rate= lambda);
                      #Dibujar la gráfica  de la distribución  exponencial 
                      plot(rango, densidad,type ="l",lwd=2,
                           xlab="Valores",ylab = "Densidad",
                           main="Distribución exponencial")
                      #Agregar los datos aleatorios a la grafica
                      hist(datos, breaks = 30, col="yellow",border="green",
                           add=TRUE, freq = FALSE)
                      # <<====>> <<========>> <<====>> <<=====>>> <<<====>>> <<<=====>>> <<=====>>>
                      #<<=======>
                        #Distribucion Gamma 
                        #<<=====>> <<======>>> <<===>>> <<==========>>> <<<========>>> <<<=====>>>
                        # <<====>> <<========>> <<====>> <<=====>>> <<<====>>> <<<=====>>> <<=====>>>
                        #<<=======>
                        a<- 2; b<-0.5
                      #Genera datos aleatorios de una distribución  gamma
                      datos<-rgamma(numvar,shape = a, rate = b)
                      #Calcular la densidad de la distribución  gamma en un rango  de valores
                      rango<- seq(0,ValMax, length.out=100)
                      densidad <- dgamma(rango,shape = a, rate = b)
                      #Dibujar la  grafica de la distribucion gamma
                      plot(rango, densidad, type="l",lwd=2,
                           xlab = "Valores",ylab = "Densidad",
                           main = "Distribución gamma")
                      #Agrerar los datos aleatorios a la gráfica 
                      hist(datos,breaks = 30,col = "yellow", border = "green",
                           add=TRUE,freq=FALSE)
                      # <<====>> <<========>> <<====>> <<=====>>> <<<====>>> <<<=====>>> <<=====>>>
                      #<=======>
                        #Distribucion Beta
                        #<<=====>> <<======>>> <<===>>> <<==========>>> <<<========>>> <<<=====>>>
                        # <<====>> <<========>> <<====>> <<=====>>> <<<====>>> <<<=====>>> <<=====>>>
                        <<=======>>
                        c<- 5
                      datos <-rbeta(numvar,shape1 = a, shape2 = c)
                      #Calcular la densidad de la distribución  beta en un rango de valores
                      rango<-seq(0,1,length.out=100)
                      densidad <-dbeta(rango,shape1 = a,shape2 = c)
                      #Dibujar la gráfica  de la distribucion beta
                      plot(rango,densidad, type="l",lwd=2,
                           xlab = "Valores",ylab = "Densidad",
                           main = "Distribución beta")
                      #Agrerar los datos aleatorios a la gráfica 
                      hist(datos,breaks = 30,col = "yellow", border = "green",
                           add=TRUE,freq=FALSE)
# <<====>> <<========>> <<====>> <<=====>>> <<<====>>> <<<=====>>> <<=====>>>
# <=======>
                        #Distribucion Cauchy
                        #<<=====>> <<======>>> <<===>>> <<==========>>> <<<========>>> <<<=====>>>
                        # <<====>> <<========>> <<====>> <<=====>>> <<<====>>> <<<=====>>> <<=====>>>
                        <<=======>>
                    numvar<-150; a=0; b=1; ValMax<-10
                    #Generar datos aleatorios de una distribución  de cauchy
                    datos <-rcauchy(numvar, location = a, scale=b)
                    #Calcular la densidad de la distribucion de Cauchy en un rango de valoresv
                    rango<-seq(-ValMax,ValMax,length.out=100)
                    densidad<-dcauchy(rango,location = a,scale = b)
                    #Dibujar la grafica de la distribución  de Cauchy
                    plot(rango,densidad, type="l",lwd=2,
                         xlab = "Valores",ylab = "Densidad",
                         main = "Distribución cauchy")
                    #Agrerar los datos aleatorios a la gráfica 
                    hist(datos,breaks = 30,col = "yellow", border = "green",
                         add=TRUE,freq=FALSE)
                    # <<====>> <<========>> <<====>> <<=====>>> <<<====>>> <<<=====>>> <<=====>>>
                    #<=======>
                      #Distribucion T-Student
                      #<<=====>> <<======>>> <<===>>> <<==========>>> <<<========>>> <<<=====>>>
                      # <<====>> <<========>> <<====>> <<=====>>> <<<====>>> <<<=====>>> <<=====>>>
                      #<<=======>>
                    #Genera datos aleatorios de una distribucion t de student
                    gl <-5; datos  <- rt(numvar, df=gl); ValMax <-5
                    #Clacular la densidad de la distribucion t de student en un rango de valores
                    rango<- seq(-ValMax,ValMax, length.out=100)
                    densidad <-dt(rango, df =gl)
                    #Dibujar la gráfica  dde la distribucion t de student
                    plot(rango,densidad, type="l",lwd=2,
                         xlab = "Valores",ylab = "Densidad",
                         main = "Distribución t de Student")
                    #Agrerar los datos aleatorios a la gráfica 
                    hist(datos,breaks = 30,col = "yellow", border = "green",
                         add=TRUE,freq=FALSE)
                    
                    # <<====>> <<========>> <<====>> <<=====>>> <<<====>>> <<<=====>>> <<=====>>>
                    #<<=======>
                      #Distribucion Chi-Cuadrada
                      #<<=====>> <<======>>> <<===>>> <<==========>>> <<<========>>> <<<=====>>>
                      # <<====>> <<========>> <<====>> <<=====>>> <<<====>>> <<<=====>>> <<=====>>>
                      #<<=======>>
                    gl <-5; numvar <- 150; ValMax <-20
                    #Genera datos aleatorios de una distribucion chi-cuadrada
                    datos<-rchisq(numvar,df =gl)
                    #Calcular la densidad de la distribucion chi-cuadrada en un rango de valores
                    rango <- dchisq(rango,df=gl)
                    #Dibujar la grafica  de la distribucion chi-cuadrada
                    plot(rango,densidad, type="l",lwd=2,
                         xlab = "Valores",ylab = "Densidad",
                         main = "Distribución chi-cuadrada")
                    #Agrerar los datos aleatorios a la gráfica 
                    hist(datos,breaks = 30,col = "yellow", border = "green",
                         add=TRUE,freq=FALSE)
                    #Generar la funcion de densidad de probabilidad 
     pdf_unif <-funcio(x){if else (x >= 0 & x <=1, 1,0)}
                    #Genera datos de la variable aleatoria uniforme continua
                    set.seed(123) #establecer una semilla para la reproducibilidad
                    datos<- runif(1000, min =0,max = 1 )
                    #Graficar la funcion de densidad de probabilidad  y de histograma
                    par(mfrow =c(1,2))  #mostrar dos graficos en una fila 
                    curve(pdf_unif, from = -0,5, to=1.5, n= 1000, col ="blue",
                          main ="Variable Aleatoria Uniforme Continua",
                          xlab = "Valor de la variable aleatoria", ylab = "Densidad de probabilidad")
                    hist(datos,breaks = 20, col = "lightblue", freq = FALSE, add=TRUE)
                    # <<====>> <<========>> <<====>> <<=====>>> <<<====>>> <<<=====>>> <<=====>>>
                    par(mfrow= c(1,2))#volver a la configuracion de graficos predeterminada
                    <<=======>
                      #Todas en una sola ventada
                      #<<=====>> <<======>>> <<===>>> <<==========>>> <<<========>>> <<<=====>>>
                      par(mfrow =c(3,3))
                    # <<====>> <<========>> <<====>> <<=====>>> <<<====>>> <<<=====>>> <<=====>>>
                    <<=======>>)
                    #Distribucion Exponelcial 
                    # <<===>> <<<======>>> <<<<=========>>>> <<<========>>>> <<<========>>>>>
                    <<<========>>>>
                      lambda<- 0.6; ValMax <- 10;
                      rango <- seq(0, ValMax,length.out =100)
                      datos <- rexp(numvar, lambda);
                      densidad <- dexp(rango, rate = lambda);
                      #Dibujar la grafica de la distribucion exponencial 
                      plot(rango, densidad, type = "l",lwd=2,
                           xlab = "Valores", ylab = "Densidad ",
                           main= "distribucion exponelcial")
                      #Agregar los datos aleatorios a la grafica
                      hist(datos, breaks = 3,col="yellow", border = "green"
                           add= TRUE, freq= FALSE)|1QWD
                      
                      
                      
                      
                      #Según la  distribucion normal
                      #Graficar la distribución binomial y la distribucion teórica 
                      barplot(prob, names.arg= x, col= "lightblue",
                              main="Distribucion Binomial vs. Distribucion Normal",
                              xlab= "Numero de exitos", ylab ="Probabilidad")
                      lines(x_teoria, prob_teoria,col="red",lwd=2)
                      #calcular la distribucion geometrica y la distribucion teórica
                      p<- 0.3; x<- 0:10 #valores posibles de ensayos antes del primer exito
                      prob<- dgeom(x,prob= p)#probabilidad de cada valor de x para la distribución 
                      x_teoria <- seq(0,10, length.out =100)#valores de x para la distribucion teorica
                      prob_teoria <- dgeom(x_teoria,prob = p)#probabilidad de cada valor de x_teoria según 
                      #la distribución 
                      #Graficar la distribución  geometrica y la distribucion teorica
                      barplot(prob,names.arg=x, col="lightblue",
                              main= "Distribucion Geometriuca vs.Distribución Teórica ",
                              xlab="Número de ensayos antes del primer éxito", ylab ="Probabilidad")
                      points(x_teoria,proba, col= "red", pch=19)
                      #Generar una muestra aleatoria de una variable geométrica 
                      set.seed(123)#fijar la semilla para reproducibilidad
                      p<- 0.3; n<- 1000; muestra <- rgeom<- rgeom(n, prob = p)
                      #Calcular la distribucion teórica  geométrica 
                      x_teoria <- 0:10 # valores posibles de la variable aleatoria
                      prob_teoria <- dgeom(x_teoria,prob = p)#probabilidad de cada valor de x_teoria segun
                      #ladistribucion geometroca
                      #graficar el histograma y la distribución  teórica 
                      hist(muestra, prob=TRUE, col= "lightblue",
                           main="Variable Aleatorio Geometrica vs. Distribucion Teórica ",
                           xlab="Valor de la variable aleatoria",ylab="Densidad")
                      lines(x_teoria,prob_teoria, col="red")
                      #Definir los valores posibles de la variable aleatoria y sus probabilidades
                      x<- 1:5 #valores posibles  de la variable aleatoria y sus probabilidades
                      prob<- re(1/5,5)# probabilidad igual para cada valor 
                      #Graficar el diagrama de barras
                      barplot(prob,names.arg = x, col="lightblue",
                              main= "Varaible Aleatoria Uniforme Discreta",
                              xlab="Valor de la variable aleatoria",ylab="Probabilidad")
                      
                      
                      
                      #<<=========>> <<=======>> <<===================>> <<=======>> <<=======>>
                      #<<=====>>
                      #Todas en una sola ventana 
                      par(mfow = c(2,3))
                      #Calcular la distribución binomial
                      n<- 10; p<- 0.5;  x<- 0:n #valores posibles de exitos
                      prob <- dbinom(x,size=n, prob=p) #´probabilidad de cada valor de x
                      #Graficar la distribucion binomial
                      barplot(prob,names.arg=x, col="lightblue",
                              main ="Distribucion Binomial",xlab ="Numero de éxitos ",
                              ylab="Probabilidad")
                      #Calcular la distribución  binomial y la distribución  teórica 
                      n<- 20; p<- 0.3; x<- 0:n #valores posibles de exitos 
                      prob <- dbinom(x,size=n,prob = p)#probabilidad de cada valor x
                      mu <-n * p #media de la distribución  binomial
                      sigma<- sqrt(n * p (1 - p))# desviacion estándar  de la distribución binomial
                      x_teorica <- dnorm(x-teorica, mean= 100)#valores de x para la distribución 
                      teorica
                      prob_teoria <- dnorm(x_teoria, mean = mu,sd = sigma)#probabilidad de cada valor 
                      de x_teoría 
                      #segun la distribucion normal
                      #Graficar la distribución  binomial y la distribución teorica
                      barplot(prob, names.arg= x, col="lightblue",
                              main ="Distribucion Binomial vs.Distribucion Normal",
                              xlab ="Numero de exitos", ylab="Probabilidad")
                      lines(x_teoria, prob_teoria,col="red",lwd=2)
                      #Calcular  la distribucion geometrica y la distribucion teorica
                      p<- 0.3; x<- 0:10 #valores posibles de ensayos antes del primer exito
                      prob<- dgoem(x,prob=p)#probabilidades de cada valor de x
                      x_teoria <- seq(0,10, length.out =100)#valores de x para la distribución 
                      teorica
                      prob_teoria<- dgeom(x_teoria,prob = p)#probabilidad de cada valor de x_teoria
                      segun la 
                      #distribución  geometrica
                      #Graficar la distribución  geometrica y la distribución teórica 
                      barplot(prob, names.arg= x, col="lightblue",
                              main="Distribucion Geometrica vs. Distribucion Teórica",
                              xlab= "Numero de ensayos antes del primer éxito", ylab="Probabilidad")
                      points(x_teoria,prob_teoria,col="red",pch= 19)
                      #Generar una muestra aleatoria  de una variable geométrica 
                      set.seed(123)#fijar la semilla para reproducibilidad
                      p<- 0.3; n<-1000; muestra <- rgeom(n, prob = p)
                      #Calcular  la distribución teórica 
                      x_teoria <- 0:10 #valores posibles  de la variable aleatoria
                      prob_teoria<- dgeom(x_teoria,prob = p)#probabilidad de  de cada valor de x_teoría 
                      segun la
                      #distribución  geometrica    
                      #Graficar el histograma  y la distribución  teorica
                      hist(muestra, prob=TRUE, col="lightblue",
                           main ="Variables Aleatoria Geometrca vs. Distribucion Teorica",
                           xlab="Valor de la variable aleatoria", ylab= "Densidad")
                      lines(x_teoria,prob ,col= "red")
                      #Calcular la distribución  teorica Poisson
                      lambda <- 2 #parametro lambda de la variable aleatoria 
                      x_teoria <- 0:10 #valores posibles de la variable aleatoria 
                      prob_teoria <- dpois(x_teoria, lambda)# probabilidad de cada valor de x_teoria
                      segun la 
                      #Distribución  Poisson 
                      #Graficar el diagrama de barra y la distribucion teorica
                      barplot(prob_teoria,names.arg=x_teoria, col="lightblue",
                              main="Distribución Poisson vs. Distrinucion teorica",
                              xlab="Valor de la variable aleatoria", ylab ="Probabilidad")
                      lines(x_teoria, prob_teoria,col="red")
                      #Definir los valores posibles de la variable aleatoria y sus probabilidades
                      prob <- rep(1/5,5)#probabilidad igual para cada valor 
                      #Graficar el diagrama de barras
                      barplot(prob, names.arg=x, col="lightblue",
                              main="Variable Aleatoria Uniforme Discreta",
                              xlab="Valor de la variable aleatoria", ylab="Probabilidad")
                      # <<=====>> <<=====================>>  <<=================>> <<========>>
                      #<<=====>>
                      
                      
                      


------------------------------------------------------------------------------------------------------------
                        SESION 6 





setwd("C:/Users/Alumno/Desktop/Muestreo/ClaseProba")
#----------------------------------------------
NumVar <- 150
NumMuestras <- 30
Datos <- matrix(0,NumVar,NumMuestras); View(Datos)
#----------------------------------------------
set.seed(550)
#----------------------------------------------
for(i in 1:NumMuestras){
  Datos[,i] <- round(runif(NumVar,30,50))
}
View(Datos)
Ind <- abs(round(rnorm(150,24,10))); View(Ind)
MiBDD <- cbind(Ind,Datos); View(MiBDD)
#----------------------------------------------
head(MiBDD,5)
#----------------------------------------------
X <- MiBDD[,1]; head(X,15)
Y <- MiBDD[,2]; head(Y,15)
#----------------------------------------------
contando <- table(X); print(contando)
#----------------------------------------------
barplot(contando)

barplot(contando,
        col=rainbow(15,0.55))

barplot(contando,
        col=rainbow(15,0.55),
        ylim = c(0,20))


barplot(contando,
        col=rainbow(15,0.55),
        ylim = c(0,17))

barplot(contando,
        col=rainbow(15,0.55),
        ylim = c(0,15),
        xlab = 'Indices',
        ylab = 'Frecuencias',
        main = 'Mi Primer grafica')
grid(1.5,10)

barplot(contando,
        col  = 'pink',#rainbow(15,0.55),
        ylim = c(0,15),
        xlim = c(-1,55),
        xlab = 'Indices',
        ylab = 'Frecuencias',
        border="red",
        main = 'Mi Primer grafica',
        density = 15)
grid(1.5,10)
#----------------------------------------------






#----------------------------------------------
