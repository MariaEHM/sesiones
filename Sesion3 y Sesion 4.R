#<><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><>
# SE SIMULA UNA BASE DE DATOS CON 150 REALIZACIONES EN CADA MUESTRA, Y 30 MUESTRAS.
#<><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><>
NumVar     <- 150   # numero de datos a generar
NumMuestra <- 30    # NUMERO DE MUESTRAS A CONSIDERAR
Datos <- matrix(0,NumVar,NumMuestra); head(Datos)
# A CONTINUACION SE SIMULA LA BASE DE DATOS
for(i in 1:NumMuestra){
  Datos[,i] <- round(runif(NumVar, 30,50));
}
View(Datos)
#<><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><>
# VAMOS A DETERMINAR LAS MEDIDAS DE TENDENCIA CENTRAL Y DE DISPERSION PARA CADA UNA DE LAS MUESTRAS
# MEDIA PARA LA PRIMERA MUESTRA
X1 <- Datos[,1]; head(X1)
suma <- 0
for(i in 1:NumVar){
  suma <- suma + X1[i]
}
PromedioX1 <- suma/NumVar
# AHORA VAMOS A CALCULAR LA MODA
contando <- table(X1); print(contando)
# ESTA INSTRUCCION CUENTA LAS VECES QUE SE REPITE CADA UNO DE LOS VALORES
ordenado <- sort(contando);
n <- length(ordenado); # CALCULAMOS EL NUMERO DE VARIABLES QUE SE TIENEN
modaX1 <- ordenado[n]
# PARA DETERMINAR LA MEDIANA RECORDEMOS QUE ES EL VALOR QUE SE ENCUENTRA UBICADO EXACTAMENTE
# EN LA MITAD, PARA ESO DETERMINAREMOS EL NUMERO DE DATOS Y DEPENDIENDO DE SI ES PAR O IMPAR SE 
# DETERMINARA LA MEDIANA
N <- length(X1)
# DETERMINEMOS SI LA CANTIDAD ES PAR O IMPAR
if( N%%2==0){
  print('ES UN NUMERO PAR DE DATOS')
  X1Ord <- sort(X1) # ESTA INSTRUCCION ORDENA DE MENOR A MAYOR.
  primero <- X1Ord[N/2]   # OBTENEMOS EL DATO QUE ESTA AL FINAL DE LOS PRIMEROS n/2 DATOS
  segundo <- X1Ord[N/2+1] # OBTENEMOS EL DATO QUE ESTA AL INICIO DE LOS SEGUNDOS n/2 DATOS
  medianaX1 <- (primero+segundo)/2
}else{
  X1Ord <- sort(X1) # ESTA INSTRUCCION ORDENA DE MENOR A MAYOR.
  medianaX1 <- X1Ord[N/2+1] # OBTENEMOS EL DATO QUE ESTA A LA MITAD DE LOS DATOS
}

MTC <- matrix(0,1,3)
MTC[1,1] <- PromedioX1;
MTC[1,2] <- medianaX1
MTC[1,3] <- modaX1
colnames(MTC) <- c('Media','Mediana','Moda'); print(MTC)
#rownames(MTC) < c('Muestra1')

# CALCULEMOS AHORA LA PRINCIPAL MEDIDA DE DISPERSION

suma <- 0;
for(i in 1:NumVar){
  termino1 <- X1[i]-PromedioX1
  termino12 <- termino1^2
  suma <- suma + termino12
}
Varianza <- suma/(NumVar-1)
DesvEst <- sqrt(Varianza)
#------------------------------------------------------------
               # SESION 4
#<><><><>=======<><><><><>==========<><><><><><>===============================
#<><><><><><><>===============<><><><>
  #<><><><><><>============<><><><><><><><>===========<><><>==============
#<><><><><><><><><><><><>
  #MODELO DE REGRESION LINEAL
  #<><><><>==============<><><>=======<><><><><><><><>===================
#<><><><><>==============<><><>
  #<><><>=========<><><><><><>======================<><><><>
#  <><><><><>================<><><><><>
  #EL MODELO DE REGRESION LINEAL SIMPLE ES:YGORRO=BETA_{0}GORRO+BETA_{1}GORRO*X
  #DONDE BETA_{1}=S_{XY}/S_{XX}
  #BETA_{0}=Y_BARRA -BETA_{1}*X_BARRA
  #S_{XX}=[suma_{i=1}^{n}(x_{i}-X_BARRA)^2]/n
  #S_{XY}=[suma_{i=1}^{n}(x_{i}-X_BARRA)*(y_{i}-Y_BARRA)]/n
  #<><><><><><>==============<><><><>=========<><><><><><>========
#<><><><><><>======<><>
  #IMPLEMENTACION
  setwd("-/Documentos/CursoProba")
#SE SIMULA UNA BASE DE DATROS CON 150 REALIZACIONES EN CADA MUESTRA, Y 30S MUESTRA.
#
NumVar <- 150 #numero de datos a generar
NumMuestra<- 30 #NUMER0 DE MUESTRAS A CONSIDERAR
Datos <- matrix(0,NumVar,NumMuestra);head(Datos)
for (i in 1:NumMuestra){Datos[,i]<- round(runif(NumVar,30,50));}
View(Datos)
#-------------------------------------------------------------------
x-abs(round(rnorm(NumVar,0,50)));print(x1)
Datos<- cbind(x1,Datos);View(Datos)
#-------------------------------------------------------------------
#consideremos las primeras dos muestras
X<-Datos[,2]; Y <-Datos[,3]
#Calculemos la media de cada variable 
suma <- 0; for(i in 1:NumVar){suma<- suma+ X[i]};xbarra<- suma/NumVar;
print(xbarra)
suma<- 0; for( i in 1:NumVar){suma<- suma+Y[i]};ybarra<- suma/NumVar;
print(ybarra)
suma <-0; for( i in 2:NumVar){suma<- suma+(X[i]-xbarra)^2};Sxx<- suma/NumVar;
print(Sxx)
suma<-0;for(i in 1:NumVar){suma<-suma +(X[i]-xbarra)*(Y[i]-xbarra)};Sxy<-suma/NumVar;
print(Sxy)
Beta1<- Sxy/Sxx;   print(Beta1);
Beta0<- ybarra- Beta1*xbarra; print(Beta0)
#para hacer las prediciones, se utiliza la ecuacion Ygorro=Ygorro= Beta0+Beatal*X
Ygorro<- Beta0+Beta1*X;print(Ygorro)
Error <- Y -Ygorro; print(Error)
#<><><><>===========================<><><><><><><><><><>============
#<><><><><><><>==========<><><><>>
  #<><><><><><>==================<><><><><>
  #COEFICIENTE DE CORRELACIÓN 
  #<><><><><><><>=================<><><><><>==============
#<><><><><><><><><><>================<><><<
  #SE DEFINE COMO COV(XY)/RAIZ(VAR(X)*VAR(Y))
  #<><><><><>===================<><><><><><><>========================<><><>
 # <><><><><><><><>
  #METODO 1
  #
  #Numrador<- SUMA[(x_{i}-x_barra)(y_{i}- y_barra)]
  #Denominador <-raiz1*raiz2,donde
  #raiz1<- suma(x_[i]-x_barra)^2
  #raiz2 <- suma(y_{i}-y_barra)^2
  #IMPLEMENTACION REVISAR- TIENE ERROR
  #-------------------------------------------------------------------------
suma1<- 0;suma2 <-0;
for(i in 1:NumVar){suma1<-suma1+X[i];suma2+Y[i];
}
xbarra <-suma1/NumVar; ybarra <-suma2/NumVar;print(xbarra);print(ybarra)
suma<-0;for(i in 1:NumVar){suma<-suma+(X[i]-xbarra)^2}; nSxy<- suma;
print(nSxy)
suma <-0;for(i in 1:NumVar){suma<- suma+(X[i]-xbarra)^2};nSxx<- suma;rnSxx<-
  sqrt(nSxx);print(rnSxx)
suma <-0;for(i in 1:NumVar){suma<- suma+(X[i]-xbarra)^2};nSyy<- suma;rnSyy<-
  sqrt(nSyy);print(rnSyy)
rxy<- nSxx/(rnSxx*rnSyy);print(rxy)
#<><><><>=========<><><><><><>==============<><><><><><>=====================
#<><><><><><><>==================<><><><><><><><><>=====================
  #<><><><><><><><><><><><><><>=====================<><>=========<><><><>======
#<><><><>=====================<><><><>
  #METODO2
  #Numerador<-Termino1+Termino2
  #Denominador <- raiz1*raiz2,DONDE
  #Termino1 <- n*suma(x_{i}*y_{i})
  #Termino2<-suma(x_{i})*suma(y_{i})
  #raiz<-raiz(n*suma(x_{i}^2-(suma(x_{i})^2)))
  #raiz2<-raiz(n*suma(y_{i}^2-(suma(y_{i})^2)))
  #IMPLEMENTACIÓN 
  #<><><><><><>====================<><><><>=======<><>======<><><><><><>=======
#<><><><><>================<><><>
suma<-0;for(i in 1:NumVar){suma<-suma+X[i]*Y[i]};Ter1<- NumVar*suma;print(Ter1)
suma<-0;suma2<-0;for(i in 1:NumVar){suma1<-suma1+X[i];suma2<-suma2+Y[i]};Ter2<-suma1*suma2;
print(Ter2)
Numerador<-Ter1-Ter2;print(Numerador);
suma<-0;for(i in 1:NumVar){suma<-suma+(X[i]^2)};Ter<-NumVar*suma-
  (NumVar*xbarra)^2; raiz1<-sqrt(Ter1);
suma<-0;for(i in 1:NumVar){suma<-suma+(Y[i]^2)};Ter<-NumVar*suma-
  (NumVar*ybarra)^2; raiz2<-sqrt(Ter2);
Denominador <-raiz1*raiz2;print(Denominador);
Rxy<-Numerador/Denominador;print(Rxy)

#<><>===========================<><><><>===========================<><><><>
#<><><><>=============<><><><>
  #<><><><><>============<><><><><>===========
#
#Cov(X,Y)<-(1/2n^2)suma suma(x_{i}-x_barra)(y_{i}-y_barra)
#IMPLEMENTACION
#<><><><><>==============<><><><><><><>====================================
#<><><><><><><>===================<><><><>
  #REGRESION LINEAL
  
  
  
  
  
  
  
  
  
  