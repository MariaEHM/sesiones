<><>============<><><><>=========================
================================================
#SE SIMULA UNA BASE DE DATOS CON 150 REALIZACION ES EN CADA MUESTRA, Y 30 MUESTRAS.
#<><><><>=================<><><>==========<><><>=====================<><><>====
==============<><><><>
Numvar <-150 #numero de datos a generar
NumMuestra <- 30 #NUMERO DE MUESTRAS A CONSIDERAR
Datos <-matrix(0,Numvar,NumMuestra); head(Datos)
# A CONTINUACION SE SIMULA LA BASE DE DATOS 
for(i  in l:NumMuestra){
  Datos[,1]<- round(runif(Numvar,30,50)):
     
  
}
View(Datos)
#<><><>========<><><><><>=================<><><><>===============
=============<><><>
#VAMOS A DETERMINARLAS MEDIDAS DE TENDENCIA CENTRAK Y DE DISPERSION 
PARA CADA UNA DE LAS MUESTRAS
#MEDIDA PARA LA PRIMERA MUESTRA
X1 <- Datos[,1];head(X1)
suma <- suma+X1[i]
for (i in 1:Numvar){
  suma<- suma+ X1[i]
}
PromedioX1 <- suma/Numvar
#ESTA INSTRUCIION CUENTA LAS VECES QUE SE REPITE CADA UNO DE LOS
VALORES
ordendo<- table(X1);print(contado)
#ESTA INSTRUCION CUENTA LAS VECES QUE SE REPITE CADA UNO DE LOS 
#VALORES
ordenado <- sort(contando);
n<-length(ordenado); #CALCULEMOS EL NUMERO DE DATOS QUE SE ENCUENYTRA UBICADOS
#EXACTAMENTE 
#EN LA MITAD,PARA ESO DETERMINAREMOS EL NUMERO DE DATOS Y DEPENDIENDO DE 
#SI ES PAR O IMPAR SE
#DETERMINARA LA MEDIANA
N<- length(X1)
#DETERMINEMOS SI LA CANTIDAD ES PAR O IMPAR
if(N%%2==0){
 print('ES UN NUMERON PAR DE DATOS')
  
  X10rd<- sort(X1)#ESTA INSTRUCCION ORDENA DE MENOR A MAYOR.
  primero<-X10rd[N/2]#OBTENEMOS EL DATO QUE ESTA AL FINAL DE LOS PRIMEROS n/2
  Datos
  segundo <-X10rd[N/2+1]#ONTENEMOS EL DATO QUE ESTA AL INICIO DE LOS SEGUNDOS
  n/2 DATOS
  medianaX1 <-(primero+segundo)/2
}else{
  X10rd <-sort(X1)#ESTA INSTRUCCION ORDENA DE MENOR A MAYOR,
  medianaX1<-X10rd[N/2+1]#OBTENEMOS EL DATO QUE ESTA A LA MITAD DE LOS DATOS
  
}
MTC<- MATRIX(0,1,3)
MTC[1,1]<-PromedioX1;
MTC[1,2]<-medianax1
MTC[1,3]<- modaX1
colnames(MTC)<- c('Media','Mediana','Moda');print(MTC)
#rownames(MTC)<-c('Muestal)

#CALCULEMOS AHORA LA PRINCIPAL MEDIA DE DISPERSION
suma<-0;
for( i in l:Numvar){
  termino1<-X1[i]-PromedioX1
  termino12 <- termino1^2
  suma <- suma +termino12
  
}

Varianza<- suma/(Numvar-1)
desEst<- sqrt(Varianza)
  
  
