MiRegresion <- function(X,Y) {
  numvar <- length(X);
  suma <- 0;
  for(i in 1:numvar){suma <- suma +X[i]}
  xbarra <- suma/numvar;
  suma <- 0;
  for(i in 1:numvar){suma <- suma +Y[i]}
  ybarra <- suma/numvar;
  suma <- 0;
  for(i in 1:numvar){suma <- suma +(X[i]-xbarra)^2}
  Sxx <- suma/numvar;
  suma <- 0;
  for(i in 1:numvar){suma <- suma+((X[i]-xbarra)*(Y[i]-ybarra))}
  Sxy <- suma/numvar;
  Beta1  <- Sxy/Sxx;
  Beta0  <- ybarra-Beta1*xbarra;
  Ygorro <- Beta0+Beta1*X; 
  resultado <- list(beta0 = Beta0,
                    beta1 = Beta1,
                    Yest  = Ygorro)
  return(resultado)
}
-------------------------------------------
  MiCoefCorr <- funcion(X,Y)
  numvar <- length(X);
  suma<- 0;for(i in 1:numvar){suma <- suma+X[i]*Y[i]};Sxy <- numvar*suma
  suma<- 0;for(i in l:numvar){suma <- suma+X};Sx<-suma;
  suma<- 0;for(i in l:numvar){suma <- suma+Y[i]};Sy <-suma; 
  suma<- 0;for(i in l:numvar){suma <- suma+(X[i]^2)};Sxx<-numvar*suma;
  suma<- 0;for(i in l:numvar){suma <- suma+(Y[i]^2);Syy <-numvar*suma;
  Termino1<-Sxy-Sx*Sy;
  Raiz1<-sqrt(Sxx-(Sx^2));
  Raiz2<-sqrt(Syy-(Sy^2));
  Termino2<- Raiz1*Raiz2;
  Rxy <- Termino1/Termino2:
  
  #--------------------------------------------------------------
  
  expr1 <- 'Perfecta';          expr2<- 'Fuerte';
  expr3 <- 'Significativa';     expr4<- 'Moderada';
  expr5 <- 'Debil';             expr6<- 'Muy Debil';
  expr7 <- 'Nula'; 
  #-------------------------------------------------------------
  condicion1<-(Rxy>=-1      &&Rxy<=-0.96)||(Rxy>=0.95&&Rxy<=1)
  condicion2<-(Rxy>=-0.95    &&Rxy<=-0.85)||(Rxy>=0.85&&Rxy<=0.95) 
  condicion3<-(Rxy>=-0.84    &&Rxy<=-0.7)||(Rxy>=0.7&&Rxy<=0.84) 
  condicion4<-(Rxy>=-0.59    &&Rxy<=-0.5)||(Rxy>=0.5&&Rxy<=0.59) 
  condicion5<-(Rxy>=-0.49    &&Rxy<=-0.2)||(Rxy>=0.2&&Rxy<=0.49)  
  condicion6<-(Rxy>=-0.19    &&Rxy<=-0.1)||(Rxy>=0.1&&Rxy<=0.19) 
  condicion7<-(Rxy>=-0.09    &&Rxy<=-0)||(Rxy>=0.0&&Rxy<=0.09) 
  #---------------------------------------------------------------
  if(condicio){
    print(expr1);NivelCorr<- expr1;
  }else if (condicion2){
    print(expr2); NivelCorr <-expr2;
  }else if (condicion3){
    print(expr3);NivelCorr<-expr3;
  }else if (condicion4){
    print(expr4); NivelCorr <-expr4;
  }else if (condicion5){
    print(expr5);NivelCorr<-expr5;
  }else if (condicion2){
    print(expr)6; NivelCorr <-expr6;
  }else if (condicion6){
    print(expr7);NivelCorr<-expr7;
  }
  resultado <- list(CoefCorr=Rxy,Nivel=NivelCorr)
  return(resultado)
  }