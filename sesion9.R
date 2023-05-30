#1. X dist. normal con media = 2 y DesvEst = 1/3
#a) P[X>3]
#b) P[2<X<3]
#c) Encontrar x0 tal que P[X>x0]= 0.45
#d) Encontrar x* tal que P[X<x*]= alfa
#alfa = 0.5;   alfa = 0.75;  alfa = 0.8;
#alfa = 0.85;  alfa = 0.9;   alfa = 0.95
#alfa = 0.99
#e) P[X<0]
#f) P[-1<X<1]
# - - - - - - -  - - - - - - -  - - - - - - - 
#SOLUCIONES:
# - - - - - - -  - - - - - - -  - - - - - - - 
#a) P[X>3]
# - - - - - - -  - - - - - - -  - - - - - - - 
x0 <- 3
mu <- 2;   desvest <- 1/3;
z0 <- (x0-mu)/desvest; print(z0)
probabilidad <- pnorm(z0,0,1);  print(probabilidad)
probbuscada  <- 1-probabilidad; print(probbuscada)
# - - - - - - -  - - - - - - -  - - - - - - - 
#b) P[2<X<3]
# - - - - - - -  - - - - - - -  - - - - - - - 
mu <- 2;   desvest <- 1/3;
x1 <- 2;   
z1 <- (x1-mu)/desvest; print(z1)
p1 <- pnorm(z1,0,1);   print(p1)
x2 <- 3;
z2 <- (x2-mu)/desvest; print(z2)
p2 <- pnorm(z2,0,1);   print(p2)
probbuscada <- p2-p1;  print(probbuscada)
# - - - - - - -  - - - - - - -  - - - - - - - 
#e) P[X<0]
# - - - - - - -  - - - - - - -  - - - - - - - 
x3 <- 0
mu <- 2;   desvest <- 1/3;
z3 <- (x3-mu)/desvest; print(z3)
probabilidad <- pnorm(z3,0,1);  print(probabilidad)
# - - - - - - -  - - - - - - -  - - - - - - - 
#f) P[-1<X<1]
# - - - - - - -  - - - - - - -  - - - - - - - 
mu <- 2;   desvest <- 1/3;
x4 <- -1;   
z4 <- (x4-mu)/desvest; print(z4)
p4 <- pnorm(z4,0,1);   print(p4)
x5 <- 1;
z5 <- (x5-mu)/desvest; print(z5)
p5 <- pnorm(z5,0,1);   print(p5)
probbuscada <- p5-p4;  print(probbuscada)
# - - - - - - -  - - - - - - -  - - - - - - - 
#c) Encontrar x0 tal que P[X>x0]= 0.45
# P[X>x*]= p*, entonces buscamos en tablas el valor x*, tal que 
# P[X<x*] = q*= 1-p*, esto se hace con la funcion qnorm(q*,0,1)
pestrella <- 0.45; qestrella = 1-pestrella; print(qestrella)
zestrella <- qnorm(qestrella,0,1); print(zestrella)
#corroboramos
pnorm(zestrella,0,1) # sí es la respuesta correcta!!!!
mu <- 2;   desvest <- 1/3;
xestrella <- mu+desvest*zestrella; print(xestrella)
# corroboramos
pnorm(xestrella,2,1/3)
# - - - - - - -  - - - - - - -  - - - - - - - 
#d) Encontrar x* tal que P[X<x*]= alfa
#i)   alfa = 0.5;   ii) alfa = 0.75;  iii) alfa = 0.8;
#iv)  alfa = 0.85;   v) alfa = 0.9;    vi) alfa = 0.95
#vii) alfa = 0.99

alfa1 <- 0.5;
t1 <- qnorm(alfa1,0,1); print(t1)
mu <- 2;   desvest <- 1/3;
x1 <- mu+desvest*z1; print(x1)



Media = 100 y DesvEst = 16
P[X<80]
P[X>140]
P[90<X<110]
Encontrar x1 y x2, tales que P[x1<X<x2]=alfa

alfa = 0.5
alfa = 0.75
alfa = 0.8
alfa = 0.85
alfa = 0.9
alfa = 0.95
alfa = 0.99