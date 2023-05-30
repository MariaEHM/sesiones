#### Subsección 1.1.1: Distribución Uniforme
##Resuelve los siguientes problemas relacionados con la variable aleatoria uniforme discreta.**
#  1. Un juego de dados justo se lanza tres veces. ¿Cuál es la probabilidad de obtener una suma de 10?

probabilidad_suma_10 <- function() {
  # Inicializar contador de resultados favorables
  resultados_favorables <- 0
  
  # Realizar todas las combinaciones posibles de 
  # lanzamientos de tres dados
  for (dado1 in 1:6) {
    for (dado2 in 1:6) {
      for (dado3 in 1:6) {
        # Verificar si la suma es igual a 10
        if ((dado1 + dado2 + dado3) == 10) {
          resultados_favorables <- resultados_favorables + 1
        }
      }
    }
  }
  
  # Calcular la probabilidad
  probabilidad <- resultados_favorables / (6^3)
  
}
print(probabilidad)



-----------------------------------------------------------------------------------------------------
  #  2. Una urna contiene 10 bolas numeradas del 1 al 10. Si se elige una bola al azar, ¿cuál es la
  #probabilidad de que sea un número par?
  numpares <-5
numtotal<-10
probabilidadpar <- (numpares/numtotal);print(probabilidadpar*100) # Respuesta 2

------------------------------------------------------------
  #  3. Un fabricante de piezas de repuesto sabe que el 5% de las piezas que produce son defectuosas. Si
  #se eligen aleatoriamente 5 piezas, ¿cuál es la probabilidad de que al menos una de ellas sea
  #defectuosa?
  defectuoso  <- 1 -(0.98)^5
print(defectuoso*100)# resultado 3

-------------------------------------------------------------------
  #  4. Un fabricante de cartas de póker quiere asegurarse de que todas las cartas tengan la misma
  #probabilidad de ser seleccionadas al azar. ¿Cuántas cartas debe incluir en su mazo?
  numtotal <- 52
print(numtotal)# respuesta 4
---------------------------------------------------------------------------------------------
  #  5. Un estudiante elige al azar una página de un libro y cuenta el número de palabras que contiene. Si
  #el libro tiene 200 páginas y un promedio de 1000 palabras por página, ¿cuál es la probabilidad de que
  #el número de palabras en una página elegida al azar esté entre 900 y 1100?
  # Definir los limites inferiores y superiores
  limite_inferior <- 900
limite_superior <- 1100
# Definir la media y la desviacion estandar
media <- 1000
desviacion_estandar <- 100
# Calcular los valores Z correspondientes
z_inferior <- (limite_inferior - media) / desviacion_estandar
z_superior <- (limite_superior - media) / desviacion_estandar
# Calcular la probabilidad utilizando la funcion pnorm
probabilidad <- pnorm(z_superior) - pnorm(z_inferior) 
probabilidad1<- probabilidad * 100 
print(probabilidad1)
---------------------------------------------------------------------
  #  6. Un vendedor de seguros tiene 10 pólizas que puede vender. Si se eligen al azar 3 pólizas, ¿cuál es la
  #probabilidad de que al menos una de ellas sea vendida?
  #casos favorables
  numcasos <- choose (10,3)
#casos total de posibilidades 
numtotal <<- choose(10,3)
#calcula la poliza de que ninguna esta vendida
probabilidadnoved <- numcasos / numtotal
#calcular la probabilidad  de que al menos una este vendida
probabilidadved <-1 - probabilidadnoved
print(probabilidadnoved)
-----------------------------------------------------------------------
  #  7. Un fabricante de dulces quiere que cada bolsa contenga una cantidad igual de caramelos de cada
  #sabor. Si hay 4 sabores de caramelos y cada bolsa contiene 20 caramelos, ¿cuál es la probabilidad de
  #que haya exactamente 5 caramelos de cada sabor en una bolsa seleccionada al azar?
  #casos favorables
  numcasos <- choose (10,5)
#casos total de posibilidades 
numtotal <- choose(20,5) * choose (15, 5) * choose(10,5) * choose(5,5)
#calcula la probabilidad 
probabilidad <- numcasos / numtotal
print(probabilidad)  

-------------------------------------------------------------------
  #  8. Una persona lanza una moneda 5 veces. ¿Cuál es la probabilidad de que obtenga exactamente 2
  #caras?
  #casos favorables
  numcasos <- choose (5,2)
#casos total de posibilidades 
numtotal <<- 2^5
#calcula la probabilidad 
probabilidad <- (numcasos / numtotal)*100
print(probabilidad)   
----------------------------------------------------------------------
  # 9. Un jugador de fútbol tiene una tasa de éxito del 80% en tiros libres. Si tira 5 tiros libres, ¿cuál es la
  #probabilidad de que haga exactamente 4 goles?
  #casos favorables
  numcasos <- choose (5,4)
# calcular la probabilidad de gol
probabilidadgol <- 0.8
#calcular de no hacer gol
probabilidadnogol <-1 -probabilidadgol
#calcular el numero total de casos
numtotal <- probabilidadgol^4 * probabilidadnogol
#calcular la probabilidad 
probabilidad <- numcasos * numtotal *100

print(probabilidad)
--------------------------------------------------------------------
  # 10. Se lanza un dado justo hasta obtener un número par. ¿Cuál es la probabilidad de que se necesiten
  #al menos 3 lanzamientos?
  probamax2 <- 1/4
#calcular  la posibilidad 
probamax3 <- probamax2 *100

print(probamax3)


----------------------------------------------------------------------------------------
  ### Subsección 1.1.2: Distribución Bernouli
  
  #Resuelve la siguiente lista de ejercicios**
  #  1. Un jugador de baloncesto tiene una tasa de acierto del 70% en tiros libres. ¿Cuál es la probabilidad
  #de que haga el primer tiro libre?
  #Probabilidad de éxito 
  probabilidadex <- 0.70
# Calcular la probabilidad de que haga el primer tiro libre
probabilidadpritiro <- probabilidadex *100
print(probabilidadpritiro)

-----------------------------------------------------#
  #  2. Un anuncio en línea tiene una tasa de clics del 2%. Si se muestra el anuncio a 1000 personas, ¿cuál
  #es la probabilidad de que exactamente 20 personas hagan clic en él?
  
  n <- 1000
# Definir el numero de exitos (personas que hacen clic en el anuncio)
k <- 20
# Definir la probabilidad de exito (probabilidad de hacer clic en el
# anuncio)
p <- 0.02
# Calcular la probabilidad de que exactamente 20 personas hagan 
probabilidad <- choose(n, k) * p^k * (1 - p)^(n - k)
print(probabilidad)
------------------------------------------------------------------
  #3. Un fabricante de bombillas sabe que el 3% de las bombillas que produce son defectuosas. Si se
  #elige una bombilla al azar, ¿cuál es la probabilidad de que sea defectuosa?
  
  probadef <- 0.3 * 100
print(probadef)

-----------------------------------------------------------
  #  4. Un jugador de fútbol americano tiene una tasa de éxito del 90% en tiros de campo. Si intenta un tiro
  #de campo, ¿cuál es la probabilidad de que tenga éxito?
  probadef <- 0.9 
#  un intento
probabilidad <-(1- 0.9 ) *100
print(probabilidad)   
----------------------------------------------
  #  5. La probabilidad de que un jugador de baloncesto haga un tiro libre es de 0.85. Si un jugador tira un
  #solo tiro libre, ¿cuál es la probabilidad de que lo haga?
  probadef <- 0.85 * 100
print(probadef)
------------------------------------------------------------------------
  #  6. La probabilidad de que una persona enferma de gripe tenga fiebre es de 0.9. Si se selecciona
  #aleatoriamente a una persona enferma de gripe, ¿cuál es la probabilidad de que tenga fiebre?
  probadef <- 0.9 * 100
print(probadef)
-----------------------------------------------
  # 7. La probabilidad de que un estudiante apruebe un examen es de 0.6. Si se selecciona
  #aleatoriamente a un estudiante, ¿cuál es la probabilidad de que apruebe su examen?
  probadef <- 0.6 * 100
print(probadef)
-----------------------------------------------
  # 8. La probabilidad de que una persona que juega a la ruleta gane en una ronda es de 0.2. Si una
  #persona juega una sola ronda, ¿cuál es la probabilidad de que gane?
  probadef <- 0.2
#  un intento
probabilidad <-(1- 0.2 ) *100
print(probabilidad)   

------------------------------------------------------------
  # 9. La probabilidad de que una persona contratada por una empresa sea un buen empleado es de 0.7.
  #Si una empresa contrata a una sola persona, ¿cuál es la probabilidad de que sea un buen empleado?
  probadef <- 0.7 
#  un intento
probabilidad <-(1- 0.7 ) *100
print(probabilidad)   
------------------------------------------------------
  #  10. La probabilidad de que una persona en una ciudad determinada tenga seguro médico es de 0.4. Si
  ##se selecciona aleatoriamente a una persona en la ciudad, ¿cuál es la probabilidad de que tenga
  #seguro médico?
  probadef <- 0.4
#  un intento
probabilidad <-(1- 0.4 ) *100
print(probabilidad)   
-----------------------------------------
  #  11. La probabilidad de que una persona llegue a tiempo a una cita es de 0.8. Si una persona tiene una
  #sola cita, ¿cuál es la probabilidad de que llegue a tiempo?
  probadef <- 0.8
#  un intento
probabilidad <-(1- 0.9 ) *100
print(probabilidad)   
-----------------------------------------------
  #  12. La probabilidad de que un estudiante se enferme durante un semestre es de 0.3. Si se selecciona
  #aleatoriamente a un estudiante, ¿cuál es la probabilidad de que se enferme durante el semestre?
  probadef <- 0.3 
#  un intento
probabilidad <-(1- 0.3 ) *100
print(probabilidad)   
--------------------------------------------------
  #   13. La probabilidad de que un equipo de fútbol gane un partido es de 0.6. Si un equipo juega un solo
  #partido, ¿cuál es la probabilidad de que gane?
  probadef <- 0.6 
#  un intento
probabilidad <-(1- 0.6 ) *100
print(probabilidad)    
--------------------------------------------------
  
  # 14. La probabilidad de que una persona responda correctamente una pregunta en un examen de
  #opción múltiple es de 0.25. Si una persona responde una sola pregunta, ¿cuál es la probabilidad de
  #que responda correctamente?
  probadef <- 0.25 
#  un intento
probabilidad <-(1- 0.25 ) *100
print(probabilidad)   
-------------------------------------------------------------------------------------------------------- 
  ### Subsección 1.1.3: Distribución Binomial
  
  #Resuelve los siguientes ejercicios**
  
  # 1. Una fábrica de chocolate sabe que el 10% de sus barras de chocolate tienen algún defecto. Si se
  #seleccionan al azar 5 barras, ¿cuál es la probabilidad de que al menos una tenga defectos?
  #n =5 , p =0.1,  x=1
  choose(5,1)*0.1^1*(1-0.1)^4*100 #respuesta 1

-----------------------------------------------------------
  #   2. Un agente de bienes raíces sabe que la probabilidad de que un comprador cierre un trato es del
  # 40%. Si el agente tiene 8 posibles compradores interesados, ¿cuál es la probabilidad de que
  #exactamente 3 cierren un trato?
  #n =8 , p =0.4,  x=3
  choose(8,3)*0.4^3*(1-0.4)^5*100 #respueta  1
-------------------------------------------------------------
  # 3. Un equipo de béisbol tiene una tasa de éxito del 70% en bateo. Si el equipo batea 25 veces, ¿cuál es
  #la probabilidad de que al menos 18 de los bateos sean exitosos?
  #n =25 , p =0.7  x=18
  choose(25,18)*0.7^18*(1-0.7)^7*100 #respueta 3
---------------------------------------------------------------------
  #  4. Una empresa de marketing sabe que la tasa de respuesta a un correo electrónico es del 20%. Si
  #envían 100 correos electrónicos, ¿cuál es la probabilidad de que menos de 10 personas respondan?
  #n =100 , p =0.2,  x=10
  choose(100,10)*0.2^10*(1-0.2)^90*100 #respueta  4
------------------------------------------------------------------------------
  #   5. Un investigador sabe que la probabilidad de que un ratón de laboratorio tenga una mutación
  ## específica es del 5%. Si el investigador utiliza 30 ratones, ¿cuál es la probabilidad de que al menos
  #uno tenga la mutación?
  #n =30 , p =0.05,  x=1
  choose(30,1)*0.5^1*(1-0.05)^29*100 #respueta 5
------------------------------------------------------------------
  #  6. Una compañía de seguros sabe que la probabilidad de que un cliente haga un reclamo es del 10%.
  #Si la compañía tiene 500 clientes, ¿cuál es la probabilidad de que al menos 50 hagan un reclamo?
  #n =500 , p =0.1,  x=50
  choose(500,50)*0.1^50*(1-0.1)^450*100 #respueta 6 
-----------------------------------------------------
  #   7. Un centro de llamadas sabe que la probabilidad de que un agente cierre una venta es del 25%. Si el
  #centro de llamadas tiene 20 agentes, ¿cuál es la probabilidad de que al menos 5 cierren ventas en un
  #día?
  #n =29 , p =0.25,  x=5
  choose(20,5)*0.25^5*(1-0.25)^15*100 #respueta 7
-----------------------------------------------------------
  #    8. Un fabricante de juguetes sabe que la probabilidad de que un juguete sea defectuoso es del 5%. Si
  #se producen 200 juguetes, ¿cuál es la probabilidad de que exactamente 15 sean defectuosos?
  #n =200 , p =0.005,  x=15
  choose(200,15)*0.05^15*(1-0.05)^185*100 #respueta 8
-----------------------------------------------------------------
  #    9. Un casino sabe que la probabilidad de que un jugador gane en la ruleta es del 47%. Si un jugador
  # juega 10 rondas, ¿cuál es la probabilidad de que gane al menos 6 rondas?
  #n =10 , p =0.47,  x=6
  choose(10,6)*0.47^6*(1-0.47)^4*100 #respueta 9
-----------------------------------------------------------------
  #  10. Una tienda de ropa sabe que la probabilidad de que un cliente compre un artículo es del 30%. Si 50
  #clientes entran a la tienda, ¿cuál es la probabilidad de que al menos 10 compren algún artículo?
  #n =50 , p =0.3,  x=10
  choose(50,10)*0.3^10*(1-0.3)^40*100 #respueta  10
---------------------------------------------------------------------------------------------------------------------  
  ### Subsección 1.1.1: Distribución Poisson
  
  
  # Resuelve los siguientes problemas**
  #1. Una tienda de abarrotes recibe un promedio de 5 clientes por hora. ¿Cuál es la probabilidad de que
  # lleguen exactamente 10 clientes en las próximas dos horas?
  # n = 5, x =10
  #n^x * exp(-x)/factorial(n)
  5^10*exp(-10)/factorial(5) # resultado 1
----------------------------------------------------------------------------
  #  2. Un centro de llamadas recibe en promedio 15 llamadas por hora. ¿Cuál es la probabilidad de que
  # reciban al menos 20 llamadas en una hora determinada?
  # n = 15, x =20
  #n^x * exp(-x)/factorial(n)
  15^20*exp(-20)/factorial(15) # resultado 2
------------------------------------------------------
  #   3. Un restaurante recibe un promedio de 3 quejas de clientes por día. ¿Cuál es la probabilidad de que
  #reciba exactamente 2 quejas en un día?
  # n = 3, x =2
  #n^x * exp(-x)/factorial(n)
  3^2*exp(-2)/factorial(3)# resultado 3
-------------------------------------------------------------
  #  4. Una empresa de envío de paquetes recibe un promedio de 4 paquetes por hora. ¿Cuál es la
  # probabilidad de que reciban al menos 6 paquetes en una hora determinada?
  # n = 4, x =6
  #n^x * exp(-x)/factorial(n)
  4^6*exp(-6)/factorial(4)# resultado 4
--------------------------------------------------------------------
  #   5. Un médico atiende un promedio de 2 pacientes por hora. ¿Cuál es la probabilidad de que atienda
  #exactamente 5 pacientes en un período de 3 horas?
  # n = 2, x =5
  #n^x * exp(-x)/factorial(n)
  2^5*exp(-5)/factorial(2)# resultado 5
---------------------------------------------------------------- 
  #   6. Un canal de noticias transmite un promedio de 3 noticias de última hora por día. ¿Cuál es la
  #probabilidad de que transmitan exactamente 2 noticias de última hora en un día determinado?
  # n = 3, x =2
  #n^x * exp(-x)/factorial(n)
  3^2*exp(-2)/factorial(3)# resultado 6
-----------------------------------------------------
  #  7. Una fábrica produce un promedio de 10 piezas defectuosas por día. ¿Cuál es la probabilidad de que
  #produzcan al menos 15 piezas defectuosas en un día determinado?
  # n = 10, x =15
  #n^x * exp(-x)/factorial(n)
  10^15*exp(-15)/factorial(10)# resultado 7
----------------------------------------------------------------
  # 8. Un equipo de fútbol anota en promedio 2 goles por partido. ¿Cuál es la probabilidad de que anoten
  #exactamente 3 goles en un partido determinado?
  # n = 2, x =3
  #n^x * exp(-x)/factorial(n)
  2^3*exp(-3)/factorial(2)# resultado 8
----------------------------------------------------------------
  # 9. Un supermercado vende en promedio 20 paquetes de arroz por día. ¿Cuál es la probabilidad de que
  #vendan menos de 15 paquetes de arroz en un día determinado?
  # n = 20, x =15
  #n^x * exp(-x)/factorial(n)
  20^15*exp(-15)/factorial(20)# resultado 9
------------------------------------------------------------
  #  10. Una empresa de taxis recibe en promedio 8 solicitudes de viaje por hora. ¿Cuál es la probabilidad
  #de que reciban al menos 12 solicitudes de viaje en una hora determinada?
  # n = 8, x =12
  #n^x * exp(-x)/factorial(n)
  8^12*exp(-12)/factorial(8)# resultado 10
-------------------------------------------------------------------------------------------------------------------------
  ### Subsección 1.1.1: Distribución Geométrica
  
  
  #Plantea y resuelve los siguientes ejercicios**
  # 1. La probabilidad de que un equipo de baloncesto anote un punto en un tiro libre es del 70%. La
  #distribución geométrica se puede usar para calcular la probabilidad de que un jugador necesite más
  #de un intento para anotar un tiro libre.
  provexito <- 0.70

intento1 <- provexito 
probavarios <- 1 - intento1 
print(probavarios) # resultado 1

--------------------------------------------------------
  #2. Un sitio web tiene una tasa de conversión del 5%, lo que significa que el 5% de los visitantes del
  #sitio web se convierten en clientes. La distribución geométrica se puede usar para calcular la
  #probabilidad de que se necesiten más de 10 visitas al sitio web para obtener un cliente.
  p<- 0.05
# PROBABILIDAD ACUMULADA DE HASTA 10
proacum <- pgeom(10,prob = p)
# PROBABILIDAD DE NECESITAR  MAS DE 1O
promas <- 1-  proacum
print(promas)
--------------------------------------------------------------------
  #  3. Un restaurante tiene una probabilidad del 20% de que un cliente ordene un postre. La distribución
  #geométrica se puede usar para calcular la probabilidad de que se necesiten más de 5 clientes antes
  #de que alguien ordene un postre.
  p<- 0.2
# PROBABILIDAD ACUMULADA DE HASTA 5
proacum <- pgeom(5,prob = p)
# PROBABILIDAD DE NECESITAR  MAS DE 5
promas <- 1-  proacum
print(promas)  
---------------------------------------
  #  4. Un jugador de póker tiene una probabilidad del 2% de recibir un as en su mano inicial. La
  #distribución geométrica se puede usar para calcular la probabilidad de que el jugador necesite más de
  #3 manos para recibir su primer as.
  p<- 0.02
# PROBABILIDAD ACUMULADA DE HASTA 3
proacum <- pgeom(3,prob = p)
# PROBABILIDAD DE NECESITAR  MAS DE 3
promas <- 1-  proacum
print(promas)
---------------------------------------------------------------------------
  #5. Una empresa de marketing envía correos electrónicos a una lista de clientes potenciales. La
  #probabilidad de que un cliente potencial abra el correo electrónico es del 10%. La distribución
  #geométrica se puede usar para calcular la probabilidad de que se necesiten más de 5 correos
  #electrónicos para que un cliente potencial abra uno.
  p<- 0.1
# PROBABILIDAD ACUMULADA DE HASTA 5
proacum <- pgeom(5,prob = p)
# PROBABILIDAD DE NECESITAR  MAS DE 5
promas <- 1-  proacum
print(promas)
------------------------------------------------------
  #  6. Un jugador de béisbol tiene una probabilidad del 25% de conectar una pelota en un turno al bate. La
  # distribución geométrica se puede usar para calcular la probabilidad de que el jugador necesite más de
  #4 turnos al bate para conectar su primer hit.
  p<- 0.25
# PROBABILIDAD ACUMULADA DE HASTA 4
proacum <- pgeom(4,prob = p)
# PROBABILIDAD DE NECESITAR  MAS DE 4
promas <- 1-  proacum
print(promas)
-------------------------------------------------------------------
  #  7. Un vendedor tiene una probabilidad del 30% de cerrar una venta en una llamada de ventas. La
  #distribución geométrica se puede usar para calcular la probabilidad de que el vendedor necesite más
  #de 6 llamadas de ventas para cerrar su primera venta.
  p<- 0.30
# PROBABILIDAD ACUMULADA DE HASTA 6
proacum <- pgeom(6,prob = p)
# PROBABILIDAD DE NECESITAR  MAS DE 6
promas <- 1-  proacum
print(promas)
------------------------------------------------------------------------
  #  8. La probabilidad de que un estudiante pase un examen es del 60%. La distribución geométrica se
  #puede usar para calcular la probabilidad de que el estudiante necesite más de 4 intentos para pasar el
  #examen.
  p<- 0.60
# PROBABILIDAD ACUMULADA DE HASTA 4
proacum <- pgeom(4,prob = p)
# PROBABILIDAD DE NECESITAR  MAS DE 4
promas <- 1-  proacum
print(promas)
----------------------------------------------------------
  #9. Un jugador de fútbol americano tiene una probabilidad del 5% de interceptar un pase en un juego.
  #La distribución geométrica se puede usar para calcular la probabilidad de que el jugador necesite más
  #de 10 intentos para interceptar su primer pase.
  p<- 0.05
# PROBABILIDAD ACUMULADA DE HASTA 10
proacum <- pgeom(10,prob = p)
# PROBABILIDAD DE NECESITAR  MAS DE 1O
promas <- 1-  proacum
print(promas)
----------------------------------------------------------
  # 10. Un pescador tiene una probabilidad del 10% de capturar un pez en cada lance de su caña de
  #pesca. La distribución geométrica se puede usar para calcular la probabilidad de que el pescador
  #necesite más de 8 lances para capturar su primer pez.
  p<- 0.1
# PROBABILIDAD ACUMULADA DE HASTA 8
proacum <- pgeom(8,prob = p)
# PROBABILIDAD DE NECESITAR  MAS DE 8
promas <- 1-  proacum
print(promas)
----------------------------------------------------------------------------------
  ## Sección 1.1: Variables Aleatorias Continuas
  
  #Aquí puedes hablar sobre lo que es RMarkdown y por qué es una herramienta útil para escribir y
  #publicar libros.
  
  ### Subsección 1.1.1: Distribución Uniforme
  *#*Plantea y resuelve los siguientes ejercicios**
  
  #1. Un fabricante de piezas mecánicas sabe que la longitud de sus piezas puede variar en un rango de
  # 10 a 20 cm. La variable aleatoria uniforme continua se puede usar para modelar la longitud de las
  #piezas que producen.
  n<- 1000
a<- 10
b<- 20
piezas <- runif(n,min = a,max = b)

---------------------------------------------------------------------
  #  2. Un negocio de alquiler de bicicletas sabe que el tiempo que los clientes alquilan sus bicicletas varía
  #entre 1 y 4 horas. La variable aleatoria uniforme continua se puede usar para modelar la duración de
  #los alquileres.
  n<- 1000
a<- 1
b<- 4
piezas <- runif(n,min = a,max = b)

----------------------------------------------------------------------
  #  3. Un restaurante sabe que la cantidad de tiempo que los clientes pasan en el restaurante varía entre
  #30 minutos y 2 horas. La variable aleatoria uniforme continua se puede usar para modelar el tiempo
  #de estancia de los clientes.
  n<- 1000
a<- 30
b<- 20
piezas <- runif(n,min = a,max = b)

--------------------------------------------------------------------------
  # 4. Una compañía de seguros sabe que la velocidad de los automóviles en una carretera puede variar
  #entre 60 y 120 km/h. La variable aleatoria uniforme continua se puede usar para modelar la velocidad
  #de los vehículos en la carretera.
  n<- 1000
a<- 10
b<- 20
piezas <- runif(n,min = a,max = b)

-------------------------------------------------------------------
  # 5. Un fabricante de ropa sabe que el peso de las camisetas que produce puede variar entre 100 y 200
  #gramos. La variable aleatoria uniforme continua se puede usar para modelar el peso de las
  #camisetas.
  n<- 1000
a<- 10
b<- 20
piezas <- runif(n,min = a,max = b)

----------------------------------------------------------------------
  #  6. Una tienda de electrónica sabe que el voltaje que suministra su fuente de alimentación puede variar
  #entre 110 y 220 voltios. La variable aleatoria uniforme continua se puede usar para modelar el voltaje
  #de la fuente de alimentación.
  n<- 1000
a<- 10
b<- 20
piezas <- runif(n,min = a,max = b)

------------------------------------------------
  # 7. Un servicio de mensajería sabe que la distancia entre los puntos de recogida y entrega puede variar
  #entre 5 y 20 km. La variable aleatoria uniforme continua se puede usar para modelar la distancia de
  #entrega.
  n<- 1000
a<- 10
b<- 20
piezas <- runif(n,min = a,max = b)

---------------------------------------------------------------
  #  8. Un fabricante de televisores sabe que el tiempo que tardan sus empleados en ensamblar un
  #televisor puede variar entre 1 y 3 horas. La variable aleatoria uniforme continua se puede usar para
  #modelar el tiempo de ensamblaje.
  n<- 1000
a<- 10
b<- 20
piezas <- runif(n,min = a,max = b)

---------------------------------------------------------------------------
  # 9. Una empresa de telecomunicaciones sabe que la velocidad de descarga de su red puede variar
  #entre 10 y 50 Mbps. La variable aleatoria uniforme continua se puede usar para modelar la velocidad
  #de descarga de la red.
  n<- 1000
a<- 10
b<- 20
piezas <- runif(n,min = a,max = b)

-----------------------------------------------------------------------
  #  10. Un concesionario de autos sabe que el precio de sus automóviles puede variar entre $10,000 y
  # $30,000. La variable aleatoria uniforme continua se puede usar para modelar el precio de los
  #automóviles en el concesionario.
  n<- 1000
a<- 10
b<- 20
piezas <- runif(n,min = a,max = b)

--------------------------------------------------------------------------
  ### Subsección 1.1.1: Distribución Exponencial
  
  #Plantea y resuelve los siguientes ejercicios**
  
  #1. Una empresa de mensajería promete entregar los paquetes dentro de 2 horas desde que se realiza
  #el pedido. El tiempo que tarda en entregarse un paquete sigue una distribución exponencial con una
  #tasa de 0.5. ¿Cuál es la probabilidad de que un paquete se entregue en menos de 1 hora?
  lambda <-0.5
x<- 1
probabilidad <- 1 -exp(-lambda *x)
print(probabilidad)

------------------------------------------------------------------------------
  # 2. La vida útil de las bombillas de una fábrica sigue una distribución exponencial con una media de
  #800 horas. ¿Cuál es la probabilidad de que una bombilla dure más de 1000 horas?
  media <- 800
tiempo <-1000
probabilidad<- 1- pexp(tiempo, rate = 1/ media)
print(probabilidad)
-------------------------------------------------------------------
  #  3. Un sistema informático tiene una tasa de fallos de 0.01 por hora. ¿Cuál es la probabilidad de que el
  #sistema falle en menos de 50 horas?
  tasafallos <- 0.01
tiempo <- 50
probabilidad <- pexp(tiempo,rate = tasafallos)
print(probabilidad)
--------------------------------------------------------------------
  #    4. Un vendedor tarda en promedio 15 minutos en atender a un cliente en su tienda. El tiempo que
  #tarda en atender a los clientes sigue una distribución exponencial con una tasa de 0.25. ¿Cuál es la
  #probabilidad de que atienda a un cliente en menos de 10 minutos?
  tasaten <- 0.25
tiempo <- 10
probabilidad <- pexp(tiempo,rate = tasaten)
print(probabilidad)

---------------------------------------------------------------------
  #   5. El tiempo entre llegadas de dos clientes a una tienda sigue una distribución exponencial con una
  #media de 5 minutos. ¿Cuál es la probabilidad de que no llegue ningún cliente en 10 minutos?
  tasalleg <- 0.2
tiempo <- 10
probabilidad <- 1- exp(-tasalleg* tiempo)
print(probabilidad)
----------------------------------------------------------------
  #  6. El tiempo que tarda una persona en recorrer una distancia de 5 km en bicicleta sigue una
  #distribución exponencial con una media de 30 minutos. ¿Cuál es la probabilidad de que la persona
  #tarde menos de 20 minutos?
  tasalleg <- 0.0333
tiempo <- 20
probabilidad <- 1- exp(-tasalleg* tiempo)
print(probabilidad)
-----------------------------------------------------------------
  #  7. El tiempo que tarda una máquina en producir una pieza sigue una distribución exponencial con una
  # tasa de 0.003 piezas por minuto. ¿Cuál es la probabilidad de que la máquina produzca al menos 10
  #piezas en 5 minutos?
  tasaprod <-  0.003
tiempo <- 5
probabilidad <- exp(-tasaprod* tiempo)
print(probabilidad)
-----------------------------------------------------------------------
  #  8. El tiempo que tarda un proceso industrial en completarse sigue una distribución exponencial con
  #una media de 8 horas. ¿Cuál es la probabilidad de que el proceso tarde más de 10 horas en
  #completarse?
  media <- 8
tasa <- 1/media
tiempo <- 10
probabilidad <<- exp(-tasa *tiempo)
print(probabilidad)
----------------------------------------------------------------------
  #  9. El tiempo que tarda un cliente en pagar en una caja sigue una distribución exponencial con una
  #media de 2 minutos. Si hay 10 clientes esperando en la fila, ¿cuál es la probabilidad de que un cliente
  #nuevo tenga que esperar más de 5 minutos para pagar?
  media <- 2
tasa <- 1/media
tiempo <- 5
probabilidad <- exp(-tasa *tiempo)
print(probabilidad) 
-----------------------------------------------------------------
  #    10. La tasa de llegada de un evento de demanda en un centro de atención telefónica sigue una
  #distribución exponencial con una media de 4 eventos por hora. ¿Cuál es la probabilidad de que no
  #llegue ningún evento en 10 minutos?
  media <- 60
tasa <- 4/media
tiempo <- 10
probabilidad <- pexp(tiempo, rate = tasa) - pexp(0, rate = tasa)
print(probabilidad)
---------------------------------------------------------------
  # 11. El tiempo que tarda una persona en llegar a una estación de tren sigue una distribución
  #exponencial con una media de 20 minutos. Si una persona tiene que tomar un tren que sale en 30
  #minutos, ¿cuál es la probabilidad de que llegue a tiempo?
  mediamin <- 20
tiempo <- 30
tasa <- 1/mediamin
probabilidad <<- exp(-tasa *tiempo)
print(probabilidad)
------------------------------------------------------------------- 
  #  12. El tiempo que tarda un coche en recorrer una distancia de 100 km en una carretera sigue una
  #distribución exponencial con una media de 2 horas. ¿Cuál es la probabilidad de que el coche tarde
  #menos de 1 hora y media?
  media <- 2
tiempo <- 1.5
dist<- 100
velocmed <- dist /media
velocmedmin <- velocmed * 60
tasa <- 1/velocmedmin
probabilidad <<- pexp(tiempo, rate = tasa)
print(probabilidad)
--------------------------------------------------------------
  #    13. Se sabe que el tiempo que tarda un técnico en reparar un electrodoméstico sigue una distribución
  #exponencial con una tasa de 0.05. ¿Cuál es la probabilidad de que el técnico repare el
  #electrodoméstico en menos de 20 minutos?
  
  tasa <- 0.05
tiempo <- 20
probabilidad <- pexp(tiempo, rate = tasa)
print(probabilidad)
----------------------------------------------------------------------
  #   14. Una empresa de mensajería sabe que el tiempo que tarda en entregar un paquete sigue una
  #distribución exponencial con una tasa de 0.1. Si se tienen que entregar 100 paquetes, ¿cuál es la
  #probabilidad de que la empresa tarde menos de 30 minutos en entregar cada paquete?
  numpaq <- 100
tasa <- 0.1
tiempo <- 30
probabilidad <- 0
for (i in 1: numpaq) {
  probabilidad <- probabilidad+ pexp(tiempo, rate = tasa)
  
}
print(probabilidad)
-----------------------------------------------------------------
  # 15. Se sabe que el tiempo que tarda en atenderse un cliente en una tienda sigue una distribución
  #exponencial con una tasa de 0.2. ¿Cuál es la probabilidad de que un cliente espere más de 10 minutos
  #para ser atendido?
  
  tasa <- 0.2
tiempo <- 10
probabilidad <- 1 - pexp(tiempo,rate = tasa)
print(probabilidad)
--------------------------------------------------------------------------  
  #  16. Un fabricante de baterías sabe que la duración de sus baterías sigue una distribución exponencial
  #con una tasa de 0.01. Si se venden 500 baterías, ¿cuál es la probabilidad de que al menos 10 de ellas
  #duren más de 100 horas?
  
  tasa <- 0.01
numbact <- 500
numdurar <- 10
probabilidad <- 1- pexp(100,rate=tasa )^numdurar
print(probabilidad)
----------------------------------------------------------------------------
  #    17. Un vendedor de autos usados sabe que la cantidad de tiempo que tarda en vender un auto sigue
  #una distribución exponencial con una tasa de 0.1. Si tiene 20 autos en venta, ¿cuál es la probabilidad
  #de que tarde más de 5 horas en venderlos todos? media <- 8
  tasa <- 0.1
numauntos <- 20
tiempolim <- 5
probabilidad<- 1- pexp(tiempolim,rate = tasa)^numauntos
print(probabilidad)
----------------------------------------------------------------------- 
  #  18. Se sabe que la cantidad de tiempo que tarda una persona en caminar desde su casa hasta la
  #estación de tren sigue una distribución exponencial con una tasa de 0.05. Si la distancia es de 1
  #kilómetro, ¿cuál es la probabilidad de que la persona tarde menos de 20 minutos en llegar a la
  #estación?
  
  tasa <- 0.05
dist<- 1
tiempolim <- 20
probabilidad <<- pexp(tiempolim,rate = tasa*dist)
print(probabilidad)
--------------------------------------------------------------
  # 19. Un banco sabe que la cantidad de tiempo que tarda un cliente en realizar una transacción sigue
  #una distribución exponencial con una tasa de 0.15. Si se tienen que atender a 50 clientes, ¿cuál es la
  #probabilidad de que el banco tarde menos de 2 horas en atenderlos a todos?
  
  tasa <- 0.15
numcli <-50
tiempolim <- 2
probabilidad <- pexp(tiempolim*60, rate = tasa*numcli)
print(probabilidad)
------------------------------------------------------
  #  20. Se sabe que la cantidad de tiempo que tarda en fallar un equipo electrónico sigue una distribución
  #exponencial con una tasa de 0.02. Si se tienen 100 equipos en operación, ¿cuál es la probabilidad de
  #que al menos 5 fallen en menos de 50 horas?
  
  tasa <- 0.02
numequi <- 100
tiempolim <- 50
numfal <- 5
probabilidad <- 1- pexp(tiempolim,rate = tasa*numequi)-
  sum(dexp(seq(0,tiempolim),rate =  tasa * numequi,log = TRUE)*
        choose(numequi,numfal)*
        (1- pexp(seq(0,tiempolim),rate = tasa))^numfal)
print(probabilidad)
--------------------------------------------------------------
  # 21. Un restaurante sabe que la cantidad de tiempo que tarda un cliente en terminar de comer sigue
  #una distribución exponencial con una tasa de 0.2. Si se tienen 30 mesas, ¿cuál es la probabilidad de
  #que al menos 5 de ellas estén desocupadas en menos de 1 hora?
  tasa <- 0.2
numesa <- 30
tiempolim <- 1
numdesoc <- 5
probabilidad <- 1- pexp(tiempolim,rate = tasa*numesa)
sum(dexp(seq(0,tiempolim),rate =  tasa * numesa,log = TRUE)*
      choose(numesa,numdesoc)*
      (1- pexp(seq(0,tiempolim),rate = tasa))^numdesoc)
print(probabilidad)
------------------------------------------------------------------
  #  22. Un servidor web sabe que la cantidad de tiempo que tarda en procesar una solicitud sigue una
  #distribución exponencial con una tasa de 0.05. Si recibe 100 solicitudes, ¿cuál es la probabilidad de
  #que tarde menos de 10 minutos en procesar cada una?
  tasa <- 0.05
numsol <-100
tiempolim <- 10
probabilidad <<- pexp(tiempolim,rate = tasa)^numsol
print(probabilidad)
-------------------------------------------------------------------------
  
  ### Subsección 1.1.1: Distribución Normal
  
  # 1. Una fábrica de papel produce rollos de papel higiénico que tienen una longitud media de 100
  # metros y una desviación estándar de 5 metros. ¿Cuál es la probabilidad de que un rollo de papel
  #higiénico tenga una longitud entre 95 y 105 metros?
  # Calcular la probabilidad
  probabilidad <- pnorm(105, mean = 100, sd = 5) - pnorm(95, mean = 100, sd = 5)
print(probabilidad)
-----------------------------------------------
  #2. La altura de los estudiantes de una escuela secundaria sigue una distribución normal con media de
  #165 cm y desviación estándar de 10 cm. ¿Cuál es la probabilidad de que un estudiante seleccionado
  #al azar tenga una altura entre 150 y 180 cm?
  probabilidad <- pnorm(180, mean = 165, sd = 10) - pnorm(150, mean = 165, sd = 10)
print(probabilidad)

-------------------------------------------------------------------
  #   3. Un fabricante de zapatos produce zapatos que tienen una longitud media de 28 cm y una
  #desviación estándar de 2 cm. ¿Cuál es la probabilidad de que un zapato seleccionado al azar tenga
  #una longitud mayor que 32 cm?
  probabilidad <- 1- pnorm(32, mean = 28, sd = 2)
print(probabilidad)
-----------------------------------------------------------------
  #  4. La distribución de las calificaciones en un examen sigue una distribución normal con una media de
  # 70 y una desviación estándar de 10. ¿Cuál es la probabilidad de que un estudiante seleccionado al
  # azar tenga una calificación menor a 60?
  probabilidad <- 1 -pnorm(60,mean = 70,sd =10)
print(probabilidad)
------------------------------------------------------------
  #  5. La duración de las baterías de un dispositivo electrónico sigue una distribución normal con una
  #media de 10 horas y una desviación estándar de 2 horas. ¿Cuál es la probabilidad de que una batería
  #seleccionada al azar dure más de 12 horas?
  probabilidad <- 1 -pnorm(12,mean = 10,sd =2)
print(probabilidad)
-------------------------------------------------------------
  #    6. La distribución de los tiempos de espera en una fila sigue una distribución normal con una media
  # de 5 minutos y una desviación estándar de 1 minuto. ¿Cuál es la probabilidad de que un cliente tenga
  # que esperar más de 8 minutos?
  probabilidad <- 1 -pnorm(8,mean = 5,sd =1)
print(probabilidad)
--------------------------------------------------------
  #   7. El peso de los recién nacidos sigue una distribución normal con una media de 3.5 kg y una
  # desviación estándar de 0.5 kg. ¿Cuál es la probabilidad de que un recién nacido pese menos de 2.5
  # kg?
  probabilidad <- pnorm(2.5,mean = 3.5,sd =0.5)*100
print(probabilidad)
---------------------------------------------------------------------
  # 8. La velocidad de un vehículo sigue una distribución normal con una media de 60 km/h y una
  # desviación estándar de 5 km/h. ¿Cuál es la probabilidad de que un vehículo viaje a una velocidad
  # mayor de 70 km/h?
  probabilidad <- 1 -pnorm(70,mean = 60,sd =5)
print(probabilidad)
------------------------------------------------------------------------
  #   9. La distribución de los ingresos de los trabajadores en una empresa sigue una distribución normal
  #  con una media de $\$50,000$ y una desviación estándar de $\$10,000$. ¿Cuál es la probabilidad de
  #que un trabajador gane menos de $\$30,000$?
  probabilidad <- pnorm(30000,mean = 50000,sd =10000)*100
print(probabilidad)
-----------------------------------------------------------------------
  #10. La temperatura de una ciudad sigue una distribucion normal con una media de 25 grados Celsius y una
  #desviacion estandar de 2 grados Celsius. ¿Cual es la probabilidad de que la temperatura sea mayor 
  #de 30 grados Celsius? Calcular la probabilidad
  probabilidad <- 1 - pnorm(30, mean = 25, sd = 2)
print(probabilidad)
----------------------------------------------------------------------------------         
  #  11. La altura de las botellas de refresco sigue una distribución normal con una media de 20 cm y una
  # desviación estándar de 1 cm. ¿Cuál es la probabilidad de que una botella seleccionada al azar tenga
  # una altura menor a 18 cm?
  probabilidad <- 1 -pnorm(60,mean = 70,sd =10)
print(probabilidad)
---------------------------------------------------------------
  #  12. Si los puntajes de un examen están distribuidos normalmente con una media de 70 y una
  #desviación estándar de 10, ¿cuál es la probabilidad de obtener un puntaje mayor a 80?
  probabilidad <- 1 -pnorm(80,mean = 70,sd =10)
print(probabilidad)
---------------------------------------------------------------------
  #   13. Suponga que el tiempo que se tarda en atender a un cliente en un banco sigue una distribución
  # normal con media de 5 minutos y una desviación estándar de 1.2 minutos. ¿Cuál es la probabilidad de
  # que un cliente sea atendido en menos de 4 minutos?
  probabilidad <- pnorm(4,mean = 5,sd =1.2)
print(probabilidad) 
-------------------------------------------------------- 
  #  14. Un estudio indica que el peso promedio de los estudiantes de una universidad es de 70 kg con una
  # desviación estándar de 8 kg. Si se selecciona una muestra aleatoria de 25 estudiantes, ¿cuál es la
  # probabilidad de que el peso promedio de la muestra sea mayor a 75 kg?
  
  probabilidad <- 1 - pnorm(75, mean = 70, sd =8/sqrt(25))
print(probabilidad)
---------------------------------------------------------
  #  15. Suponga que la altura de los jugadores de un equipo de baloncesto se distribuye normalmente
  # con una media de 200 cm y una desviación estándar de 10 cm. Si se selecciona un jugador al azar,
  # ¿cuál es la probabilidad de que su altura sea mayor a 215 cm?
  probabilidad <- 1 - pnorm(215, mean = 200, sd = 10)
print(probabilidad)
---------------------------------------
  #  16. Si los ingresos mensuales de los empleados de una empresa están distribuidos normalmente con
  # una media de $\$2500$ y una desviación estándar de $\$500$, ¿cuál es la probabilidad de que un
  # empleado tenga un ingreso mensual mayor a $\$3000$?
  probabilidad <- 1 - pnorm(3000, mean = 2500, sd = 500)
print(probabilidad)
-------------------------------------------------
  #  17.La cantidad de días que tarda una empresa en pagar a sus proveedores sigue una distribución
  # normal con una media de 30 días y una desviación estándar de 5 días. Si se selecciona un proveedor
  #al azar, ¿cuál es la probabilidad de que la empresa le pague en menos de 25 días?
  probabilidad <- pnorm(25, mean = 30, sd = 5)
print(probabilidad)
-----------------------------------------------------------
  #  18.Suponga que el diámetro de los tornillos producidos por una fábrica sigue una distribución normal
  # con media de 4 cm y una desviación estándar de 0.2 cm. Si se selecciona un tornillo al azar, ¿cuál es
  # la probabilidad de que su diámetro esté entre 3.8 cm y 4.2 cm?
  probabilidad <- pnorm(4.2, mean = 4, sd = 0.2) - pnorm(3.8, mean = 4, sd = 0.2)
print(probabilidad)      
---------------------------------------------------------
  #  19. El tiempo que tarda un trabajador en completar una tarea sigue una distribución normal con media
  # de 6 horas y una desviación estándar de 1 hora. Si se selecciona un trabajador al azar, ¿cuál es la
  # probabilidad de que complete la tarea en menos de 4 horas?
  probabilidad <- pnorm(4, mean = 6, sd = 1)
print(probabilidad)
---------------------------------------------------------
  #    20. Si la cantidad de gasolina que se llena en un tanque de un automóvil sigue una distribución
  # normal con una media de 50 litros y una desviación estándar de 5 litros, ¿cuál es la probabilidad de
  # que se llenen entre 45 y 55 litros?
  probabilidad <- pnorm(55, mean = 50, sd = 5) - pnorm(45,mean = 50, sd=5)
print(probabilidad)
--------------------------------------------------------
  
  #  21. El peso de los paquetes que se envían por una compañía de paquetería sigue una distribución
  # normal con una media de 2 kg y una desviación estándar de 0.5 kg. Si se selecciona un paquete al
  #azar, ¿cuál es la probabilidad de que su peso esté entre 1.5 kg y 2.5 kg?
  probabilidad <- pnorm(2.5, mean = 2, sd = 0.5) - pnorm(1.5,mean = 2, sd= 0.5)
print(probabilidad)

------------------------------------------------------------------------------------------
  ### Subsección 1.1.1: Distribución Gamma
  
  
  #Resuelve los ejercicios siguientes**
  # 1. Un fabricante de bombillas sabe que la vida útil de las bombillas sigue una distribución gamma con
  # una media de 1500 horas y una desviación estándar de 500 horas. ¿Cuál es la probabilidad de que
  # una bombilla dure más de 2000 horas?
  probabilidad <- 1- pgamma (2000,shape = (1500/5000)^2,scale = 500^2)
print(probabilidad) #resultado 1
--------------------------------------------------------------------------
  #    2. Una planta de producción de papel produce rollos de papel de 100 metros de longitud. La
  # distribución de la longitud de los rollos sigue una distribución gamma con una media de 105 metros y
  # una desviación estándar de 10 metros. ¿Cuál es la probabilidad de que un rollo tenga una longitud de
  #entre 90 y 110 metros?
  probabilidad <- pgamma (110,shape = (105/10)^2,scale = 10^2)- 
  pgamma(90,shape=(105/10)^2,scale =10^2)
print(probabilidad) #resultado 2         
--------------------------------------------------------------
  #   3. La cantidad de tiempo que un grupo de estudiantes dedica a estudiar para un examen sigue una
  # distribución gamma con una media de 20 horas y una desviación estándar de 5 horas. Si el tiempo
  # mínimo requerido para pasar el examen es de 15 horas, ¿cuál es la probabilidad de que un estudiante
  # al azar pase el examen?
  probabilidad <- 1- pgamma (15,shape = (20/5)^2,scale = 5^2)
print(probabilidad) #resultado 3                          
---------------------------------------------------------------
  #   4. Un fabricante de baterías de automóvil sabe que el tiempo de vida de las baterías sigue una
  # distribución gamma con una media de 4 años y una desviación estándar de 1 año. ¿Cuál es la
  # probabilidad de que una batería dure menos de 2 años?
  probabilidad <- pgamma (2,shape = (4/1)^2,scale = 1^2)
print(probabilidad) #resultado 5                       
--------------------------------------------------------------------
  #    5. La cantidad de tiempo que una persona tarda en completar una tarea sigue una distribución
  # gamma con una media de 10 minutos y una desviación estándar de 2 minutos. Si se requiere que la
  # tarea se complete en un tiempo máximo de 12 minutos, ¿cuál es la probabilidad de que una persona
  # al azar pueda completar la tarea a tiempo?
  probabilidad <- pgamma (12,shape = (10/2)^2,scale = 2^2)
print(probabilidad) #resultado 5
---------------------------------------------------------
  # 6. Una fábrica de muebles produce sillas de madera. La cantidad de tiempo que tarda en producir una
  #silla sigue una distribución gamma con una media de 2 horas y una desviación estándar de 0.5 horas.
  #Si se requiere que se produzcan 10 sillas en un día de trabajo, ¿cuál es la probabilidad de que se logre
  #completar la producción en un día?
  probabilidad <- pgamma (10,shape = (2/0.5)^2,scale = 0.5^2)
print(probabilidad) #resultado 6                        
-------------------------------------------------------------
  #    7. Un fabricante de relojes sabe que la vida útil de sus relojes sigue una distribución gamma con una
  #media de 5 años y una desviación estándar de 1 año. Si un reloj es considerado defectuoso si dura
  #menos de 3 años, ¿cuál es la probabilidad de que un reloj al azar sea considerado defectuoso?
  probabilidad <- pgamma (3,shape = (5/1)^2,scale = 1^2)
print(probabilidad) #resultado 7
--------------------------------------------------------
  #    8. Un fabricante de focos sabe que la vida útil de sus focos sigue una distribución gamma con una
  # media de 800 horas y una desviación estándar de 200 horas. Si se requiere que los focos duren al
  # menos 1000 horas, ¿cuál es la probabilidad de que un foco al azar dure el tiempo necesario?
  probabilidad <- 1- pgamma (1000,shape = (800/200)^2,scale = 200^2)
print(probabilidad) #resultado 8                        
---------------------------------------------------------
  #   9. La cantidad de tiempo que tarda un técnico en reparar un dispositivo electrónico sigue una
  # distribución gamma con una media de 3 horas y una desviación estándar de 0.5 horas. Si se requiere
  # que el dispositivo sea reparado en un tiempo máximo de 4 horas, ¿cuál es la probabilidad de que el
  # técnico al azar pueda reparar el dispositivo a tiempo?
  probabilidad <- pgamma (4,shape = (3/0.5)^2,scale = 0.5^2)*100
print(probabilidad) #resultado 9                        
------------------------------------------------------------------
  #  10. Se sabe que el tiempo de vida (en años) de una lámpara sigue una distribución gamma con
  # parámetros $\alpha=3$ y $\beta=2$. Encuentra la probabilidad de que la lámpara dure más de 4 años.
  probabilidad <- (1- pgamma (4,shape = 3,rate =2))*100
print(probabilidad) #resultado 10
------------------------------------------------------
  #11. Un servicio de reparación de electrodomésticos tiene un promedio de 10 reparaciones diarias con
  # una desviación estándar de 2 reparaciones. Si se modela el número de reparaciones diarias como una
  # distribución gamma, ¿cuál es el valor del parámetro $\alpha$?
  madia <- 10 
desvesta <- 2
varianza <- desvesta^2
alpha <- (media^2)/varianza
print(alpha) # resultado 11
------------------------------------
  # 12. El tiempo que tarda en llegar un cliente al restaurante sigue una distribución gamma con
  # parámetros $\alpha=2$ y $\beta=3$. ¿Cuál es la probabilidad de que un cliente tarde más de 5
  # minutos en llegar?
  alpha <- 2
beta <- 3
umbral <- 5
probabilidad <-  pgamma(umbral,shape =alpha, rate = beta)*100
print(probabilidad)# resultado 12
-----------------------------------------------------------
  # 13. Se sabe que la cantidad de kilómetros que recorre un taxi en un día sigue una distribución gamma
  # con parámetros $\alpha=4$ y $\beta=6$. ¿Cuál es la probabilidad de que el taxi recorra menos de 20
  # kilómetros en un día?
  alpha <- 4
beta <- 6
umbral <- 20
probabilidad <-  pgamma(umbral,shape =alpha, rate = beta)*100
print(probabilidad)# resultado 13
--------------------------------------------------
  #   14. El tiempo que tarda un empleado en resolver una tarea sigue una distribución gamma con
  # parámetros $\alpha=5$ y $\beta=2$. ¿Cuál es el valor esperado del tiempo de resolución de la tarea?
  alpha <- 5
beta <- 2

valor<-alpha/ beta
print(valor)# resultado 14
------------------------------------
  #  15. Un grupo de personas está siendo tratado con un medicamento cuyos efectos secundarios siguen
  # una distribución gamma con parámetros $\alpha=2$ y $\beta=4$. ¿Cuál es la probabilidad de que el
  # efecto secundario más duradero dure más de 6 horas?
  alpha <- 2
beta <- 4

probabilidad <-  pgamma(6,shape =alpha, rate = 1, lower.tail = FALSE)*100
print(probabilidad)# resultado 12
--------------------------------------------------------
  # 16. La cantidad de tiempo que tarda una máquina en fabricar una pieza sigue una distribución gamma
  # con parámetros $\alpha=4$ y $\beta=5$. ¿Cuál es la probabilidad de que la máquina tarde menos de
  # 10 minutos en fabricar una pieza?
  alpha <- 4
beta <- 5
probabilidad <-  pgamma(10,shape =alpha, rate = 1/beta)*100
print(probabilidad)# resultado 16
----------------------------------------------------
  #  17. Se sabe que la cantidad de kilómetros que puede recorrer un coche con un tanque de gasolina
  # sigue una distribución gamma con parámetros $\alpha=3$ y $\beta=8$. Si el coche tiene un tanque
  # de gasolina de 40 litros, ¿cuál es la probabilidad de que pueda recorrer más de 600 kilómetros con un
  # tanque lleno?
  #convertir de kilómetros  a litro 
  km <- 600 
lt <- km/(3/8)
alpha <- 3
beta <- 8
probabilidad <- pgamma(lt,shape =alpha, rate = 1/beta)*100
print(probabilidad)# resultado 17
------------------------------------------------------------
  #   18. El tiempo que tarda un equipo de trabajo en completar un proyecto sigue una distribución gamma
  # con parámetros $\alpha=6$ y $\beta=3$. ¿Cuál es la varianza del tiempo de completación del
  # proyecto?
  alpha <- 6
beta <- 3
varianza <- alpha/(beta^2)
print(varianza)# resultado 18
-----------------------------------------------------------
  #  19. Se sabe que el tiempo que tarda en salir un autobús desde la terminal sigue una distribución
  # gamma con parámetros $\alpha=3$ y $\beta=2$. ¿Cuál es la probabilidad de que el autobús tarde
  #  más de 5 minutos en salir?
  alpha <- 3
beta <- 2
tiempolim <- 5
probabilidad <-  1-pgamma(tiempolim,shape =alpha, rate = 1/beta)
print(probabilidad)# resultado 19
-----------------------------------------------
  #  20. La cantidad de tiempo que tarda un paquete en ser entregado sigue una distribución gamma con
  # parámetros $\alpha=5$ y $\beta=4$. ¿Cuál es la probabilidad de que el paquete sea entregado antes
  # de 25 minutos?alpha <- 2
  beta <- 3
umbral <- 5
probabilidad <-  pgamma(umbral,shape =alpha, rate = beta)*100
print(probabilidad)# resultado 20
---------------------------------------------------------
  #    21. Un proceso de producción tiene un promedio de 10 piezas producidas por hora con una
  # desviación estándar de 2 piezas. Si se modela el número de piezas producidas por hora como una
  # distribución gamma, ¿cuál es el valor del parámetro $\beta$
  pro <- 10
desvesta <- 2
beta <- pro/ desvesta^2
print(beta)# resultado 21
---------------------------------------------------------------------------------------------------
  ### Subsección 1.1.1: Distribución Beta
  
  # Resuelve los siguientes problemas**
  #   1. Un sitio web de videos desea medir la tasa de clics en su botón de "reproducir". Si se sabe que el
  # 10% de los usuarios hacen clic en el botón, ¿cuál es la probabilidad de que de 20 usuarios,
  # exactamente 3 hagan clic?
  usuariostot <- 20
usuarioclic <- 3
probaclic <- 0.1
probabilidad<- dbinom(usuarioclic, usuariostot, probaclic)
print(probabilidad)

---------------------------------------------------------------------
  # 2. Una empresa quiere saber la probabilidad de que un producto defectuoso se encuentre en el
  # proceso de producción. Si se sabe que la tasa de producción de productos defectuosos es del 5%,
  # ¿cuál es la probabilidad de que de 200 productos producidos, exactamente 12 sean defectuosos?
  productos_totales <- 200
productos_defectuosos <- 12
tasa_defectuosos <- 0.05
probabilidad_exacta <- choose(productos_totales, productos_defectuosos) * 
  dbeta(tasa_defectuosos, productos_defectuosos + 1, productos_totales - productos_defectuosos + 1) *
  dbinom(productos_defectuosos, productos_totales, tasa_defectuosos)
print(probabilidad_exacta)
-#---------------------------------------------------------------
#   3. Un sitio web de citas desea medir la tasa de conversión de las personas que visitan su sitio y se
#suscriben a un plan premium. Si se sabe que el 2% de las personas que visitan el sitio se suscriben a
# un plan premium, ¿cuál es la probabilidad de que de 500 visitas, exactamente 10 personas se
# suscriban?
vistotales <- 500
personas <- 10
tasaconver <- 0.02
probabilidad_exacta <- dbinom(personas, vistotales, tasaconver)
print(probabilidad_exacta)
--------------------------------------------------
  #  4. Una compañía desea medir la tasa de conversión de los anuncios en línea. Si se sabe que el 20% de
  # las personas que ven el anuncio hacen clic en él, ¿cuál es la probabilidad de que de 50 vistas,
  # exactamente 12 personas hagan clic?
  vistotales <- 50
personas <- 12
tasaconver <- 0.2
probabilidad_exacta <- dbeta(tasaconver, personas + 1, vistotales - personas + 1)
print(probabilidad_exacta)
----------------------------------------------------------------------
  #  5. Un restaurante desea medir la satisfacción de sus clientes en relación con un nuevo plato que
  # acaba de agregar al menú. Si se sabe que el 70% de los clientes están satisfechos con el nuevo plato,
  # ¿cuál es la probabilidad de que de 100 clientes, exactamente 50 estén satisfechos?
  clitotales <- 100
clisatisfechos <- 50
tasasatisn <- 0.7
probabilidad_exacta <- dbeta(tasasatisn, clisatisfechos + 1, clitotales - clisatisfechos+ 1)
print(probabilidad_exacta)
--------------------------------------------------------------------
  #  6. Un fabricante de lámparas desea medir la tasa de fallas de sus productos. Si se sabe que el 3% de
  # las lámparas fallan, ¿cuál es la probabilidad de que de 500 lámparas producidas, exactamente 20
  # fallen?
  lamtotales <- 500
lamfalladas <- 20
tasafallas <- 0.03
probabilidad_exacta <- dbeta(tasafallas, lamfalladas + 1, lamtotales - lamfalladas + 1)
print(probabilidad_exacta)
----------------------------------------------------------------
  #  7. Una empresa desea medir la tasa de aprobación de sus solicitudes de crédito. Si se sabe que el
  # 80% de las solicitudes son aprobadas, ¿cuál es la probabilidad de que de 200 solicitudes,
  # exactamente 150 sean aprobadas?
  solitotales <- 200
soliAprobadas <- 150
tasaprobacion <- 0.8
probabilidad_exacta <- dbeta(tasaprobacion, soliAprobadas + 1, solitotales - soliAprobadas + 1)
print(probabilidad_exacta) 
---------------------------------------------------------------
  #  8. Una tienda de ropa desea medir la tasa de conversión de los visitantes en compradores. Si se sabe
  # que el 10% de las personas que visitan la tienda compran algo, ¿cuál es la probabilidad de que de 50
  # visitas, exactamente 5 personas compren?
  vistotales <- 50
compras_realizadas <- 5
tasaconver <- 0.1
probabilidad_exacta <- dbeta(tasaconver, compras_realizadas + 1, vistotales - compras_realizadas + 1)

print(probabilidad_exacta)
---------------------------------------------------------------------
  #  9. Un fabricante de televisores desea medir la tasa de fallas de sus productos. Si se sabe que el 2% de
  # los televisores fallan, ¿cuál es la probabilidad de que de 1000 televisores producidos, exactamente 15
  # fallen?
  teleproducidos <- 1000
fallas <- 15
tasafallas <- 0.02
probabilidad_exacta <- dbeta(tasafallas, fallas + 1, teleproducidos - fallas + 1)
print(probabilidad_exacta)
---------------------------------------------------------------------------------
  #  10. Un sitio web de noticias desea medir la tasa de clics en sus artículos. Si se sabe que el 15% de los
  # usuarios que visitan el sitio hacen clic en los artículos, ¿cuál es la probabilidad de que de 80 visitas,
  # exactamente 10 usuarios hagan clic en los artículos?
  visitas <- 80
clics <- 10
tasaclic <- 0.15
probabilidad_exacta <- dbeta(tasaclic, clics + 1, visitas - clics + 1)
print()
-------------------------------------------------------------------------
  #  11. Una empresa de marketing desea conocer el porcentaje de conversiones de su última campaña
  # publicitaria. Se seleccionan aleatoriamente 200 personas que han visto la publicidad y se observa que
  # 50 de ellas han realizado una compra. Suponiendo una distribución beta con parámetros a=2 y b=4,
  # ¿cuál es la probabilidad de que la tasa de conversión esté entre el 20% y el 30%?
  personvistas <- 200
personcompraron <- 50
alpha <- 2
beta <- 4
tasaconver_min <- 0.2
tasaconver_max <- 0.3
probabilidad_intervalo <- pbeta(tasaconver_max, personcompraron + alpha, personvistas - personcompraron + beta) -
  pbeta(tasaconver_min, personcompraron + alpha, personvistas - personcompraron + beta)
print(probabilidad_intervalo)
---------------------------------------------------- 
  #  12. Un fabricante de pinturas quiere determinar la proporción de pigmento que debe agregar a su
  #fórmula para obtener el color deseado. Se realiza una prueba en la que se mezcla la fórmula con
  #diferentes proporciones de pigmento y se mide la intensidad del color en una escala de 0 a 100.
  #Suponiendo una distribución beta con parámetros a=5 y b=2, ¿cuál es la probabilidad de que la
  #proporción de pigmento necesaria para obtener una intensidad de color de al menos 80 sea menor al
  # 30%?
  intendeseada <- 80
propormax <- 0.3
alpha <- 5
beta <- 2
probabilidad <- pbeta(propormax, alpha, beta)
print(probabilidad)
--------------------------------------------------------------
  #  13. Un estudio de opinión pública desea conocer el porcentaje de personas que apoyan una propuesta
  #de ley. Se seleccionan aleatoriamente 1000 personas y se les pregunta si están a favor o en contra. Se
  #observa que 650 están a favor. Suponiendo una distribución beta con parámetros a=7 y b=5, ¿cuál es
  #la probabilidad de que la tasa de aprobación esté entre el 60% y el 70%?
  npersonas <- 1000
naprobaciones <- 650
por_min <- 0.6
por_max <- 0.7
alpha <- 7
beta <- 5
probabilidad_aprobacion <- pbeta(por_max, alpha * naprobaciones, beta * (npersonas - naprobaciones)) -
  pbeta(por_min, alpha * naprobaciones, beta * (npersonas - naprobaciones))
print(probabilidad_aprobacion)
------------------------------------------------------
  #  14. Una empresa de seguros desea conocer la proporción de personas que renuevan su póliza de
  #seguro cada año. Se seleccionan aleatoriamente 5000 clientes y se observa que 4200 renuevan su
  #póliza. Suponiendo una distribución beta con parámetros a=15 y b=5, ¿cuál es la probabilidad de que
  #la tasa de renovación esté por encima del 85%?
  nclientes <- 5000
nrenovaciones <- 4200
porlimite <- 0.85
alpha <- 15
beta <- 5
probabilidad <- 1 - pbeta(porlimite, alpha * nrenovaciones, beta * (nclientes - nrenovaciones))
print(probabilidad)
------------------------------------------------------------
  #  15. Una compañía de transporte de paquetería desea conocer la proporción de envíos que llegan a su
  #destino en el plazo establecido. Se revisan aleatoriamente 100 envíos y se observa que 80 llegan a
  # Suponiendo una distribución beta con parámetros a=8 y b=2, ¿cuál es la probabilidad de que
  #la tasa de envíos que llegan a tiempo esté por encima del 85%?
  nenvios <- 100
nllegan_a_tiempo <- 80
porlimite <- 0.85
alpha <- 8
beta <- 2
probabilidad <- 1 - pbeta(porlimite, alpha * nllegan_a_tiempo, beta * (nenvios - nllegan_a_tiempo))
print(probabilidad)

-----------------------------------------------------------------
  # 16. Una empresa de telecomunicaciones desea conocer la proporción de clientes que utilizan su
  #servicio de streaming de video. Se seleccionan aleatoriamente 300 clientes y se observa que 150
  #utilizan el servicio. Suponiendo una distribución beta con parámetros a=10 y b=10, ¿cuál es la
  #probabilidad de que la tasa de uso del servicio esté entre el 40% y el 60%?
  nclientes <- 300
servicioN <- 150
porcentaje_inferior <- 0.4
porcentaje_superior <- 0.6
alpha <- 10
beta <- 10
probabilidad_tasa_uso <- pbeta(porcentaje_superior, alpha * servicioN, beta * (nclientes - servicioN)) -
  pbeta(porcentaje_inferior, alpha * servicioN, beta * (nclientes - servicioN))
print(probabilidad_tasa_uso)
--------------------------------------------------------------
  #  17. Una empresa de producción de alimentos desea conocer la proporción de productos que cumplen
  #con las especificaciones de calidad establecidas. Se seleccionan aleatoriamente 500 unidades de
  #producto y se observa que 460 cumplen con las especificaciones. Suponiendo una distribución beta
  #con parámetros a=6 y b=4, ¿cuál es la probabilidad de que la tasa de unidades que cumplen con las
  #especificaciones esté por encima del 90%?
  Nunidades <- 500
Ncumplen <- 460
porcentaje_superior <- 0.9
alpha <- 6
beta <- 4
probabilidad_tasa_cumplimiento <- 1 - pbeta(porcentaje_superior, alpha * Ncumplen, beta * (Nunidades - Ncumplen))
print(probabilidad_tasa_cumplimiento)                    
-----------------------------------------------------------------------------------------
  
  ### Subsección 1.1.1: Distribución T-Student
  
  #  1. Un fabricante de guantes médicos afirma que el tiempo medio de duración de sus guantes es de 3
  #horas, con una desviación estándar de 0.5 horas. Si tomamos una muestra aleatoria de 20 guantes,
  # ¿cuál es la probabilidad de que la duración media de los guantes sea mayor de 3.2 horas?
  xbarra1<- 3  
t <-3.2
s1 <- 0.5
n <- 20
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

probabilidad1 <- xbarra - z* s; print(probabilidad1)
probabilidad2 <- xbarra + z* s; print(probabilidad2)



----------------------------------------------------------
  # 2. Se sabe que el peso de los paquetes de cierta marca sigue una distribución normal con media de
  #  1.5 kg y desviación estándar de 0.3 kg. Si tomamos una muestra aleatoria de 10 paquetes, ¿cuál es la
  #probabilidad de que el peso promedio de la muestra sea menor de 1.3 kg?
  xbarra1<- 1.5 
t <-1.3
s1 <- 0.3
n <- 10
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

probabilidad1 <- xbarra - z* s; print(probabilidad1)
probabilidad2 <- xbarra + z* s; print(probabilidad2)


---------------------------------------------------------------
  #  3. Se desea estudiar la altura promedio de los estudiantes de una universidad. Se toma una muestra
  #aleatoria de 15 estudiantes y se obtiene una altura promedio de 1.75 metros, con una desviación
  #estándar de 0.15 metros. Si la altura promedio de la población es de 1.8 metros, ¿es significativo el
  #resultado obtenido?
  xbarra1<- 1.75  
t <-0.15
s1 <- 1.8
n <- 15
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
print('El resultado dice que hay una diferencia entre 1.46 y 1.64')
----------------------------------------------------
  #  4. Se sabe que el tiempo de vida de las baterías de cierto modelo sigue una distribución normal con
  #media de 1000 horas y desviación estándar de 100 horas. Si tomamos una muestra aleatoria de 25
  #baterías, ¿cuál es la probabilidad de que la duración promedio de la muestra sea mayor de 1050
  #horas?
  xbarra1<- 100  
t <-0.105
s1 <- 25
n <- 1000
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

probabilidad1 <- xbarra - z* s; print(probabilidad1)
probabilidad2 <- xbarra + z* s; print(probabilidad2)


--------------------------------------------------
  #  5. Se desea estudiar la duración promedio de las películas de acción en cierto cine. Se toma una
  #muestra aleatoria de 12 películas y se obtiene una duración promedio de 2.1 horas, con una
  #desviación estándar de 0.25 horas. Si la duración promedio de las películas de acción es de 2 horas,
  #¿es significativo el resultado obtenido?
  xbarra1<- 2.1
t <-0.25
s1 <- 2
n <- 12        
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

promedio1 <- xbarra - z* s; print(promedio1)
promedio2 <- xbarra + z* s; print(promedio2)

print('La duracion de las peliculas es de 1 a 2 horas')
-----------------------------------------------------
  #  6. Se sabe que el diámetro de ciertos tornillos sigue una distribución normal con media de 10 mm y
  #desviación estándar de 0.5 mm. Si tomamos una muestra aleatoria de 15 tornillos, ¿cuál es la
  #probabilidad de que el diámetro promedio de la muestra esté entre 9.7 mm y 10.3 mm?
  xbarra1<- 0.5
t <-0.97
s1 <- 15
n <- 10
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

probabilidad1 <- xbarra - z* s; print(probabilidad1)
probabilidad2 <- xbarra + z* s; print(probabilidad2)



----------------------------------------------------------------
  #  7. Se desea estudiar la cantidad promedio de libros que los estudiantes universitarios compran por
  #semestre. Se toma una muestra aleatoria de 20 estudiantes y se obtiene una cantidad promedio de 4
  #libros, con una desviación estándar de 1.5 libros. Si la cantidad promedio de libros que los
  #estudiantes compran por semestre es de 3.5, ¿es significativo el resultado obtenido?
  xbarra1<- 4  
t <-3.5
s1 <- 1.5                           
n <- 20
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

probabilidad1 <- xbarra - z* s; print(probabilidad1)
probabilidad2 <- xbarra + z* s; print(probabilidad2)
print('Que solo el 3% de los 20 estudiantes compran libros por semestre')



------------------------------------------------------
  #  8. Se sabe que la altura de ciertas plantas sigue una distribución normal con media de 1.2 metros y
  #desviación estándar de 0.2 metros. Si tomamos una muestra aleatoria de 30 plantas, ¿cuál es la
  #probabilidad de que la altura promedio de la muestra sea mayor de 1.3 metros?
  xbarra1<- 1.2 
t <-1.3
s1 <- 0.2
n <- 30
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

probabilidad1 <- xbarra - z* s; print(probabilidad1)
probabilidad2 <- xbarra + z* s; print(probabilidad2)


------------------------------------------------------------------
  #  9. En una muestra de 25 estudiantes, se obtuvo una media de 7.5 en un examen de matemáticas con
  #una desviación estándar de 1.2. ¿Cuál es el intervalo de confianza del 95% para la media poblacional
  #de calificaciones?
  xbarra1<- 7.5 
t <-0.95
s1 <-1.2
n <- 25
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
.----------------------------------------------------------------
  #  10. Se sabe que el 90% de las mujeres adultas tienen una altura entre 1.5 y 1.8 metros. Si se
  #selecciona una muestra aleatoria de 50 mujeres, ¿cuál es la probabilidad de que su altura promedio
  #esté dentro de ese rango?
  xbarra1<- 1.5 
t <-0.90
s1 <- 1.8
n <- 50
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

probabilidad1 <- xbarra - z* s; print(probabilidad1)
probabilidad2 <- xbarra + z* s; print(probabilidad2)


------------------------------------------------------
  #  11. Se selecciona una muestra aleatoria de 30 observaciones de una población normalmente
  #distribuida. La media muestral es de 50 y la desviación estándar es de 8. ¿Cuál es la probabilidad de
  #que la media poblacional esté entre 46 y 54?
  xbarra1<- 50  
t <-0.95
s1 <- 8
n <- 30
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

probabilidad1 <- xbarra - z* s; print(probabilidad1)
probabilidad2 <- xbarra + z* s; print(probabilidad2)


-------------------------------------------------------
  #  12. En un estudio de mercado se entrevistó a una muestra aleatoria de 100 consumidores. Se
  #encontró que la media de gasto semanal en alimentos es de $\$80$ con una desviación estándar de
  # $\$15$. ¿Cuál es el intervalo de confianza del 99% para la media poblacional de gasto semanal en
  #alimentos?
  xbarra1<- 80  
t <-0.99
s1 <- 15
n <- 100
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
---------------------------------------------------------------
  #  13. Se desea estimar la media poblacional de la duración de una batería en horas. Se selecciona una
  #muestra aleatoria de 20 baterías y se obtiene una media de 30 horas con una desviación estándar de
  #5 horas. Si se sabe que la distribución de la duración de las baterías es normal, ¿cuál es el intervalo de
  #confianza del 90% para la media poblacional de duración?
  xbarra1<- 30 
t <-0.90
s1 <- 5
n <- 20
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
-------------------------------------------------------------
  #  14. Un fabricante de piezas de automóviles afirma que la vida media de sus piezas es de 50,000 km.
  #Para comprobarlo, se selecciona una muestra aleatoria de 25 piezas y se encuentra que la vida media
  #es de 48,000 km con una desviación estándar de 6,000 km. Si se asume que la distribución de la vida
  #de las piezas sigue una distribución normal, ¿hay suficiente evidencia para rechazar la afirmación del
  #fabricante al nivel de significancia del 5%?
  xbarra1<- 48000 
t <-0.5
s1 <- 6000
n <- 50000
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
----------------------------------------------------
  #  15. Se quiere determinar si hay diferencia entre las medias de dos poblaciones normales
  # independientes con varianzas desconocidas. Se selecciona una muestra aleatoria de 25 elementos de
  # cada población y se encuentra que la media muestral de la primera es de 5 y la de la segunda es de 6.
  # Además, las desviaciones estándar muestrales son de 1.5 y 2, respectivamente. Si se utiliza un nivel
  # de significancia del 1%, ¿se puede concluir que hay diferencia entre las medias poblacionales?
  xbarra1<- 5  
t <-0.1
s1 <- 6
n <- 25
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo) # primero
--------
  xbarra1<- 1.5  
t <-0.1
s1 <- 2
n <- 25
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo) # segundo
---------------------------------------------------------------------
  #  16. Se desea comprobar si la media poblacional de la edad de los clientes de una tienda de ropa es
  # mayor a 30 años. Se selecciona una muestra aleatoria de 40 clientes y se obtiene una media de 32
  # años y una desviación estándar de 4 años. Si se utiliza un nivel de significancia del 5%, ¿se puede
  # afirmar que la media poblacional es mayor a 30 años?
  xbarra1<- 30
t <-0.5
s1 <- 4
n <- 40
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)



