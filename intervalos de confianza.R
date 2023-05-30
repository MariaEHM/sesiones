# probabilidad  ejercicios  de las notas

#intervalos de confianza para muestras grandes

#ingeniería en sistemas electrónicos
#1. Un fabricante de resistencias electrónicas toma una muestra aleatoria de 100 resistencias de su línea de
#producción y encuentra que la media muestral de resistencia es de 500 ohms, con una desviación estándar
##muestral de 10 ohms. ¿Cuál es el intervalo de confianza del 95 % para la resistencia media poblacional? Tamaño
#de la muestra (n): 100 Media muestral (x): 500 ohms Desviación estándar muestral (s): 10 ohms Nivel de
#confianza: 95 % Valor crítico z de la distribución normal estándar para un nivel de confianza del 95 %: 1.96

xbarra1<-  500
z <-0.95
alpha1 <- 10
n <- 100
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
----------------------------------------------------------------------------------------------------
  #2. Un equipo de investigación quiere estimar la cantidad media de RAM en las computadoras portátiles producidas
  #por una empresa. Toman una muestra aleatoria de 200 computadoras y encuentran que la media muestral de
  # RAM es de 8 GB, con una desviación estándar muestral de 1 GB. ¿Cuál es el intervalo de confianza del 99 %
  #para la cantidad media de RAM en las computadoras portátiles producidas por la empresa?
  xbarra1<-  8 
z <-0.99
alpha1 <- 1
n <- 200
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
----------------------------------------------------------------------------------------
  #    3. En un estudio de calidad de señal en sistemas de comunicaciones, se mide la relación señal-ruido en 50 muestras
  # de un dispositivo. Se obtiene una media muestral de 25 dB y una desviación estándar muestral de 2 dB.
  #Construir un intervalo de confianza del 95 % para la verdadera relación señal-ruido media del dispositivo.
  xbarra1<-  25  -----#media muestral
  z <-0.95              #distribucion normal estandar
alpha1 <-  2          # desviación  estandar
n <-  50               #tamañ0 de la muestra
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
---------------------------------------------------------------
  #    4. Se desea evaluar el rendimiento de un nuevo algoritmo de compresión de datos en tiempo real. Se toman 100
  # mediciones del rendimiento y se obtiene una media muestral de 300 Mbps y una desviación estándar muestral
  # de 20 Mbps. Construir un intervalo de confianza del 99 % para la verdadera tasa de compresión media.
  xbarra1<- 300  
z <- 0.99
alpha1 <- 20
n <- 100
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
----------------------------------------------------------
  #   5. En un estudio de fiabilidad de componentes electrónicos, se mide el tiempo de vida de 80 muestras de un
  #dispositivo. Se obtiene una media muestral de 5000 horas y una desviación estándar muestral de 800 horas.
  #Construir un intervalo de confianza del 90 % para la verdadera vida media del dispositivo.
  #mediciones de tiempos de respuesta y se obtiene una media muestral de 10 ms y una desviación estándar
  #muestral de 1.5 ms. Construir un intervalo de confianza del 95 % para la verdadera velocidad de respuesta
  #media del sistema.
  xbarra1<-  5000
z <- 0.90
alpha1 <-800
n <- 80
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
----------------------------------------------------------------------------------
  #    7. En un estudio de calidad de señal en sistemas de radar, se mide la relación señal-ruido en 150 muestras de un
  #dispositivo. Se obtiene una media muestral de 30 dB y una desviación estándar muestral de 3 dB. Construir
  #un intervalo de confianza del 98 % para la verdadera relación señal-ruido media del dispositivo.
  #ingenieria en software
  xbarra1<- 30
z <- 0.98
alpha1 <-3
n <-  150
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
------------------------------    
  # 1. Se desea evaluar el rendimiento de un algoritmo de búsqueda en una base de datos. Se toman 500 mediciones
  # del tiempo de búsqueda y se obtiene una media muestral de 3.5 segundos y una desviación estándar muestral
  # de 0.5 segundos. Construir un intervalo de confianza del 95 % para la verdadera velocidad de búsqueda media.
  xbarra1<- 3.5
z <- 0.95
alpha1 <- 0.5
n <- 500
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
-------------------------------------
  #    2. En un estudio de usabilidad de una aplicación móvil, se mide el tiempo que tardan los usuarios en realizar una
  # tarea específica en la aplicación. Se toman 200 mediciones y se obtiene una media muestral de 45 segundos
  # y una desviación estándar muestral de 10 segundos. Construir un intervalo de confianza del 99 % para la
  # verdadera duración media de la tarea.
  xbarra1<-  45 
z <- 0.99
alpha1 <- 10
n <-  200
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
---------------------------------------------
  #   3. Se desea evaluar la eficacia de un nuevo algoritmo de clasificación de datos. Se toman 1000 mediciones de la
  # precisión de clasificación y se obtiene una media muestral del 85 % y una desviación estándar muestral del 5 %.
  # Construir un intervalo de confianza del 90 % para la verdadera precisión media de clasificación.
  xbarra1<-  0.85
z <- 0.90
alpha1 <-0.5
n <- 1000
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
-------------------------------------------
  # 4. En un estudio de rendimiento de un sistema de procesamiento de imágenes, se mide la velocidad de procesamiento
  # en milisegundos. Se toman 300 mediciones y se obtiene una media muestral de 25 ms y una desviación estándar
  # muestral de 3 ms. Construir un intervalo de confianza del 99 % para la verdadera velocidad media de
  # procesamiento.
  xbarra1<-  25
z <- 0.99
alpha1 <- 3
n <- 300
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
--------------------------------------------
  # 5. Se desea evaluar la eficacia de un sistema de recomendación de productos en línea. Se toman 500 mediciones
  # de la satisfacción de los usuarios y se obtiene una media muestral de 4.2 estrellas y una desviación estándar
  # muestral de 0.5 estrellas. Construir un intervalo de confianza del 95 % para la verdadera satisfacción media de
  # los usuarios.
  # ingenieria en sistemas energeticos
  xbarra1<-  4.2 
z <- 0.95
alpha1 <- 0.5
n <- 500
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
----------------------------
  # 1. Se desea evaluar la eficacia de un sistema de energía solar para generar electricidad en una región específica.
  # Se toman 1000 mediciones de la producción de energía diaria y se obtiene una media muestral de 10 kWh y
  # una desviación estándar muestral de 2 kWh. Construir un intervalo de confianza del 95 % para la verdadera
  #producción media de energía diaria.
  xbarra1<-  10 
z <- 0.95
alpha1 <- 2
n <-  1000
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
-----------------------------
  #    2. En un estudio de la eficacia de un nuevo sistema de enfriamiento para una central eléctrica, se mide la
  # temperatura de salida del aire. Se toman 500 mediciones y se obtiene una media muestral de 20 grados Celsius
  # y una desviación estándar muestral de 3 grados Celsius. Construir un intervalo de confianza del 99 % para la
  # verdadera temperatura media de salida del aire.
  xbarra1<- 20  
z <-0.99
alpha1 <- 3
n <- 500
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
------------------------
  #   3. Se desea evaluar la eficacia de un nuevo sistema de control de la temperatura en una planta de producción de
  # energía. Se toman 300 mediciones de la temperatura y se obtiene una media muestral de 50 grados Celsius y
  # una desviación estándar muestral de 5 grados Celsius. Construir un intervalo de confianza del 90 % para la
  # verdadera temperatura media.
  xbarra1<- 50
z <- 0.90
alpha1 <- 5
n <-  300
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
-----------------------------------
  #    4. En un estudio de la eficacia de un nuevo sistema de almacenamiento de energía, se mide la cantidad de energía
  # almacenada en kWh. Se toman 200 mediciones y se obtiene una media muestral de 100 kWh y una desviación
  # estándar muestral de 10 kWh. Construir un intervalo de confianza del 99 % para la verdadera cantidad media
  # de energía almacenada.
  xbarra1<-  100 
z <- 0.99
alpha1 <- 10
n <-  200
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
----------------------------------
  #    5. Se desea evaluar la eficacia de un nuevo sistema de generación de energía hidroeléctrica en una represa. Se
  # toman 400 mediciones de la producción de energía y se obtiene una media muestral de 50 MW y una desviación
  # estándar muestral de 6 MW. Construir un intervalo de confianza del 95 % para la verdadera producción media
  # de energía.
  #proteccion civil
  xbarra1<-  50
z <- 0-95
alpha1 <- 6
n <- 400
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
----------------------------------
  # 1. Se desea estimar el tiempo medio que tarda una brigada de Protección Civil en llegar al lugar de una emergencia.
  # Se selecciona una muestra aleatoria de 100 emergencias y se obtiene un tiempo medio de 15 minutos con una
  # desviación estándar de 3 minutos. Se desea construir un intervalo de confianza del 95 % para la media del
  # tiempo de llegada de las brigadas.
  xbarra1<- 15 
z <- 0.95
alpha1 <- 3
n <- 100
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
---------------------------
  # 2. Se quiere determinar la concentración media de un contaminante en el aire en una zona de Protección Civil.
  # Se toma una muestra aleatoria de 50 mediciones y se obtiene una concentración media de 0.5 ppm con una
  # desviación estándar de 0.1 ppm. Se desea construir un intervalo de confianza del 90 % para la media de la
  # concentración de contaminante en el aire.
  xbarra1<-  0.5
z <- 0.90
alpha1 <- 0.1
n <-  50
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
--------------------------------
  # 3. Se desea determinar la media de la cantidad de material de rescate utilizado en las operaciones de Protección
  # 3 Civil. Se toma una muestra aleatoria de 200 operaciones y se obtiene una media de 500 kg con una desviación
  # estándar de 50 kg. Se desea construir un intervalo de confianza del 99 % para la media de la cantidad de
  #material de rescate utilizado en las operaciones.
  xbarra1<-  500 
z <- 0.99
alpha1 <- 50
n <-  500
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
------------------------------------------- 
  #    4. Se desea estimar la media del tiempo que tarda en ser restablecido el servicio eléctrico en una zona afectada
  # por un desastre natural. Se selecciona una muestra aleatoria de 150 casos y se obtiene un tiempo medio de
  # restablecimiento de 6 horas con una desviación estándar de 1.5 horas. Se desea construir un intervalo de
  # confianza del 95 % para la media del tiempo de restablecimiento del servicio eléctrico.
  xbarra1<- 6  
z <- 0.95
alpha1 <- 1.5
n <- 150
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
----------------------------------------
  # 5. Se quiere estimar la media del número de llamadas de emergencia recibidas en un centro de atención de
  # Protección Civil durante los fines de semana. Se toma una muestra aleatoria de 300 fines de semana y se obtiene
  # un número medio de llamadas de 200 con una desviación estándar de 30. Se desea construir un intervalo de
  # confianza del 98 % para la media del número de llamadas de emergencia recibidas durante los fines de semana.
  # ingenieria
  xbarra1<-  200 
z <- 0.98
alpha1 <- 30
n <-  300
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
------------------------------------------------
  # 1. Un fabricante de circuitos electrónicos afirma que la vida útil media de una batería de su producción es de al
  # menos 500 horas. Si se toma una muestra aleatoria de 100 baterías y se encuentra una vida útil media de 510
  # horas con una desviación estándar de 20 horas, ¿qué se puede concluir con un nivel de confianza del 95 %?
  xbarra1<-  510
z <- 0.95
alpha1 <- 20
n <- 100
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
----------------------------------------------------------
  #  2. Un ingeniero quiere estimar la resistencia promedio de una muestra de 60 vigas de acero. Los datos indican
  # una media de 20,5 kN/m2 y una desviación estándar de 1,8 kN/m2. Si se desea tener un nivel de confianza del
  # 99 %, ¿cuál es el intervalo de confianza para la resistencia media de las vigas?
  xbarra1<- 20.5  
z <-0.99
alpha1 <-1.8
n <- 60
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
------------------------------------------
  #    3. Se está llevando a cabo una investigación sobre la cantidad de agua que un motor de combustión interna puede
  # absorber antes de que falle. Se toma una muestra de 80 motores y se encuentra una media de absorción de 80
  # ml con una desviación estándar de 10 ml. Si se desea estimar la cantidad media de absorción con un nivel de
  #confianza del 90 %, ¿cuál es el intervalo de confianza?
  xbarra1<-  80
z <- 0.90
alpha1 <- 10
n <-  80
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
----------------------------------------------
  #    4. Un fabricante de turbinas de viento desea probar su nueva turbina en condiciones extremas de viento. Para
  # ello, se mide la potencia generada por la turbina en una muestra de 50 pruebas. La media de la potencia
  # generada es de 1200 kW con una desviación estándar de 100 kW. ¿Cuál es el intervalo de confianza del 95 %
  # para la potencia media de la turbina?
  xbarra1<- 1200  
z <- 0.95
alpha1 <- 100
n <-  50
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
----------------------------------------------------
  #      5. Se está llevando a cabo una investigación para determinar la vida útil media de un nuevo material utilizado en
  # la construcción de edificios. Se toma una muestra de 200 piezas del material y se encuentra que la vida útil
  # media es de 20 años con una desviación estándar de 2 años. Si se desea tener un nivel de confianza del 99 %,
  # ¿cuál es el intervalo de confianza para la vida útil media del material?
  xbarra1<-  20
z <- 0.99
alpha1 <- 2
n <-  200
xbarra <-  xbarra1 - z
alpha <- alpha1 /sqrt(n)

LimInf <- xbarra - z* alpha; print(LimInf)
LimSup <- xbarra + z* alpha; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
--------------------------------------------------------------------------------------------------------
  #Muestras pequeñas
  # 1. Intervalo de confianza del 95 % para la altura media poblacional de los estudiantes de una universidad con una
  # muestra de 10 estudiantes:
  #Tamaño de la muestra (n): 10
  #Media muestral (x): 170 cm
  #Desviación estándar muestral (s): 8 cm
  xbarra1<- 170  
t <-0.95
s1 <- 8
n <- 10
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
--------------------------------------------------
  
  # 2. Intervalo de confianza del 90 % para el peso medio poblacional de los recién nacidos con una muestra de 15
  # bebés
  # Tamaño de la muestra (n): 15
  # Media muestral (x): 3.5 kg
  # Desviación estándar muestral (s): 0.5 kg
  xbarra1<- 3.5  
t <-0.90
s1 <- 0.5
n <- 15
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
------------------------------------------
  #3. Intervalo de confianza del 99 para la duración media de las baterías de un dispositivo electrónico con una
  # muestra de $84 dispositivos:
  #  Tamaño de la muestra (n): 8
  #Media muestral (x): 30 horas
  #Desviación estándar muestral (s): 5 horas
  xbarra1<- 30  
t <-0.99
s1 <- 5
n <- 8
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
---------------------------------------------
  #4. Intervalo de confianza del 95 % para el número medio de días de hospitalización de pacientes con una muestra
  #de 12 pacientes: Tamaño de la muestra (n): 12 Media muestral (x): 5 días Desviación estándar muestral (s): 2
  #días
  xbarra1<- 5  
t <-0.95
s1 <- 2
n <- 12
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
-----------------------------------------------
  #5. Intervalo de confianza del 99 % para la media poblacional de la concentración de un compuesto en una muestra
  #de agua con una muestra de 7 mediciones: Tamaño de la muestra (n): 7 Media muestral (x): 4 mg/L Desviación
  #estándar muestral (s): 1 mg/L
  xbarra1<- 4
t <-0.99
s1 <- 1
n <- 7
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
----------------------------------------
  #6. Intervalo de confianza del 95 % para la altura media de una muestra de 8 plantas: Tamaño de la muestra (n): 8
  #Media muestral (x): 12 cm Desviación estándar muestral (s): 2 cm Nivel de confianza: 95 %
  xbarra1<- 12  
t <-0.95
s1 <-2
n <- 8
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
------------------------------------
  #7. Intervalo de confianza del 90 % para el tiempo medio de reacción de un grupo de 6 sujetos: Tamaño de la
  #muestra (n): 6 Media muestral (x): 0.3 segundos Desviación estándar muestral (s): 0.05 segundos
  xbarra1<- 0.3
t <-0.90
s1 <- 0.05
n <- 6
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
--------------------------------------------------------------
  #8. Intervalo de confianza del 99 % para la resistencia media de un lote de 10 piezas de acero: Tamaño de la
  #muestra (n): 10 Media muestral (x): 750 MPa Desviación estándar muestral (s): 25 MPa
  xbarra1<- 750  
t <-0.95
s1 <- 25
n <- 10
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
-------------------------------
  #9. Intervalo de confianza del 95 % para la concentración media de un compuesto en una muestra de 12 aguas
  #subterráneas: Tamaño de la muestra (n): 12 Media muestral (x): 5 mg/L Desviación estándar muestral (s): 1.5
  #mg/L
  xbarra1<- 5  
t <-0.95
s1 <- 1.5
n <- 12
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
-------------------------------------------------
  # 10. Intervalo de confianza del 95 % para la eficiencia media de una muestra de 20 paneles solares: Tamaño de la
  # muestra (n): 20 Media muestral (x): 80 % Desviación estándar muestral (s): 4 %
  xbarra1<- 0.80 
t <-0.95
s1 <- 4
n <- 20
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
----------------------------------------
  # 11. Intervalo de confianza del 90 % para la concentración media de un compuesto en una muestra de 15 suelos:
  #  Tamaño de la muestra (n): 15 Media muestral (x): 0.3 mg/g Desviación estándar muestral (s): 0.05 mg/g Nivel
  # de confianza: 90 %
  xbarra1<- 0.3  
t <-0.90
s1 <- 0.05
n <- 15
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)

-----------------------------------------
  #12. concentración media de un producto químico en una muestra de 25 aguas subterráneas: Tamaño de la muestra
  # (n): 25 Media muestral (x): 10.2 mg/L Desviación estándar muestral (s): 2.3 mg/L
  xbarra1<- 10.2 
t <-0.90
s1 <- 2.3
n <- 25
xbarra <-  xbarra1 - z
s <- s1 /sqrt(n)

LimInf <- xbarra - z* s; print(LimInf)
LimSup <- xbarra + z* s; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
-----------------------------------------------------------------------------------------------------------
  #Varianzas conocidas y distintas
  # 1. Se desea conocer si el tiempo promedio de atención en dos cajeros automáticos diferentes es significativamente
  # diferente. Se toman muestras aleatorias de 50 transacciones en cada cajero y se obtienen medias muestrales
  # de 3.5 y 4.2 minutos, respectivamente. La desviación estándar poblacional es conocida y es de 0.8 minutos.
  # Construir un intervalo de confianza del 95 % para la diferencia de las medias poblacionales.
  n1 <- 50
n2 <- 50
dbarra1 <- 3.5
dbarra2 <- 4.2
alpha1 <- 0.8
alpha2 <- 0.8
alpha0 <-0.8
z1 <-0.95
dbarra <- dbarra1-dbarra2
z <- z1 ^alpha0/2
Al <- alpha1/ n1 + alpha2/ n2


LimInf <- dbarra - z*(sqrt(Al)); 
LimSup <- dbarra + z*(Al); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)

---------------------------------------------------------------------------
  # 2. Se quiere determinar si hay una diferencia significativa en la altura promedio de dos poblaciones de estudiantes
  # universitarios. Se toman muestras aleatorias de 100 estudiantes de cada población y se obtienen medias
  # muestrales de 1.72 y 1.68 metros, respectivamente. La desviación estándar poblacional es conocida y es de 0.07
  # metros. Construir un intervalo de confianza del 99 % para la diferencia de las medias poblacionales.
  n1 <- 100
n2 <- 100
dbarra1 <- 1.72
dbarra2 <- 1.68
alpha1 <- 0.07
alpha2 <- 0.07
alpha0 <-0.07
z1 <-0.99
dbarra <- dbarra1-dbarra2
z <- z1 ^alpha0/2
Al <- alpha1/ n1 + alpha2/ n2


LimInf <- dbarra - z*(sqrt(Al)); 
LimSup <- dbarra + z*(Al); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)

--------------------------------------------------------------------------
  # 3. Se quiere comparar la productividad de dos plantas de producción de una empresa. Se toman muestras aleatorias
  # de 200 trabajadores de cada planta y se obtienen medias muestrales de 150 y 140 unidades producidas por
  # hora, respectivamente. La desviación estándar poblacional es conocida y es de 12 unidades por hora. Construir
  # un intervalo de confianza del 90 % para la diferencia de las medias poblacionales.
  n1 <- 200
n2 <- 200
dbarra1 <- 150
dbarra2 <- 150
alpha1 <- 12
alpha2 <- 12
alpha0 <-12
z1 <-0.90
dbarra <- dbarra1-dbarra2
z <- z1 ^alpha0/2
Al <- alpha1/ n1 + alpha2/ n2


LimInf <- dbarra - z*(sqrt(Al)); 
LimSup <- dbarra + z*(Al); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)

-------------------------------------------------------------------------------
  # 4. Un fabricante de focos quiere comparar dos marcas diferentes en términos de vida útil. Toma una muestra de
  # 100 focos de la marca A, con una vida media de 1200 horas y una desviación estándar de 200 horas, y una
  # muestra de 120 focos de la marca B, con una vida media de 1350 horas y una desviación estándar de 180 horas.
  #Construir un intervalo de confianza del 95 % para la diferencia media de vida útil entre las dos marcas.
  n1 <- 100
n2 <- 120
dbarra1 <- 1200
dbarra2 <- 1350
alpha1 <- 200
alpha2 <- 180
alpha0 <-380
z1 <-0.95
dbarra <- dbarra1-dbarra2
z <- z1 ^alpha0/2
Al <- alpha1/ n1 + alpha2/ n2


LimInf <- dbarra - z*(sqrt(Al)); 
LimSup <- dbarra + z*(Al); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
-----------------------------------------------------------------
  # 5. Un estudio quiere comparar la efectividad de dos tipos de medicamentos en el tratamiento de la hipertensión.
  # Se toma una muestra de 80 pacientes tratados con el medicamento A, con una presión arterial media de 130
  # mmHg y una desviación estándar de 10 mmHg, y una muestra de 90 pacientes tratados con el medicamento B,
  # con una presión arterial media de 125 mmHg y una desviación estándar de 12 mmHg. Construir un intervalo
  # de confianza del 99 % para la diferencia media en la presión arterial entre los dos medicamentos.
  n1 <- 80
n2 <- 90
dbarra1 <- 130
dbarra2 <- 125
alpha1 <- 10
alpha2 <- 12
alpha0 <-22
z1 <-0.99
dbarra <- dbarra1-dbarra2
z <- z1 ^alpha0/2
Al <- alpha1/ n1 + alpha2/ n2


LimInf <- dbarra - z*(sqrt(Al)); 
LimSup <- dbarra + z*(Al); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
----------------------------
  # 6. Se quiere comparar dos programas de entrenamiento en términos de su efectividad en el aumento de la fuerza
  # muscular. Se toma una muestra de 60 atletas que han seguido el programa A, con una fuerza media de 150
  # kg y una desviación estándar de 20 kg, y una muestra de 70 atletas que han seguido el programa B, con una
  # fuerza media de 155 kg y una desviación estándar de 18 kg. Construir un intervalo de confianza del 90 % para
  # la diferencia media en la fuerza muscular entre los dos programas.
  n1 <- 50
n2 <- 50
dbarra1 <- 3.5
dbarra2 <- 4.2
alpha1 <- 0.8
alpha2 <- 0.8
alpha0 <-0.8
z1 <-0.95
dbarra <- dbarra1-dbarra2
z <- z1 ^alpha0/2
Al <- alpha1/ n1 + alpha2/ n2


LimInf <- dbarra - z*(sqrt(Al)); 
LimSup <- dbarra + z*(Al); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
-------------------------------------
  #  7. Un estudio quiere comparar dos tipos de fertilizantes en términos de su efectividad en el crecimiento de plantas
  # de maíz. Se toma una muestra de 200 plantas tratadas con el fertilizante A, con una altura media de 1.8 metros
  # y una desviación estándar de 0.2 metros, y una muestra de 220 plantas tratadas con el fertilizante B, con una
  # altura media de 2.0 metros y una desviación estándar de 0.18 metros. Construir un intervalo de confianza del
  # 95 % para la diferencia media en la altura de las plantas entre los dos fertilizantes.
  n1 <- 200
n2 <- 220
dbarra1 <- 1.8
dbarra2 <- 2-0
alpha1 <- 0.2
alpha2 <- 0.18
alpha0 <-0.20
z1 <-0.95
dbarra <- dbarra1-dbarra2
z <- z1 ^alpha0/2
Al <- alpha1/ n1 + alpha2/ n2


LimInf <- dbarra - z*(sqrt(Al)); 
LimSup <- dbarra + z*(Al); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
-----------------------------------
  # 8. Se quiere comparar el rendimiento de dos tipos de discos duros en términos de su velocidad de lectura y
  # escritura. Se toma una muestra de 150 discos duros del tipo A, con una velocidad media de lectura/escritura
  # de 100 MB/s y una desviación estándar de 10 MB/s, y una muestra de 180 discos duros del tipo B, con
  # una velocidad media de lectura/escritura de 120 MB/s y una desviación estándar de 12 MB/s. Construir un
  # intervalo de confianza del 99 % para la diferencia media en la velocidad de lectura/escritura entre los dos tipos
  # de discos duros.
  n1 <- 150
n2 <- 180
dbarra1 <- 100
dbarra2 <- 120
alpha1 <- 10
alpha2 <- 12
alpha0 <-22
z1 <-0.99
dbarra <- dbarra1-dbarra2
z <- z1 ^alpha0/2
Al <- alpha1/ n1 + alpha2/ n2


LimInf <- dbarra - z*(sqrt(Al)); 
LimSup <- dbarra + z*(Al); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
-----------------------------------
  # 9. Un investigador desea comparar la efectividad de dos métodos de enseñanza de matemáticas en una muestra
  # de 60 estudiantes. El primer grupo de 30 estudiantes recibió el método A y el segundo grupo de 30 estudiantes
  # recibió el método B. La media y la desviación estándar de las calificaciones de los estudiantes en el grupo A
  # fueron de 75 y 8, respectivamente, mientras que en el grupo B fueron de 80 y 10, respectivamente. Construir
  # un intervalo de confianza del 95 % para la diferencia media entre las calificaciones de los dos grupos.
  n1 <- 30
n2 <- 30
dbarra1 <- 75
dbarra2 <- 80
alpha1 <- 8
alpha2 <- 10
alpha0 <- 18
z1 <-0.95
dbarra <- dbarra1-dbarra2
z <- z1 ^alpha0/2
Al <- alpha1/ n1 + alpha2/ n2


LimInf <- dbarra - z*(sqrt(Al)); 
LimSup <- dbarra + z*(Al); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
----------------------------------------------------------------------------------------------------
  #10. Se desea comparar la efectividad de dos medicamentos para reducir la presión arterial en pacientes con
  #hipertensión. Un grupo de 25 pacientes tomó el medicamento A y otro grupo de 30 pacientes tomó el
  #medicamento B. La media y la desviación estándar de la presión arterial de los pacientes en el grupo A fueron
  #de 140 y 10, respectivamente, mientras que en el grupo B fueron de 135 y 12, respectivamente. Construir un
  #intervalo de confianza del 90 % para la diferencia media entre las presiones arteriales de los dos grupos.
  n1 <- 25
n2 <- 30
dbarra1 <- 140
dbarra2 <- 135
alpha1 <- 10
alpha2 <- 12
alpha0 <-22
z1 <-0.90
dbarra <- dbarra1-dbarra2
z <- z1 ^alpha0/2
Al <- alpha1/ n1 + alpha2/ n2


LimInf <- dbarra - z*(sqrt(Al)); 
LimSup <- dbarra + z*(Al); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo) 

--------------------------------------------------------------------------------------------------
  # 11. Se desea comparar la duración de dos tipos de baterías para un teléfono móvil. Se seleccionó una muestra
  # aleatoria de 50 baterías de cada tipo. La media y la desviación estándar de la duración de las baterías del tipo
  # A fueron de 20 horas y 3 horas, respectivamente, mientras que las del tipo B fueron de 22 horas y 4 horas,
  # respectivamente. Construir un intervalo de confianza del 99 % para la diferencia media entre las duraciones de
  # las baterías de los dos tipos.
  n1 <- 50
n2 <- 50
dbarra1 <- 20
dbarra2 <- 22
alpha1 <- 3
alpha2 <- 4
alpha0 <-7
z1 <-0.99
dbarra <- dbarra1-dbarra2
z <- z1 ^alpha0/2
Al <- alpha1/ n1 + alpha2/ n2


LimInf <- dbarra - z*(sqrt(Al)); 
LimSup <- dbarra + z*(Al); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
---------------------------------------
  # 12. Se desea comparar el tiempo que tardan dos marcas de coches en recorrer una distancia de 100 km. Se seleccionó
  # una muestra aleatoria de 20 coches de cada marca. La media y la desviación estándar del tiempo que tardaron
  # los coches de la marca A fueron de 1.5 horas y 0.2 horas, respectivamente, mientras que los de la marca B
  # tardaron 1.3 horas y 0.3 horas, respectivamente. Construir un intervalo de confianza del 95 % para la diferencia
  # media entre los tiempos que tardan los coches de las dos marcas.
  n1 <- 20
n2 <- 20
dbarra1 <- 1.5
dbarra2 <- 1.3
alpha1 <- 0.2
alpha2 <- 0.3
alpha0 <-0.5
z1 <-0.95
dbarra <- dbarra1-dbarra2
z <- z1 ^alpha0/2
Al <- alpha1/ n1 + alpha2/ n2


LimInf <- dbarra - z*(sqrt(Al)); 
LimSup <- dbarra + z*(Al); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
------------------------------------
  # 13. Se desea comparar el rendimiento de dos tipos de fertilizantes en el crecimiento de las plantas de maíz. Se
  # seleccionó una muestra aleatoria de 40 plantas de cada tipo de fertilizante. La media y la desviación estándar
  # del crecimiento de las plantas con el fertilizante A fueron de 150 cm y 20 cm, respectivamente, mientras que
  # las del fertilizante B fueron de 160 cm y 18 cm, respectivamente. Construir un intervalo de confianza del 99 %
  # para la diferencia media entre los crecimientos de las plantas con los dos tipos de fertilizante.
  n1 <- 40
n2 <- 40
dbarra1 <- 150
dbarra2 <- 160
alpha1 <- 20
alpha2 <- 18
alpha0 <-38
z1 <-0.99
dbarra <- dbarra1-dbarra2
z <- z1 ^alpha0/2
Al <- alpha1/ n1 + alpha2/ n2


LimInf <- dbarra - z*(sqrt(Al)); 
LimSup <- dbarra + z*(Al); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
----------------------------
  # 14. Se quiere comparar el tiempo promedio de entrega de dos proveedores de paquetes. Se toma una muestra de
  # 25 paquetes del primer proveedor, con un tiempo promedio de entrega de 3.5 días y una desviación estándar
  # muestral de 1.2 días, y una muestra de 30 paquetes del segundo proveedor, con un tiempo promedio de entrega
  # de 4.2 días y una desviación estándar muestral de 1.5 días. Construya un intervalo de confianza del 95 % para
  # la diferencia de los tiempos promedio de entrega.
  n1 <- 25
n2 <- 30
dbarra1 <- 3.5
dbarra2 <- 4.2
alpha1 <- 1.2
alpha2 <- 1.5
alpha0 <-2.7
z1 <-0.95
dbarra <- dbarra1-dbarra2
z <- z1 ^alpha0/2
Al <- alpha1/ n1 + alpha2/ n2


LimInf <- dbarra - z*(sqrt(Al)); 
LimSup <- dbarra + z*(Al); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
-------------------------------
  # 15. Se quiere comparar el rendimiento promedio de dos grupos de estudiantes en un examen de matemáticas.
  # Se toma una muestra de 10 estudiantes del primer grupo, con un rendimiento promedio de 75 puntos y
  # una desviación estándar muestral de 8 puntos, y una muestra de 12 estudiantes del segundo grupo, con un
  # rendimiento promedio de 68 puntos y una desviación estándar muestral de 10 puntos. Se asume que las
  # varianzas de ambos grupos son iguales. Construya un intervalo de confianza del 90 % para la diferencia de los
  # rendimientos promedio.
  n1 <- 10
n2 <- 12
dbarra1 <- 75
dbarra2 <- 68
alpha1 <- 8
alpha2 <- 10
alpha0 <-0.8
z1 <-0.90
dbarra <- dbarra1-dbarra2
z <- z1 ^alpha0/2
Al <- alpha1/ n1 + alpha2/ n2


LimInf <- dbarra - z*(sqrt(Al)); 
LimSup <- dbarra + z*(Al); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
----------------------------------------------------------------------------------------------------------
  #2.2.2. Varianzas conocidas e iguales
  
  #  1. Un estudio compara los niveles de estrés entre trabajadores de dos empresas diferentes. Se toman muestras de
  # 50 trabajadores de cada empresa. La media de estrés en la empresa 1 es de 6.2 con una desviación estándar de
  # 1.1, mientras que la media de estrés en la empresa 2 es de 5.8 con una desviación estándar de 1.3. Construir un
  # intervalo de confianza al 95 % para la diferencia de medias entre ambas empresas.
  DesvEst <- 0.8
n1 <- 50
n2 <- 50
xbarra1 <- 3.5
xbarra2 <- 4.2             
xbarra <- xbarra1-xbarra2
alpha <- 90; Zalpha1 <- 1.64
alpha <- 95; Zalpha2 <- 1.96
alpha <- 99; Zalpha3 <- 2.57

nc <- Zalpha2
ErrorStd <- DesvEst*sqrt((n1+n2)/(n1*n2))
ErrorStd <- DesvEst*sqrt((1/n1)+(1/n2))

LimInf <- xbarra - nc*ErrorStd; print(LimInf)
LimSup <- xbarra + nc*ErrorStd; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
-----------------------------------------------------------
  # 2. Se quiere comparar la eficacia de dos medicamentos para tratar la hipertensión arterial. Se toma una muestra
  # de 100 pacientes para cada medicamento. La media de presión arterial en el grupo que recibe el medicamento 1
  # es de 145 mmHg con una desviación estándar de 5 mmHg, mientras que la media de presión arterial en el
  # grupo que recibe el medicamento 2 es de 140 mmHg con una desviación estándar de 6 mmHg. Construir un
  # intervalo de confianza al 99 % para la diferencia de medias entre ambos medicamentos
  DesvEst <- 0.8
n1 <- 50
n2 <- 50
xbarra1 <- 3.5
xbarra2 <- 4.2             
xbarra <- xbarra1-xbarra2
alpha <- 90; Zalpha1 <- 1.64
alpha <- 95; Zalpha2 <- 1.96
alpha <- 99; Zalpha3 <- 2.57

nc <- Zalpha2
ErrorStd <- DesvEst*sqrt((n1+n2)/(n1*n2))
ErrorStd <- DesvEst*sqrt((1/n1)+(1/n2))

LimInf <- xbarra - nc*ErrorStd; print(LimInf)
LimSup <- xbarra + nc*ErrorStd; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
-------------------------------------------------------------------------------------.

# 3. Se desea comparar el tiempo de respuesta de dos proveedores de internet. Se toman muestras de 30 usuarios
# para cada proveedor. La media de tiempo de respuesta del proveedor 1 es de 20 milisegundos con una desviación
# estándar de 3 milisegundos, mientras que la media de tiempo de respuesta del proveedor 2 es de 25 milisegundos
# con una desviación estándar de 4 milisegundos. Construir un intervalo de confianza al 90 % para la diferencia
# de medias entre ambos proveedores.
DesvEst <- 0.8
n1 <- 50
n2 <- 50
xbarra1 <- 3.5
xbarra2 <- 4.2             
xbarra <- xbarra1-xbarra2
alpha <- 90; Zalpha1 <- 1.64
alpha <- 95; Zalpha2 <- 1.96
alpha <- 99; Zalpha3 <- 2.57

nc <- Zalpha2
ErrorStd <- DesvEst*sqrt((n1+n2)/(n1*n2))
ErrorStd <- DesvEst*sqrt((1/n1)+(1/n2))

LimInf <- xbarra - nc*ErrorStd; print(LimInf)
LimSup <- xbarra + nc*ErrorStd; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
----------------------------------------------------------------------------------
  # 4. Un estudio compara los precios de productos en dos supermercados diferentes. Se toman muestras de 50
  # productos de cada supermercado. La media de precio en el supermercado 1 es de $5,2 con una desviación
  # estándar de $1,1, mientras que la media de precio en el supermercado 2 es de $4,8 con una desviación estándar
  # de $1,3. Construir un intervalo de confianza al 95 % para la diferencia de medias entre ambos supermercados.
  DesvEst <- 0.8
n1 <- 50
n2 <- 50
xbarra1 <- 3.5
xbarra2 <- 4.2             
xbarra <- xbarra1-xbarra2
alpha <- 90; Zalpha1 <- 1.64
alpha <- 95; Zalpha2 <- 1.96
alpha <- 99; Zalpha3 <- 2.57

nc <- Zalpha2
ErrorStd <- DesvEst*sqrt((n1+n2)/(n1*n2))
ErrorStd <- DesvEst*sqrt((1/n1)+(1/n2))

LimInf <- xbarra - nc*ErrorStd; print(LimInf)
LimSup <- xbarra + nc*ErrorStd; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
---------------------------------------------------------------------
  # 5. Se quiere comparar la efectividad de dos programas de entrenamiento para mejorar la flexibilidad. Se toman
  # muestras de 40 participantes para cada programa. La media de mejora en la flexibilidad en el programa 1 es
  # de 15 grados con una desviación estándar de 2 grados, mientras que la media de mejora en la flexibilidad en el
  # programa 2 es de 18 grados con una desviación estándar de 3 grados. Construir un intervalo de confianza al
  # 99 % para la diferencia de medias entre ambos programas.
  DesvEst <- 0.8
n1 <- 50
n2 <- 50
xbarra1 <- 3.5
xbarra2 <- 4.2             
xbarra <- xbarra1-xbarra2
alpha <- 90; Zalpha1 <- 1.64
alpha <- 95; Zalpha2 <- 1.96
alpha <- 99; Zalpha3 <- 2.57

nc <- Zalpha2
ErrorStd <- DesvEst*sqrt((n1+n2)/(n1*n2))
ErrorStd <- DesvEst*sqrt((1/n1)+(1/n2))

LimInf <- xbarra - nc*ErrorStd; print(LimInf)
LimSup <- xbarra + nc*ErrorStd; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
-------------------------------------------------------------------
  #  6. Se quiere comparar el tiempo de reacción de dos grupos de conductores en una situación de emergencia. Se
  # toman muestras de 25 conductores de cada grupo. La media de tiempo de reacción del grupo 1 es de 0.8
  # segundos con una desviación estándar de 0.1 segundos, mientras que la media de tiempo de reacción del grupo
  # 2 es de 0.9 segundos con una desviación estándar de 0.2 segundos. Construir un intervalo de confianza al 99 %
  # para la diferencia de medias entre ambos grupos.
  DesvEst <- 0.8
n1 <- 50
n2 <- 50
xbarra1 <- 3.5
xbarra2 <- 4.2             
xbarra <- xbarra1-xbarra2
alpha <- 90; Zalpha1 <- 1.64
alpha <- 95; Zalpha2 <- 1.96
alpha <- 99; Zalpha3 <- 2.57

nc <- Zalpha2
ErrorStd <- DesvEst*sqrt((n1+n2)/(n1*n2))
ErrorStd <- DesvEst*sqrt((1/n1)+(1/n2))

LimInf <- xbarra - nc*ErrorStd; print(LimInf)
LimSup <- xbarra + nc*ErrorStd; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
-----------------------------------------------------------------
  #   7. Se desea comparar la eficacia de dos métodos de enseñanza de matemáticas en alumnos de tercer grado de
  # primaria. Se toma una muestra de 50 alumnos para cada método. La media del puntaje en el examen del
  # método 1 es de 70 puntos con una desviación estándar de 5 puntos, mientras que la media del puntaje en el
  # examen del método 2 es de 75 puntos con una desviación estándar de 6 puntos. Construir un intervalo de
  # confianza al 95 % para la diferencia de medias entre ambos métodos.
  DesvEst <- 0.8
n1 <- 50
n2 <- 50
xbarra1 <- 3.5
xbarra2 <- 4.2             
xbarra <- xbarra1-xbarra2
alpha <- 90; Zalpha1 <- 1.64
alpha <- 95; Zalpha2 <- 1.96
alpha <- 99; Zalpha3 <- 2.57

nc <- Zalpha2
ErrorStd <- DesvEst*sqrt((n1+n2)/(n1*n2))
ErrorStd <- DesvEst*sqrt((1/n1)+(1/n2))

LimInf <- xbarra - nc*ErrorStd; print(LimInf)
LimSup <- xbarra + nc*ErrorStd; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
--------------------------------------------------------
  #v  8. Un estudio compara la tasa de mortalidad en dos hospitales. Se toman muestras de 100 pacientes de cada
  # hospital. La tasa de mortalidad en el hospital 1 es del 5 % con una desviación estándar del 1 %, mientras que
  # la tasa de mortalidad en el hospital 2 es del 8 % con una desviación estándar del 2 %. Construir un intervalo de
  # confianza al 90 % para la diferencia de proporciones entre ambos hospitales.
  DesvEst <- 0.8
n1 <- 50
n2 <- 50
xbarra1 <- 3.5
xbarra2 <- 4.2             
xbarra <- xbarra1-xbarra2
alpha <- 90; Zalpha1 <- 1.64
alpha <- 95; Zalpha2 <- 1.96
alpha <- 99; Zalpha3 <- 2.57

nc <- Zalpha2
ErrorStd <- DesvEst*sqrt((n1+n2)/(n1*n2))
ErrorStd <- DesvEst*sqrt((1/n1)+(1/n2))

LimInf <- xbarra - nc*ErrorStd; print(LimInf)
LimSup <- xbarra + nc*ErrorStd; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
---------------------------------------
  #  9. Se quiere comparar el rendimiento académico entre estudiantes de dos escuelas diferentes. Se toman muestras
  # de 30 estudiantes de cada escuela. La media de calificaciones en la escuela 1 es de 8.5 con una desviación
  # estándar de 1.2, mientras que la media de calificaciones en la escuela 2 es de 7.8 con una desviación estándar
  # de 1.5. Construir un intervalo de confianza al 99 % para la diferencia de medias entre ambas escuelas.
  DesvEst <- 0.8
n1 <- 50
n2 <- 50
xbarra1 <- 3.5
xbarra2 <- 4.2             
xbarra <- xbarra1-xbarra2
alpha <- 90; Zalpha1 <- 1.64
alpha <- 95; Zalpha2 <- 1.96
alpha <- 99; Zalpha3 <- 2.57

nc <- Zalpha2
ErrorStd <- DesvEst*sqrt((n1+n2)/(n1*n2))
ErrorStd <- DesvEst*sqrt((1/n1)+(1/n2))

LimInf <- xbarra - nc*ErrorStd; print(LimInf)
LimSup <- xbarra + nc*ErrorStd; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
-------------------------------------------------------
  #   10. Se toman dos muestras de tamaño 100 cada una. La primera muestra tiene una media de 25 y una desviación
  # estándar de 5, mientras que la segunda muestra tiene una media de 30 y una desviación estándar de 6. Construir
  # un intervalo de confianza al 95 % para la diferencia de medias entre ambas poblaciones.
  DesvEst <- 0.8
n1 <- 50
n2 <- 50
xbarra1 <- 3.5
xbarra2 <- 4.2             
xbarra <- xbarra1-xbarra2
alpha <- 90; Zalpha1 <- 1.64
alpha <- 95; Zalpha2 <- 1.96
alpha <- 99; Zalpha3 <- 2.57

nc <- Zalpha2
ErrorStd <- DesvEst*sqrt((n1+n2)/(n1*n2))
ErrorStd <- DesvEst*sqrt((1/n1)+(1/n2))

LimInf <- xbarra - nc*ErrorStd; print(LimInf)
LimSup <- xbarra + nc*ErrorStd; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
------------------------------------------
  # 11. Se quiere comparar la eficacia de dos tratamientos para reducir el dolor de cabeza. Se toma una muestra de
  # 150 pacientes para cada tratamiento. La media del grupo 1 es de 6.5 y la del grupo 2 es de 5.8. La desviación
  # estándar en ambos casos es de 1.2. Construir un intervalo de confianza al 99 % para la diferencia de medias
  # entre ambos tratamientos.
  DesvEst <- 0.8
n1 <- 50
n2 <- 50
xbarra1 <- 3.5
xbarra2 <- 4.2             
xbarra <- xbarra1-xbarra2
alpha <- 90; Zalpha1 <- 1.64
alpha <- 95; Zalpha2 <- 1.96
alpha <- 99; Zalpha3 <- 2.57

nc <- Zalpha2
ErrorStd <- DesvEst*sqrt((n1+n2)/(n1*n2))
ErrorStd <- DesvEst*sqrt((1/n1)+(1/n2))

LimInf <- xbarra - nc*ErrorStd; print(LimInf)
LimSup <- xbarra + nc*ErrorStd; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
----------------------------------------
  # 12. Se desea comparar la velocidad de procesamiento de dos computadoras diferentes. Se toman muestras de
  # 50 usuarios para cada computadora. La media de velocidad de la computadora 1 es de 4.2 GHz con una
  # desviación estándar de 0.5 GHz, mientras que la media de velocidad de la computadora 2 es de 4.5 GHz con
  # una desviación estándar de 0.6 GHz. Construir un intervalo de confianza al 90 % para la diferencia de medias
  # entre ambas computadoras.
  DesvEst <- 0.8
n1 <- 50
n2 <- 50
xbarra1 <- 3.5
xbarra2 <- 4.2             
xbarra <- xbarra1-xbarra2
alpha <- 90; Zalpha1 <- 1.64
alpha <- 95; Zalpha2 <- 1.96
alpha <- 99; Zalpha3 <- 2.57

nc <- Zalpha2
ErrorStd <- DesvEst*sqrt((n1+n2)/(n1*n2))
ErrorStd <- DesvEst*sqrt((1/n1)+(1/n2))

LimInf <- xbarra - nc*ErrorStd; print(LimInf)
LimSup <- xbarra + nc*ErrorStd; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
-----------------------------------------------
  # 13. Se quiere comparar el rendimiento académico de estudiantes de dos escuelas diferentes. Se toman muestras
  # de 80 estudiantes para cada escuela. La media de calificaciones en la escuela 1 es de 7.8 con una desviación
  # estándar de 1.2, mientras que la media de calificaciones en la escuela 2 es de 8.2 con una desviación estándar
  # de 1.3. Construir un intervalo de confianza al 95 % para la diferencia de medias entre ambas escuelas.
  DesvEst <- 0.8
n1 <- 50
n2 <- 50
xbarra1 <- 3.5
xbarra2 <- 4.2             
xbarra <- xbarra1-xbarra2
alpha <- 90; Zalpha1 <- 1.64
alpha <- 95; Zalpha2 <- 1.96
alpha <- 99; Zalpha3 <- 2.57

nc <- Zalpha2
ErrorStd <- DesvEst*sqrt((n1+n2)/(n1*n2))
ErrorStd <- DesvEst*sqrt((1/n1)+(1/n2))

LimInf <- xbarra - nc*ErrorStd; print(LimInf)
LimSup <- xbarra + nc*ErrorStd; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
------------------------------------------
  #  14. Se desea comparar la eficacia de dos programas de entrenamiento para corredores. Se toman muestras de
  # 200 corredores para cada programa. La media de tiempo de carrera del programa 1 es de 30 minutos con
  # una desviación estándar de 2.5 minutos, mientras que la media de tiempo de carrera del programa 2 es de
  # 28 minutos con una desviación estándar de 2.7 minutos. Construir un intervalo de confianza al 99 % para la
  # diferencia de medias entre ambos programas.
  DesvEst <- 0.8
n1 <- 50
n2 <- 50
xbarra1 <- 3.5
xbarra2 <- 4.2             
xbarra <- xbarra1-xbarra2
alpha <- 90; Zalpha1 <- 1.64
alpha <- 95; Zalpha2 <- 1.96
alpha <- 99; Zalpha3 <- 2.57

nc <- Zalpha2
ErrorStd <- DesvEst*sqrt((n1+n2)/(n1*n2))
ErrorStd <- DesvEst*sqrt((1/n1)+(1/n2))

LimInf <- xbarra - nc*ErrorStd; print(LimInf)
LimSup <- xbarra + nc*ErrorStd; print(LimSup)

Intervalo <- paste0("mu ∈  ","(",round(LimInf,2),",",
                    round(LimSup,2),")"); 
print(Intervalo)
-------------------------------------------------------------------------------------------------------
  #Varianzas desconocidas e iguales
  #  1. Se quiere comparar la duración de dos tipos de baterías en un mismo dispositivo electrónico. Se toman muestras
  # de 10 baterías de cada tipo. La duración media de las baterías tipo A es de 25 horas con una desviación
  # estándar de 3 horas, mientras que la duración media de las baterías tipo B es de 30 horas con una desviación
  # estándar de 4 horas. Construir un intervalo de confianza al 95 % para la diferencia de medias entre ambas
  # baterías.
  n1 <- 10
n2 <- 10
xbarra1 <- 25 
xbarra2 <- 30
S1 <- 3
S2 <- 4
xbarra <- xbarra1-xbarra2
nc <- 0.95
Sd <- sqrt(((n1-1)*S1^2+(n2-1)*S2^2)/(n1+n2-2))
gl = n1+n2-2
Talpha2 <- round(qt(nc, gl),4)
LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)

--------------------------------------------------
  # 2. Un estudio compara el efecto de dos tratamientos para reducir el dolor en pacientes con artritis. Se toman
  # muestras de 15 pacientes para cada tratamiento. La media de reducción del dolor en el tratamiento 1 es de 2.5
  # con una desviación estándar de 0.8, mientras que la media de reducción del dolor en el tratamiento 2 es de 3
  # con una desviación estándar de 0.9. Construir un intervalo de confianza al 99 % para la diferencia de medias
  # entre ambos tratamientos.
  
  n1 <- 15
n2 <- 15
xbarra1 <-  2.5
xbarra2 <- 3
S1 <- 0.8
S2 <- 0.9
xbarra <- xbarra1-xbarra2
nc <- 0.99
Sd <- sqrt(((n1-1)*S1^2+(n2-1)*S2^2)/(n1+n2-2))
gl = n1+n2-2
Talpha2 <- round(qt(nc, gl),4)
LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
---------------------------------
  # 3. Se quiere comparar la calidad de dos marcas de café. Se toman muestras de 8 tazas de cada marca. La media
  # del puntaje en sabor del café de la marca A es de 8 con una desviación estándar de 1.2, mientras que la media
  # del puntaje en sabor del café de la marca B es de 7 con una desviación estándar de 1.1. Construir un intervalo
  # de confianza al 90 % para la diferencia de medias entre ambas marcas.
  n1 <- 8
n2 <- 8
xbarra1 <- 8
xbarra2 <- 7
S1 <- 1.2
S2 <- 1.1 
xbarra <- xbarra1-xbarra2
nc <- 0.90
Sd <- sqrt(((n1-1)*S1^2+(n2-1)*S2^2)/(n1+n2-2))
gl = n1+n2-2
Talpha2 <- round(qt(nc, gl),4)
LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
------------------------------------
  #  4. Un estudio compara el efecto de dos programas de entrenamiento en la fuerza muscular. Se toman muestras
  # de 12 atletas para cada programa. La media de ganancia de fuerza en el programa 1 es de 10 kg con una
  # desviación estándar de 2 kg, mientras que la media de ganancia de fuerza en el programa 2 es de 12 kg con
  # una desviación estándar de 3 kg. Construir un intervalo de confianza al 95 % para la diferencia de medias entre
  # ambos programas.
  n1 <-  12
n2 <-  12
xbarra1 <- 10 
xbarra2 <- 12
S1 <- 2
S2 <- 3
xbarra <- xbarra1-xbarra2
nc <- 0.95
Sd <- sqrt(((n1-1)*S1^2+(n2-1)*S2^2)/(n1+n2-2))
gl = n1+n2-2
Talpha2 <- round(qt(nc, gl),4)
LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)

-------------------------------------------------
  # 5. Se quiere comparar la eficacia de dos métodos de enseñanza de idiomas en estudiantes de secundaria. Se toman
  # muestras de 5 estudiantes para cada método. La media del puntaje en el examen del método 1 es de 85 puntos
  # con una desviación estándar de 6 puntos, mientras que la media del puntaje en el examen del método 2 es
  # de 80 puntos con una desviación estándar de 7 puntos. Construir un intervalo de confianza al 99 % para la
  # diferencia de medias entre ambos métodos.
  n1 <-  5
n2 <- 5
xbarra1 <-  85
xbarra2 <-  80
S1 <- 6
S2 <- 7
xbarra <- xbarra1-xbarra2
nc <- 0.99
Sd <- sqrt(((n1-1)*S1^2+(n2-1)*S2^2)/(n1+n2-2))
gl = n1+n2-2
Talpha2 <- round(qt(nc, gl),4)
LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
-------------------------------------------
  # 6. Un estudio compara la efectividad de dos medicamentos para reducir la presión arterial en pacientes con
  # hipertensión. Se toman muestras de 6 pacientes para cada medicamento. La media de la presión arterial en el
  # medicamento 1 es de 140 mmHg con una desviación estándar de 5 mmHg, mientras que la media de la presión
  # arterial en el medicamento 2 es de 135 mmHg con una desviación estándar de 4 mmHg. Construir un intervalo
  # de confianza al 95 % para la diferencia de medias entre ambos medicamentos.
  n1 <- 6
n2 <- 6
xbarra1 <-  140
xbarra2 <- 135
S1 <- 5
S2 <- 4
xbarra <- xbarra1-xbarra2
nc <- 0.95
Sd <- sqrt(((n1-1)*S1^2+(n2-1)*S2^2)/(n1+n2-2))
gl = n1+n2-2
Talpha2 <- round(qt(nc, gl),4)
LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
-------------------------------
  #  7. Se quiere comparar el rendimiento de dos marcas de neumáticos en condiciones de lluvia. Se toman muestras
  # de 10 neumáticos de cada marca. La distancia de frenado media de los neumáticos de la marca A es de 25
  # metros con una desviación estándar de 2 metros, mientras que la distancia de frenado media de los neumáticos
  # de la marca B es de 22 metros con una desviación estándar de 3 metros. Construir un intervalo de confianza al
  # 90 % para la diferencia de medias entre ambas marcas.
  n1 <- 10
n2 <- 10
xbarra1 <- 25
xbarra2 <-  22
S1 <- 2
S2 <- 3
xbarra <- xbarra1-xbarra2
nc <- 0.90
Sd <- sqrt(((n1-1)*S1^2+(n2-1)*S2^2)/(n1+n2-2))
gl = n1+n2-2
Talpha2 <- round(qt(nc, gl),4)
LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
-------------------------------------------
  # 8. Un estudio compara la calidad de dos tipos de lentes de contacto. Se toman muestras de 8 lentes de contacto
  # de cada tipo. La media de la duración de los lentes de contacto del tipo 1 es de 14 horas con una desviación
  # estándar de 1.2 horas, mientras que la media de la duración de los lentes de contacto del tipo 2 es de 15 horas
  # con una desviación estándar de 1.4 horas. Construir un intervalo de confianza al 95 % para la diferencia de
  # medias entre ambos tipos.
  n1 <- 8
n2 <- 8
xbarra1 <-  14
xbarra2 <- 15 
S1 <- 1.2
S2 <- 1.4 
xbarra <- xbarra1-xbarra2
nc <- 0.95
Sd <- sqrt(((n1-1)*S1^2+(n2-1)*S2^2)/(n1+n2-2))
gl = n1+n2-2
Talpha2 <- round(qt(nc, gl),4)
LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
------------------------------
  # 9. Se quiere comparar el rendimiento de dos tipos de lápices en la escritura en papel rugoso. Se toman muestras
  # de 12 lápices de cada tipo. La media de la escritura del lápiz tipo A es de 5.5 con una desviación estándar de
  # 0.8, mientras que la media de la escritura del lápiz tipo B es de 6 con una desviación estándar de 1.1. Construir
  # un intervalo de confianza al 99 % para la diferencia de medias entre ambos tipos.
  n1 <- 12
n2 <- 12
xbarra1 <- 5.5
xbarra2 <-  6
S1 <-0.8
S2 <- 1.1
xbarra <- xbarra1-xbarra2
nc <- 0.99
Sd <- sqrt(((n1-1)*S1^2+(n2-1)*S2^2)/(n1+n2-2))
gl = n1+n2-2
Talpha2 <- round(qt(nc, gl),4)
LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
---------------------------------
  # 10. Un estudio compara el efecto de dos tipos de fertilizantes en el crecimiento de plantas. Se toman muestras de
  # 15 plantas para cada tipo de fertilizante. La media de la altura de las plantas con el fertilizante 1 es de 25 cm
  # con una desviación estándar de 3 cm, mientras que la media de la altura de las plantas con el fertilizante 2 es
  # de 28 cm con una desviación estándar de 4 cm. Construir un intervalo de confianza al 90 % para la diferencia
  # de medias entre ambos fertilizantes.
  n1 <-  1
n2 <- 2
xbarra1 <- 25 
xbarra2 <- 28
S1 <- 3
S2 <- 4
xbarra <- xbarra1-xbarra2
nc <- 0.90
Sd <- sqrt(((n1-1)*S1^2+(n2-1)*S2^2)/(n1+n2-2))
gl = n1+n2-2
Talpha2 <- round(qt(nc, gl),4)
LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
------------------------------------
  #  11. Se quiere comparar la efectividad de dos programas de entrenamiento en fuerza para atletas. Se toman muestras
  # aleatorias de 8 atletas para cada programa y se obtienen los siguientes resultados en aumento de peso levantado
  # en kg: programa A (44, 37, 50, 41, 48, 36, 45, 38) y programa B (42, 39, 44, 40, 37, 47, 45, 41). Construir
  # un intervalo de confianza al 95 % para la diferencia de medias de aumento de peso levantado entre ambos
  # programas.
  n1 <- 2
n2 <-2
xbarra1 <- 8
xbarra2 <- 8
S1 <-44                          
S2 <- 42
xbarra <- xbarra1-xbarra2                         **********************
  nc <- 0.95
Sd <- sqrt(((n1-1)*S1^2+(n2-1)*S2^2)/(n1+n2-2))
gl = n1+n2-2
Talpha2 <- round(qt(nc, gl),4)
LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
----------------------------------------
  # 12. Un estudio comparativo quiere analizar si hay una diferencia en la cantidad de calorías quemadas durante
  # dos tipos de entrenamiento cardiovascular. Se toman muestras aleatorias de 10 personas para cada tipo de
  # entrenamiento y se obtienen los siguientes resultados en calorías quemadas: tipo A (124, 134, 137, 120, 129,
  # 132, 136, 131, 130, 128) y tipo B (136, 125, 128, 122, 130, 132, 138, 129, 131, 133). Construir un intervalo de
  # confianza al 90 % para la diferencia de medias de calorías quemadas entre ambos tipos de entrenamiento.
  n1 <- 
  n2 <- 
  xbarra1 <- 
  xbarra2 <- 
  S1 <- 
  S2 <- 
  xbarra <- xbarra1-xbarra2
nc <- 0.95
Sd <- sqrt(((n1-1)*S1^2+(n2-1)*S2^2)/(n1+n2-2))
gl = n1+n2-2
Talpha2 <- round(qt(nc, gl),4)
LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
-----------------------------------------
  #  13. Se quiere comparar el tiempo promedio de entrega de dos compañías de paquetería. Se toman muestras
  # aleatorias de 12 entregas para cada compañía y se obtienen los siguientes resultados en días: compañía A (4, 5,6, 3,
  # 4, 5, 6, 3, 4, 5, 6, 3) y compañía B (5, 6, 4, 5, 6, 3, 4, 6, 5, 4, 6, 5). Construir un intervalo de confianza al
  # 99 % para la diferencia de medias de tiempo de entrega entre ambas compañías.
  n1 <- 2
n2 <- 2
xbarra1 <-  12                       **********************
  xbarra2 <- 12
S1 <- 4
S2 <-.5
xbarra <- xbarra1-xbarra2
nc <- 0.99
Sd <- sqrt(((n1-1)*S1^2+(n2-1)*S2^2)/(n1+n2-2))    ****************
  gl = n1+n2-2
Talpha2 <- round(qt(nc, gl),4)
LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
-----------------------------
  #  14. Se quiere comparar la efectividad de dos tratamientos para reducir los niveles de colesterol en la sangre. Se
  # toman muestras aleatorias de 15 pacientes para cada tratamiento y se obtienen los siguientes resultados en
  # mg/dL: tratamiento A (182, 172, 193, 198, 168, 178, 186, 192, 182, 175, 168, 177, 183, 188, 178) y tratamiento
  # B (188, 198, 183, 192, 202, 194, 187, 182, 189, 193, 196, 185, 182, 197, 190). Construir un intervalo de confianza
  # al 95 % para la diferencia de medias de niveles de colesterol entre ambos tratamientos.
  n1 <- 2
n2 <- 2
xbarra1 <-  15
xbarra2 <-  15
S1 <- 182
S2 <- 188                         *************
  xbarra <- xbarra1-xbarra2
nc <- 0.95
Sd <- sqrt(((n1-1)*S1^2+(n2-1)*S2^2)/(n1+n2-2))
gl = n1+n2-2
Talpha2 <- round(qt(nc, gl),4)
LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
----------------------
  # 15. Se quiere comparar la productividad en la línea de producción de dos turnos de trabajo. Se toman muestras
  # aleatorias de 7 horas para cada turno y se obtienen los siguientes resultados en unidades producidas: turno A
  # (57, 60, 59, 62, 58, 56, 61) y turno B (63, 61, 64, 60, 62, 61, 63). Construir un intervalo de confianza al 95 %
  # para la diferencia de medias de productividad entre ambos.
  n1 <- 2
n2 <- 2 
xbarra1 <- 7
xbarra2 <- 7              *******************
  S1 <- 57
S2 <- 63
xbarra <- xbarra1-xbarra2
nc <- 0.95
Sd <- sqrt(((n1-1)*S1^2+(n2-1)*S2^2)/(n1+n2-2))
gl = n1+n2-2
Talpha2 <- round(qt(nc, gl),4)
LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
-----------------------------------------------------------------------------------------------------------------
  #Varianzas desconocidas y distintas
  
  
  #  1. Se quiere comparar la efectividad de dos medicamentos para el dolor de cabeza. Se toma una muestra de
  # 10 pacientes para el medicamento A y otra muestra de 12 pacientes para el medicamento B. Los resultados
  # muestran que la media del medicamento A es de 6.5 y la media del medicamento B es de 5.8. Las desviaciones
  # estándar son de 0.7 para el medicamento A y 0.9 para el medicamento B. Construya un intervalo de confianza
  # del 95 % para la diferencia de medias.
  n1 <- 10
n2 <- 12
xbarra1 <- 6.5
xbarra2 <- 5.8
S1 <- 0.7
S2 <- 0.9
xbarra <- xbarra1-xbarra2
nc <- 0.975
Sd <- sqrt(((S1^2)/n1)+((S2^2)/n2))

numerador <- (((S1^2)/n1)+((S2^2)/n2))^2
denominador <- ((((S1^2)/n1)^2)/(n1-1))+((((S2^2)/n2)^2)/(n2-1))
gl = ceiling(numerador/denominador)
Talpha2 <- round(qt(nc, gl),4)

LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
----------------------------------------------
  #2. Un gerente de recursos humanos quiere comparar los salarios promedio de dos departamentos de su empresa. Se
  # toma una muestra de 8 empleados del departamento A con una media de salario de $25,000 y una desviación
  # estándar de $1,500. Se toma otra muestra de 12 empleados del departamento B con una media de salario de
  # $28,000 y una desviación estándar de $2,000. Calcule un intervalo de confianza del 90 % para la diferencia de
  # salarios promedio.
  n1 <- 8
n2 <-  12
xbarra1 <- 2500
xbarra2 <- 28000
S1 <- 1500
S2 <- 2000
xbarra <- xbarra1-xbarra2
nc <- 0.90
Sd <- sqrt(((S1^2)/n1)+((S2^2)/n2))

numerador <- (((S1^2)/n1)+((S2^2)/n2))^2
denominador <- ((((S1^2)/n1)^2)/(n1-1))+((((S2^2)/n2)^2)/(n2-1))
gl = ceiling(numerador/denominador)
Talpha2 <- round(qt(nc, gl),4)

LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
---------------------------
  #3. Se quiere comparar la eficacia de dos tipos de fertilizantes en el crecimiento de plantas. Se toman dos muestras
  # de 15 plantas cada una y se miden las alturas. La media de las alturas para el fertilizante A es de 16 cm con
  # una desviación estándar de 2 cm, mientras que la media de las alturas para el fertilizante B es de 18 cm con
  # una desviación estándar de 3 cm. Construya un intervalo de confianza del 99 % para la diferencia de medias.
  n1 <- 15
n2 <- 15
xbarra1 <-  2   ********
  xbarra2 <- 3 
S1 <-16 
S2 <- 18 
xbarra <- xbarra1-xbarra2
nc <- 0.99
Sd <- sqrt(((S1^2)/n1)+((S2^2)/n2))

numerador <- (((S1^2)/n1)+((S2^2)/n2))^2
denominador <- ((((S1^2)/n1)^2)/(n1-1))+((((S2^2)/n2)^2)/(n2-1))
gl = ceiling(numerador/denominador)
Talpha2 <- round(qt(nc, gl),4)

LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
-------------------------------------
  #4. Un equipo de investigadores quiere comparar la duración media de dos tipos de baterías para dispositivos
  # móviles. Se toma una muestra de 10 baterías del tipo A con una duración media de 15 horas y una desviación
  # estándar de 1.2 horas. Se toma otra muestra de 12 baterías del tipo B con una duración media de 13 horas
  # y una desviación estándar de 1.5 horas. Construya un intervalo de confianza del 95 % para la diferencia de
  # duraciones medias.
  n1 <- 10
n2 <- 12
xbarra1 <-15
xbarra2 <-13 
S1 <-1.2 
S2 <- 1.5
xbarra <- xbarra1-xbarra2
nc <- 0.95
Sd <- sqrt(((S1^2)/n1)+((S2^2)/n2))

numerador <- (((S1^2)/n1)+((S2^2)/n2))^2
denominador <- ((((S1^2)/n1)^2)/(n1-1))+((((S2^2)/n2)^2)/(n2-1))
gl = ceiling(numerador/denominador)
Talpha2 <- round(qt(nc, gl),4)

LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
---------------------------------
  # 5. Un estudio quiere comparar el rendimiento promedio de dos tipos de semillas de tomate. Se toman dos muestras
  # de 20 plantas cada una y se miden las producciones de tomate. La media de producción para las semillas del
  # tipo A es de 30 kg con una desviación estándar de 4 kg, mientras que la media de producción para las semillas
  # del tipo B es de 35 kg con una desviación estándar de 6 kg. Construya un intervalo de confianza del 99 % para
  # la diferencia de medias.
  n1 <- 20 
n2 <-  20
xbarra1 <-  30
xbarra2 <-  35
S1 <- 4 
S2 <- 6 
xbarra <- xbarra1-xbarra2
nc <- 0.99
Sd <- sqrt(((S1^2)/n1)+((S2^2)/n2))

numerador <- (((S1^2)/n1)+((S2^2)/n2))^2
denominador <- ((((S1^2)/n1)^2)/(n1-1))+((((S2^2)/n2)^2)/(n2-1))
gl = ceiling(numerador/denominador)
Talpha2 <- round(qt(nc, gl),4)

LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
-----------------------------------------
  # 6. Se quiere comparar la eficacia de dos métodos de enseñanza de matemáticas en dos grupos de estudiantes, con
  # tamaños de muestra de n1 = 20 y n2 = 25. Se obtuvieron las siguientes estadísticas muestrales: d1 = 70, s1 = 8
  # y d2 = 74, s2 = 10. Construir un intervalo de confianza del 95 % para la diferencia de las medias poblacionales.
  n1 <- 20 
n2 <- 25
xbarra1 <-  70
xbarra2 <- 74
S1 <- 8 
S2 <- 10
xbarra <- xbarra1-xbarra2
nc <- 0.95
Sd <- sqrt(((S1^2)/n1)+((S2^2)/n2))

numerador <- (((S1^2)/n1)+((S2^2)/n2))^2
denominador <- ((((S1^2)/n1)^2)/(n1-1))+((((S2^2)/n2)^2)/(n2-1))
gl = ceiling(numerador/denominador)
Talpha2 <- round(qt(nc, gl),4)

LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
--------------------------------------------------
  #7. Se desea comparar los tiempos de reacción de dos tipos de conductores al conducir en condiciones de lluvia,
  # con tamaños de muestra de n1 = 15 y n2 = 20. Los tiempos de reacción en segundos se registraron y se
  # obtuvieron las siguientes estadísticas muestrales: d1 = 2,5, s1 = 0,6 y d2 = 2,1, s2 = 0,5. Construir un intervalo
  # de confianza del 99 % para la diferencia de las medias poblacionales.
  n1 <- 15 
n2 <- 20
xbarra1 <- 2.5
xbarra2 <- 2.1
S1 <- 0.6
S2 <- 0.5
xbarra <- xbarra1-xbarra2
nc <- 0.99
Sd <- sqrt(((S1^2)/n1)+((S2^2)/n2))

numerador <- (((S1^2)/n1)+((S2^2)/n2))^2
denominador <- ((((S1^2)/n1)^2)/(n1-1))+((((S2^2)/n2)^2)/(n2-1))
gl = ceiling(numerador/denominador)
Talpha2 <- round(qt(nc, gl),4)

LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
------------------------------------
  #8. Se realizó un estudio para comparar el desempeño de dos grupos de trabajadores en una empresa en términos
  # de productividad. Se obtuvieron muestras de tamaños n1 = 18 y n2 = 25, y se calcularon las siguientes
  # estadísticas muestrales: d1 = 120, s1 = 6 y d2 = 125, s2 = 8,5. Construir un intervalo de confianza del 90 %
  # para la diferencia de las medias poblacionales.
  n1 <- 18
n2 <- 25
xbarra1 <- 120
xbarra2 <-  125
S1 <- 6
S2 <- 8.5
xbarra <- xbarra1-xbarra2
nc <- 0.90
Sd <- sqrt(((S1^2)/n1)+((S2^2)/n2))

numerador <- (((S1^2)/n1)+((S2^2)/n2))^2
denominador <- ((((S1^2)/n1)^2)/(n1-1))+((((S2^2)/n2)^2)/(n2-1))
gl = ceiling(numerador/denominador)
Talpha2 <- round(qt(nc, gl),4)

LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
------------------------------------------------
  # 9. Se quiere comparar la efectividad de dos tratamientos médicos para reducir los niveles de colesterol en pacientes
  # con enfermedad cardiovascular. Se seleccionaron muestras de tamaños n1 = 12 y n2 = 15, y se obtuvieron las
  # siguientes estadísticas muestrales: d1 = 15, s1 = 3,5 y d2 = 12, s2 = 4. Construir un intervalo de confianza del
  # 95 % para la diferencia de las medias poblacionales.
  n1 <-  12
n2 <-  15
xbarra1 <-  15
xbarra2 <-  12
S1 <- 3.5 
S2 <-  4
xbarra <- xbarra1-xbarra2
nc <- 0.95
Sd <- sqrt(((S1^2)/n1)+((S2^2)/n2))

numerador <- (((S1^2)/n1)+((S2^2)/n2))^2
denominador <- ((((S1^2)/n1)^2)/(n1-1))+((((S2^2)/n2)^2)/(n2-1))
gl = ceiling(numerador/denominador)
Talpha2 <- round(qt(nc, gl),4)

LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
------------------------------------------------
  #10. Se desea comparar la precisión de dos marcas de básculas en la medición de pesos de objetos pequeños. Se
  # tomaron muestras de tamaños n1 = 10 y n2 = 15, y se obtuvieron las siguientes estadísticas muestrales:
  #  d1 = 50, s1 = 1,5 y d2 = 51, s2 = 2. Construir un intervalo de confianza del 99 % para la diferencia de las
  # medias poblacionales.
  n1 <- 10
n2 <- 15
xbarra1 <-  50
xbarra2 <-  51
S1 <- 1.5
S2 <- 2
xbarra <- xbarra1-xbarra2
nc <- 0.99
Sd <- sqrt(((S1^2)/n1)+((S2^2)/n2))

numerador <- (((S1^2)/n1)+((S2^2)/n2))^2
denominador <- ((((S1^2)/n1)^2)/(n1-1))+((((S2^2)/n2)^2)/(n2-1))
gl = ceiling(numerador/denominador)
Talpha2 <- round(qt(nc, gl),4)

LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
------------------------------------------
  # 11. Se realizó un experimento para evaluar la efectividad de dos tipos de fertilizantes en la producción de maíz.
  # Se tomaron muestras de tamaños n1 = 20 y n2 = 25, y se obtuvieron las siguientes estadísticas muestrales:
  #  d1 = 150, s1 = 12 y d2 = 145, s2 = 10. Construir un intervalo de confianza del 95 % para la diferencia de las
  # medias poblacionales.n1 <- 10
  n1 <- 20
n2 <-  25
xbarra1 <-  150    **************
  xbarra2 <-145
S1 <- 12
S2 <- 10
xbarra <- xbarra1-xbarra2
nc <- 0.95
Sd <- sqrt(((S1^2)/n1)+((S2^2)/n2))

numerador <- (((S1^2)/n1)+((S2^2)/n2))^2
denominador <- ((((S1^2)/n1)^2)/(n1-1))+((((S2^2)/n2)^2)/(n2-1))
gl = ceiling(numerador/denominador)
Talpha2 <- round(qt(nc, gl),4)

LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
--------------------------------------------------
  # 12. Se quiere comparar la cantidad promedio de horas de sueño entre dos grupos de estudiantes universitarios: uno
  # que consume bebidas energéticas y otro que no. Se toma una muestra de 10 estudiantes de cada grupo y se
  # obtiene que el grupo que consume bebidas energéticas duerme en promedio 6.2 horas y tiene una desviación
  # estándar de 1.2 horas, mientras que el grupo que no consume bebidas energéticas duerme en promedio 7.5
  # horas y tiene una desviación estándar de 1.8 horas. ¿A un nivel de confianza del 95 %, se puede concluir que
  # hay una diferencia significativa en la cantidad promedio de horas de sueño entre los dos grupo
  n1 <- 10 
n2 <-  10
xbarra1 <- 6.2
xbarra2 <-  7.5
S1 <-  1.2
S2 <-  1.8
xbarra <- xbarra1-xbarra2
nc <- 0.95
Sd <- sqrt(((S1^2)/n1)+((S2^2)/n2))

numerador <- (((S1^2)/n1)+((S2^2)/n2))^2
denominador <- ((((S1^2)/n1)^2)/(n1-1))+((((S2^2)/n2)^2)/(n2-1))
gl = ceiling(numerador/denominador)
Talpha2 <- round(qt(nc, gl),4)

LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
-----------------------------------------------------------------------------
  # 13. Se quiere comparar el tiempo promedio de reacción entre dos grupos de conductores: uno que usa un 
  #nuevosistema de alerta de colisión y otro que no. Se toma una muestra de 15 conductores de cada grupo y se obtiene
  #que el grupo que usa el nuevo sistema tiene un tiempo promedio de reacción de 0.8 segundos y una desviación
  # estándar de 0.2 segundos, mientras que el grupo que no usa el sistema tiene un tiempo promedio de reacción de
  # 0.9 segundos y una desviación estándar de 0.3 segundos. ¿A un nivel de confianza del 99 %, se puede concluir
  # que el nuevo sistema de alerta de colisión tiene un efecto significativo en el tiempo promedio de reacción de los
  # conductores?
  n1 <-  15
n2 <-  15
xbarra1 <- 0.8
xbarra2 <- 0.9
S1 <- 0.2
S2 <-0.3 
xbarra <- xbarra1-xbarra2
nc <- 0.99
Sd <- sqrt(((S1^2)/n1)+((S2^2)/n2))

numerador <- (((S1^2)/n1)+((S2^2)/n2))^2
denominador <- ((((S1^2)/n1)^2)/(n1-1))+((((S2^2)/n2)^2)/(n2-1))
gl = ceiling(numerador/denominador)
Talpha2 <- round(qt(nc, gl),4)

LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
----------------------------------------------------------------
  #  14. Un estudio compara la efectividad de dos métodos de enseñanza de matemáticas en dos grupos de estudiantes,
  # con tamaños de muestra de 12 y 15 respectivamente. La media y desviación estándar de los resultados obtenidos
  # en cada grupo son: grupo 1 (media= 75, s = 10) y grupo 2 (media= 80, s = 12). Construir un intervalo de
  # confianza del 95 % para la diferencia de las medias poblacionales.
  n1 <-12
n2 <- 15
xbarra1 <- 75
xbarra2 <-  80
S1 <- 10 
S2 <- 12
xbarra <- xbarra1-xbarra2
nc <- 0.95
Sd <- sqrt(((S1^2)/n1)+((S2^2)/n2))

numerador <- (((S1^2)/n1)+((S2^2)/n2))^2
denominador <- ((((S1^2)/n1)^2)/(n1-1))+((((S2^2)/n2)^2)/(n2-1))
gl = ceiling(numerador/denominador)
Talpha2 <- round(qt(nc, gl),4)

LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)
--------------------------------------------------------------
  # 15. Un psicólogo quiere comparar la autoestima de un grupo de pacientes con ansiedad con un grupo de pacientes
  # sin ansiedad. Para ello, toma una muestra de 10 pacientes con ansiedad y otra muestra de 8 pacientes sin
  # ansiedad. La media y desviación estándar de la autoestima de los pacientes con ansiedad son 25 y 3, mientras
  # que para los pacientes sin ansiedad son 30 y 4. Construir un intervalo de confianza del 99 % para la diferencia
  # de las medias poblacionales.
  n1 <- 10
n2 <- 8
xbarra1 <- 25
xbarra2 <- 30
S1 <- 3
S2 <- 4
xbarra <- xbarra1-xbarra2
nc <- 0.99
Sd <- sqrt(((S1^2)/n1)+((S2^2)/n2))

numerador <- (((S1^2)/n1)+((S2^2)/n2))^2
denominador <- ((((S1^2)/n1)^2)/(n1-1))+((((S2^2)/n2)^2)/(n2-1))
gl = ceiling(numerador/denominador)
Talpha2 <- round(qt(nc, gl),4)

LimInf <- xbarra - Talpha2*(Sd); 
LimSup <- xbarra + Talpha2*(Sd); 
Intervalo <- paste0("mu ∈  ","(",
                    round(LimInf,2),
                    ",",
                    round(LimSup,2),
                    ")"); 
print(Intervalo)

























