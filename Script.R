# SCRIPT DE LA PRACTICA DE APRENDIZAJE COMPUTACIONAL 2024-2025




if(!require("caret")) {
  install.packages("caret", dependencies = c("Depends", "Suggests"))
  require(caret)
}

# Descargamos la base de datos
url <- "https://archive.ics.uci.edu/static/public/27/credit+approval.zip"
download.file(url, destfile = "credit_approval.zip")


# Descomprimimos la base de datos
unzip("credit_approval.zip")


# Cargamos la base de datos, na.string = "?" quitamos los datos con ese valor y lo sustituye por NA
credit <- read.table("crx.data", header = FALSE, sep = ",", na.strings = "?")


# Cargamos en credit.trainIdx la base de datos descargada del UCI
credit.trainIdx <- readRDS("credit.trainIdx.rds")
credit.Datos.Train <- credit[credit.trainIdx,]
credit.Datos.Test <- credit[-credit.trainIdx,]


# Estos comandos es para comprobar que tiene 553 observaciones
nrow(credit.Datos.Train)
nrow(credit.Datos.Test)

# Muestra la cantidad de valores NA por columna
# Esto es por si alguna columna tiene demasiados valores NA para eliminarla
colSums(is.na(credit))  

# El resultado que ves al ejecutar summary(credit$V16) sugiere que V16, que debería 
# ser la columna de clase con valores + o -, está tratada como un vector de caracteres 
# en lugar de una variable categórica o factor en R.

# PREGUNTAAAAAAAAAAAAAAA TEACHER 
# credit <- data.frame(lapply(credit, FUN=as.factor))


#Ahora tenemos que comprobar que todos los niveles están correctos
# comparando con lo que tenemos en la web:
# al reemplazar "?" con NA, R sigue considerando "?" como un nivel 
# válido a menos que uses droplevels() para actualizar los niveles activos.

# Visualizamos la tabla
str(credit)

# Convertir automáticamente columnas de tipo 'chr' a 'factor'
credit[sapply(credit, is.character)] <- lapply(credit[sapply(credit, is.character)], as.factor)


# Tratamos la columna V1

# Visualizamos los niveles
levels(credit$V1)

# Eliminamos niveles no usados
credit$V1 <- droplevels(credit$V1)

# Comprobamos
levels(credit$V1)

# El siguiente con erroes es V2
levels(credit$V2)



# V4
# Primero vamos a ver el interrogante
levels(credit$V4)

credit$V4 <- droplevels(credit$V4)

# Ahora vamos a añadir un elemento que no aparece y luego lo hacemos factor
levels(credit$V4)<-c(levels(credit$V4),"t")
str(credit$V4)

# V5 -> ?
levels(credit$V5)
credit$V5 <- droplevels(credit$V5)


# V6 -> ?
levels(credit$V6)
credit$V6 <- droplevels(credit$V6)


# V7
levels(credit$V7)
credit$V7 <- droplevels(credit$V7)


# V14
levels(credit$V14)





# Ahora vamos a renombrar algunas columnas para ganar legibilidad
levels(credit$V16) <- c("rechazada", "aprobada")


# Comprobamos que no hay missing-data:
sum(!complete.cases(credit))
# De momento no haremos nada con estos 37 datos perdidos

# Ya hemos concluido este paso de arreglar los datos
summary(credit)
str(credit)




# ANALISIS MONOVARIABLE


# Decidimos que es interesante analizar V2 ya que su media y mediana son 
# parecidas, lo cual sugiere que la distribución puede ser simétrica (cercana a normal).

# Generamos un histograma base con la función hist() de R para la variable V2
# con probability = TRUE para mostrar la densidad en vez de las frecuencias absolutas.
hist(credit$V2, probability = TRUE, main = "Histograma de V2 con Curva de Densidad")

# Añadimos una línea de densidad para observar la forma de la distribución de V2,
# usando la función lines() y especificando el color "blue" para la curva.
lines(density(credit$V2, na.rm = TRUE), col = "blue")

# Creamos un histograma con ggplot2 para la variable V2 del dataframe 'credit',
# estableciendo aesthetic mapping (aes) para usar V2 en el eje x.
myHist = ggplot(data = credit, aes(credit$V2)) +
  # Añadimos un histograma con color de contorno naranja y relleno naranja claro (alpha = 0.2)
  # y definimos intervalos de 5 unidades usando el argumento breaks.
  geom_histogram(col = "orange", fill = "orange", alpha = 0.2, breaks = seq(0, 80, by = 5)) +
  # Agregamos un título al gráfico usando labs().
  labs(title = "Histograma para la variable V2 con línea de densidad")

# Añadimos una línea vertical en el histograma de ggplot para marcar la media de V2
# usando geom_vline() y especificando la posición con xintercept = mean().
myHist = myHist + geom_vline(xintercept = mean(credit$V2, na.rm = TRUE), col = "blue")

# Añadimos una línea vertical en el histograma de ggplot para marcar la mediana de V2
# usando geom_vline() y especificando la posición con xintercept = median().
myHist = myHist + geom_vline(xintercept = median(credit$V2, na.rm = TRUE), col = "red")

# Mostramos el gráfico final en ggplot2 con el histograma de V2,
# junto con las líneas de media (azul) y mediana (roja).
myHist





# Como vemos, V2 no sigue exactamente una distribución normal, ya que la media 
# y la mediana no son idénticas, lo que indica una posible asimetría en los datos.

# Para confirmar formalmente la falta de normalidad, realizaremos un gráfico Q-Q (Quantile-Quantile),
# que nos permite comparar la distribución de V2 con la distribución teórica normal.

# Cargamos la librería 'gridExtra' para facilitar la visualización si queremos combinar
# gráficos adicionales en una cuadrícula. (Aquí sólo la cargamos en caso de que queramos 
# usar varios gráficos más adelante en el análisis).
library(gridExtra)

# Creamos el gráfico Q-Q para V2 utilizando ggplot2.
p1 = ggplot(data = credit, aes(sample = V2)) +
  # Añadimos un título al gráfico Q-Q.
  ggtitle("QQ plot para V2") +
  # Añadimos los puntos del gráfico Q-Q, que compara los cuantiles de la muestra de V2 
  # con los cuantiles teóricos de una distribución normal.
  geom_qq() + 
  # Añadimos la línea Q-Q teórica (stat_qq_line), que muestra cómo se deberían 
  # alinear los puntos si la distribución de V2 fuera normal.
  stat_qq_line() + 
  # Etiquetas para los ejes: 'Distribución teórica' en el eje x y 'Distribución muestral' en el eje y.
  xlab("Distribución teórica") + 
  ylab("Distribución muestral")

# Mostramos el gráfico Q-Q, que permite ver si los puntos se alinean (lo que indicaría normalidad)
# o si se desvían de la línea (indicando falta de normalidad en la distribución de V2).
p1


# Como vemos hay una desviación de la diagonal. Por tanto, podemos
# concluir que no se trata de una distribución normal.


#------------------------------------------------------------------------------


# Comenzamos con el análisis de la variable categórica V16, 
# que contiene las clases objetivo o categorías para nuestro análisis.

# Resumen estadístico de V16, que muestra la frecuencia de cada categoría en la variable.
summary(credit$V16)

# Verificamos la estructura de V16 para confirmar que es de tipo factor y ver 
# las categorías presentes en la variable.
str(credit$V16)

# Calculamos el porcentaje de cada categoría en V16:
# Primero, usamos table(credit$V16) para obtener las frecuencias de cada categoría.
# Luego, aplicamos prop.table() para obtener la proporción relativa, multiplicando por 100 para obtener el porcentaje.
porcent <- prop.table(table(credit$V16)) * 100

# Creamos una tabla combinada que incluye tanto el número total (frecuencia) como el porcentaje de cada categoría:
# Usamos cbind() para unir el total (frecuencia) y el porcentaje en una tabla.
porcent_table <- cbind(total = table(credit$V16), porcentaje = porcent)

# Mostramos el vector de porcentajes, que nos permite ver el porcentaje de cada categoría.
porcent

# Ahora que entendemos la frecuencia y proporción de cada categoría,
# creamos un diagrama de sectores (o "quesos") para ilustrarlo de forma gráfica.

# Creamos el gráfico de sectores con pie(), donde usamos el vector de porcentajes.
# El argumento main establece el título del gráfico y col usa la función rainbow() para aplicar un color diferente
# a cada sector, igual al número de categorías en V16.
pie(porcent, main = "Diagrama de Quesos para V16", col = rainbow(length(porcent)))

# Con este gráfico de sectores, podemos ver visualmente la distribución de cada categoría en V16.
# Dado que las categorías tienen frecuencias similares, podemos observar que la distribución es aproximadamente uniforme,
# y no se identifican categorías con frecuencias extremas (outliers).

#----------------------------------------------------------------------------

# Analizamos la variable categórica V6 para obtener una visión general de su distribución y composición.

# Mostramos un resumen estadístico de V6, que nos indica la frecuencia de cada categoría en esta variable.
summary(credit$V6)

# Verificamos la estructura de V6 para confirmar que es una variable categórica (factor o carácter).
str(credit$V6)

# Calculamos el porcentaje de cada categoría en V6:
# Primero, obtenemos la frecuencia de cada categoría con table(credit$V6).
# Luego, usamos prop.table() para calcular la proporción relativa de cada categoría y multiplicamos por 100 para expresarlo en porcentaje.
porcent <- prop.table(table(credit$V6)) * 100

# Creamos una tabla que combina tanto el número total de observaciones (frecuencia) como el porcentaje de cada categoría:
# cbind() se usa para unir la frecuencia y el porcentaje en una tabla única.
porcent_table <- cbind(total = table(credit$V6), porcentaje = porcent)

# Mostramos el vector de porcentajes, lo que nos permite visualizar el porcentaje de cada categoría en V6.
porcent

# Ahora que entendemos la distribución de frecuencias y porcentajes de cada categoría, generamos un gráfico de sectores
# para visualizar estos porcentajes de manera gráfica y comparativa.

# Creamos el gráfico de sectores (o "quesos") usando la función pie(), pasándole el vector de porcentajes.
# Establecemos un título con el argumento main y aplicamos diferentes colores a cada categoría usando rainbow().
pie(porcent, main = "Diagrama de Quesos para V6", col = rainbow(length(porcent)))

# Mostramos nuevamente el vector de porcentajes para recordar los valores antes de proceder con el análisis de los valores NA.
porcent

# Calculamos la suma del resumen de V6 para confirmar el número total de observaciones, incluyendo posibles NA.
sum(summary(credit$V6))

# Calculamos el porcentaje de valores NA en la variable V6, asumiendo que hay 9 valores NA de un total de 690 observaciones.
porcentaje_na <- 9 / 690 * 100
porcentaje_na

# Concluimos que, siendo aproximadamente un 1.3% de valores faltantes (NA), la eliminación de estas observaciones es razonable,
# ya que este porcentaje es bajo y es poco probable que afecte significativamente el análisis.

#-------------------------------------------------------------------------------

# Vamos a analizar la variable V15, que parece tener una distribución con muchos valores cercanos a 0
# pero también valores atípicos elevados, lo que podría afectar la media.

# Realizamos un resumen estadístico de la variable V15 para obtener una visión general de los valores,
# incluyendo mínimos, máximos, media y mediana.
summary(credit$V15)

# Generamos un histograma básico para observar la distribución de V15.
# Usamos probability = TRUE para mostrar el histograma en términos de densidad.
hist(credit$V15, probability = TRUE,
     main = "Histograma de V15 con Curva de Densidad")

# Añadimos una curva de densidad a la gráfica para visualizar mejor la forma de la distribución,
# usando la función density() con na.rm = TRUE para excluir valores NA.
lines(density(credit$V15, na.rm = TRUE), col = "blue")

# Observamos que muchos valores altos de V15 hacen difícil ver los valores menores.
# Para solucionar esto, usamos ggplot2 para crear un histograma que enfoque la escala en valores menores,
# estableciendo los intervalos hasta 1000 (ajustable según el rango de interés).

# Creamos un histograma de V15 con ggplot2, aplicando intervalos de 10 en el rango de 0 a 1000.
myHist = ggplot(data = credit, aes(credit$V15)) +
  geom_histogram(col = "orange", fill = "orange", alpha = 0.2, breaks = seq(0, 1000, by = 10)) +
  labs(title = "Histograma para la variable V15 con línea de densidad")

# Añadimos una línea vertical azul en el histograma para indicar la media de V15,
# utilizando mean() y especificando que se ignoren valores NA.
myHist = myHist + geom_vline(xintercept = mean(credit$V15, na.rm = TRUE), col = "blue")

# Añadimos una línea vertical roja para indicar la mediana de V15, usando median().
myHist = myHist + geom_vline(xintercept = median(credit$V15, na.rm = TRUE), col = "red")

# Mostramos el gráfico final en ggplot2, que incluye el histograma de V15 y las líneas de media y mediana.
myHist

# Comentario sobre la distribución:
# Al observar el histograma, vemos que la mayoría de los valores están cerca de 0, lo que indica una
# acumulación de valores bajos. Esto se confirma con el resumen estadístico: la media es de 1017.4 
# y la mediana es 5.0, lo que significa que la mayoría de los valores son pequeños, 
# pero hay algunos valores muy altos (outliers) que elevan la media, creando una diferencia significativa
# entre media y mediana.



library(gridExtra)
p1 = ggplot(data=credit,aes(sample=V15)) +
  ggtitle("QQ plot para V15") +
  geom_qq() + 
  stat_qq_line() + 
  xlab("Distribución teórica") + ylab("Distribución muestral")


p1




# Multivariable
summary(credit$V15)

library(reshape2)
# creando una estructura de datos para usar lattice con facilidad con "melt()"
library(reshape2)
# creando una estructura de datos para usar lattice con facilidad con "melt()"


#----------------------------------------------------------------------------


melted_data <- melt(credit, id.vars = "V16", measure.vars = "V2", 
                    variable.name = "Variable", value.name = "Value")
melted_data

library(ggplot2)
ggplot(melted_data, aes(x = V16, y = Value)) +
  geom_boxplot() +
  xlab("Categoría de V16") +
  ylab("Valores de V2") +
  ggtitle("Distribución de V2 por Categoría de V16")


# Observando el boxplot podemos decir que ambos tienen una distribución
# simétrica. En cuanto a los outliners, no hay en exceso (en rechazada algo más).
# Ambas tienen un sesgo positivo, ya que los bigotes superiores tienen mayor
# longitud. 


library(ggplot2)

# Gráfico de densidad ajustado
ggplot(data = melted_data, aes(x = Value, color = V16, fill = V16)) +
  geom_density(alpha = 0.6) +
  scale_fill_discrete() +
  scale_color_discrete() +
  ylab("Densidad") +  # Etiqueta para el eje y
  xlab("Valores") +   # Etiqueta para el eje x
  ggtitle("Densidad de Valores por Especie")  # Título del gráfico

#El análisis de la distribución de V2 muestra que ambas categorías (rechazada y 
#aprobada) presentan una asimetría positiva, con picos de densidad más altos en 
#valores bajos y una cola extendida hacia la derecha. La curva de las solicitudes 
#rechazadas tiene una densidad más pronunciada cerca de los valores bajos (alrededor
#de 20), mientras que la curva de las solicitudes aprobadas es más dispersa y 
#se extiende hacia valores más altos, sugiriendo que las aprobaciones están 
#asociadas con un rango más amplio de valores.

#La superposición de las dos curvas, especialmente en el rango bajo de V2, 
#indica que esta variable por sí sola no es un fuerte discriminante entre 
#aprobaciones y rechazos. Sin embargo, a medida que los valores de V2 aumentan, 
#las aprobaciones se vuelven más frecuentes.

#En resumen, V2 parece influir en la decisión de crédito, pero debido a la 
#considerable superposición, podría requerir análisis adicionales junto con 
#otras variables para mejorar la capacidad predictiva.

# Boxplot chetados con puntos

library(ggplot2)
ggplot(melted_data, aes(x = V16, y = Value, color=V16, fill=V16)) +
  geom_boxplot(alpha=0.6) +
  geom_jitter(color="black") +
  scale_fill_discrete() +
  scale_color_discrete() +
  xlab("Categoría de V16") +
  ylab("Valores de V2") +
  ggtitle("Distribución de V2 por Categoría de V16")



##########################################
##########################################
##########################################
##########################################
##########################################
##########################################
# MUTIVARIABLE V16 V9

melted_data <- melt(credit, id.vars = "V16", measure.vars = "V9", 
                    variable.name = "Variable", value.name = "Value")
melted_data

library(ggplot2)
ggplot(melted_data, aes(x = V16, y = Value)) +
  geom_boxplot() +
  xlab("Categoría de V16") +
  ylab("Valores de V2") +
  ggtitle("Distribución de V2 por Categoría de V16")


# Observando el boxplot podemos decir que ambos tienen una distribución
# simétrica. En cuanto a los outliners, no hay en exceso (en rechazada algo más).
# Ambas tienen un sesgo positivo, ya que los bigotes superiores tienen mayor
# longitud. 


library(ggplot2)

# Gráfico de densidad ajustado
ggplot(data = melted_data, aes(x = Value, color = V16, fill = V16)) +
  geom_density(alpha = 0.6) +
  scale_fill_discrete() +
  scale_color_discrete() +
  ylab("Densidad") +  # Etiqueta para el eje y
  xlab("Valores") +   # Etiqueta para el eje x
  ggtitle("Densidad de Valores por Especie")  # Título del gráfico

#------------------------------------------------------------------------------
###########################################################################
###########################################################################
# Limpiamos datos

colnames(credit)[colnames(credit) == "V1"] <- "Genero"
colnames(credit)[colnames(credit) == "V2"] <- "Edad"
colnames(credit)[colnames(credit) == "V3"] <- "Deuda"
colnames(credit)[colnames(credit) == "V4"] <- "EstadoCivil"
colnames(credit)[colnames(credit) == "V8"] <- "AnyosContratado"
colnames(credit)[colnames(credit) == "V10"] <- "Empleado"
colnames(credit)[colnames(credit) == "V11"] <- "Solvencia"
colnames(credit)[colnames(credit) == "V13"] <- "composicionPoblacion"


# 1. Tratamiento de Datos Nulos

# Ignorar registros incompletos:

credit_sin_na <- na.omit(credit) # Crea una copia sin filas con NA
nrow(credit_sin_na)


# Rellenar con una constante
credit_constante <- credit
levels(credit_constante$Genero) <- c(levels(credit_constante$Genero), "unknown")
credit_constante$Genero[is.na(credit_constante$Genero)] <- "unknown"
nrow(credit_constante)

# Razonable?
sum(credit_constante$Genero == "unknown")
sum(credit_constante$Genero == "unknown")/nrow(credit_constante)*100


# Imputar con la media de la columna
credit_media <- credit # Crea una copia del dataframe
sum(is.na(credit_media$Edad))
credit_media$Edad[is.na(credit_media$Edad)] <- mean(credit$Edad, na.rm = TRUE)
sum(is.na(credit_media$Edad))

# Cabe destacar que la imputación con la media o la mediana depende de si la distribuión
# es simétrica. Vamos a ver si es:

hist(credit$Edad, main = "Distribución de V2", xlab = "Edad", col = "lightblue", breaks = 20)
boxplot(credit$Edad, main = "Boxplot de V2", col = "lightgreen")

# No es simétrica, por tanto, es más interesante usar la mediana:

# Por tanto deberíamos de usar la mediana:
credit_mediana <- credit
sum(is.na(credit_mediana$Edad))
credit_mediana$Edad[is.na(credit_mediana$Edad)] <- median(credit$Edad, na.rm = TRUE)
sum(is.na(credit_mediana$Edad))


###################################################################
###################################################################
# He pensado que podemos hacer una criba en base al número de NA, voy 
# a calcular todos por columnas por si acaso:
numero_na_por_columnas <- colSums(is.na(credit))
numero_na_por_columnas

# Solo hace falta tratar las siguientes variables:
# Genero: 12
# Edad: 12
# EstadoCivil: 6
# V5: 6
# V6: 9
# V7: 9
# V14: 13

#V1 (a, b), incorrecto (hay nulos)
#v2 continuous,incorrecto (hay nulos)
#v3 continuous, Ok
#v4 (u, y, l, t), incorrecto resultado("?" "l" "u" "y")
#v5 g, p, gg), incorrecto resultado("?"  "g"  "gg" "p" )
#v6 (c, d, cc, i, j, k, m, r, q, w, x, e, aa, ff), incorrecto (hay nulos)
#v7 v, h, bb, j, n, z, dd, ff, o), incorrecto (hay nulos)
#v8 continuous, Ok
#v9 (t, f), Ok
#v10 (t, f), Ok
#v11 continuous, Ok
#v12 (t, f), Ok
#v13 (g, p, s), Ok
#v14 continuous, incorrectos (hay nulos)
#v15 continuous, Ok
#v16 (rechazada, aprobada), Ok


# Para las categéroricas en caso de que haya alguna que se repita basto, 
# podemos poner esa.

# Parece que para muchas variables categóricas, al haber tan pocos NA, no tiene
# mucho sentido imputar

summary(credit$V5)

frecuencias <- table(credit$V5)

barplot(frecuencias, 
        main = "Distribución de credit$V5", 
        xlab = "Categorías de V5", 
        ylab = "Frecuencia", 
        col = "lightblue", 
        border = "black")

prop.table(summary(credit$V5))

sum(is.na(credit$V5))
credit$V5[is.na(credit$V5)] <- "g"
sum(is.na(credit$V5))

# Verificamos V6
frecuencias <- table(credit$V6)

barplot(frecuencias, 
        main = "Distribución de credit$V6", 
        xlab = "Categorías de V6", 
        ylab = "Frecuencia", 
        col = "lightblue", 
        border = "black")

# Verificamos V7
frecuencias <- table(credit$V7)

barplot(frecuencias, 
        main = "Distribución de credit$V7", 
        xlab = "Categorías de V5", 
        ylab = "Frecuencia", 
        col = "lightblue", 
        border = "black")


# Imputamos "Estado Civil" y V7:

sum(is.na(credit$EstadoCivil))
credit$EstadoCivil[is.na(credit$EstadoCivil)] <- "u"
sum(is.na(credit$EstadoCivil))

sum(is.na(credit$V7))
credit$V7[is.na(credit$V7)] <- "v"
sum(is.na(credit$V7))


# Para V6 hacemos imputación aleatoria recomendada:

set.seed(123) # Para reproducibilidad
categorias <- names(table(credit$V6))
probabilidades <- prop.table(table(credit$V6))

# Imputar valores NA
credit$V6[is.na(credit$V6)] <- sample(categorias, size = sum(is.na(credit$V6)), replace = TRUE, prob = probabilidades)
sum(is.na(credit$V6))


# Procedemos al tratamiento de outliers y nulos
# hacemos summary grande para ver qué pinta tienen las variables y decidir:

summary(credit)

# En primera instancia lo que vamos a hacer es eliminar las filas correspondientes
# a las personas que tienen menos de 18 años. Esto son valores claramente icorrectos
# que no aportan nada bueno.

# Eliminar filas con Edad < 18
credit <- credit[credit$Edad >= 18, ]
credit <- na.omit(credit)

# Analizamos ahora Deuda
hist(credit$Deuda, xlab="", main="Máximo valor de Deuda", probability=T)
lines(density(credit$Deuda,na.rm=T))
rug(jitter(credit$Deuda))

boxplot(credit$Deuda,boxwex=0.15,ylab="Máximo valor de Deuda")
rug(jitter(credit$Deuda),side=2)
abline(h=mean(credit$Deuda,na.rm=T),lty=2)

# Seguro que los datos de Deuda han sufrido una transformación para que no se
# pueda hacer ningún estudio que saque conclusiones que ponga en peligro la 
# confidencialidad de datos. Por tanto, no veo buena opción 

# Analizamos ahora V15
hist(credit$V15, xlab="", main="Máximo valor de V15", probability=T)
lines(density(credit$V15,na.rm=T))
rug(jitter(credit$V15))

boxplot(credit$V15,boxwex=0.15,ylab="Máximo valor de V15")
rug(jitter(credit$V15),side=2)
abline(h=mean(credit$V15,na.rm=T),lty=2)

# Parece que los valores extremos son válidos pero afectan la distribución,
# aplicamos una transformación que reduzca el impacto de los outliers:
# Decidimos que debido a la diferencia de valores una transformación logaritmica
# es muy interesante. ¿Qué hace?
# Toma el logaritmo de cada valor, lo que comprime las diferencias entre los valores altos y bajos.
# Los valores extremadamente grandes (outliers) son mucho más reducidos en comparación con los valores pequeños, lo que mitiga la asimetría y el sesgo hacia la derecha.

credit$V15 <- log(credit$V15 + 1) # Suma 1 para evitar log(0)

# Verificamos que se ha hecho la transformación:
summary(credit$V15)
hist(credit$V15, main = "Distribución de V15 Transformada (Logarítmica)", probability = TRUE)

# ________________________________________________________________________________
# ________________________________________________________________________________
# ________________________________________________________________________________
# ________________________________________________________________________________
# Sin embargo, no estoy seguro de que sea la mejor opción. Voy a dejar comentadas
# otras técnicas que pueden ser de gran interés:

# Aplicar Winsorización a la columna V15:
# Instalar y cargar el paquete DescTools para Winsorización
if (!require("DescTools")) install.packages("DescTools")
library(DescTools)

# Aplicar winsorización al percentil 5 y 95
#credit$V15_wins <- Winsorize(credit$V15, probs = c(0.05, 0.95))

# Verificar los resultados
#summary(credit$V15_wins)

# Aplicar k-means clustering para ajustar outliers en V15 en R
# Escalar los datos para mejorar el rendimiento de k-means
#credit$V15_scaled <- scale(credit$V15)

# Aplicar k-means clustering (2 grupos en este caso)
#set.seed(42) # Asegura reproducibilidad
#kmeans_model <- kmeans(credit$V15_scaled, centers = 2, nstart = 10)

# Asignar los clusters al dataset
#credit$Cluster <- kmeans_model$cluster

# Calcular las medias de cada cluster
#cluster_means <- tapply(credit$V15, credit$Cluster, mean)

# Ajustar los valores de V15 que sean extremos en su grupo
#credit$V15_cluster_adjusted <- ifelse(
#  credit$V15 > cluster_means[credit$Cluster],
#  cluster_means[credit$Cluster],
#  credit$V15
#)

# Verificar resultados
#head(credit)




# ________________________________________________________________
# Me gustaría destacar que la elaboración de valores nulos en la segunda pasada
# hay que probar:
# 6.2.3 Sustituci´on mediante estudio de correlaciones
#6.2.4 Sustituci´on de variables num´ericas mediante preProcess de caret (clustering)

# Lo siguiente que vamos a hacer es eliminar la categoría "t" de EstadoCivil
# ya que lo único que hace es complicar las cosas, ya que está vacía.



summary(credit)
# Eliminar niveles sin observaciones
credit$EstadoCivil <- droplevels(credit$EstadoCivil)
summary(credit)



# Partimos los datos en Entrenamiento y testing:




















































