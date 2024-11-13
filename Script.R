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

