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

# Factorizamos todas las categorias ya que nos lo dan como un vector
credit <- data.frame(lapply(credit,FUN=as.factor))


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




# Análisis monovariable
summary(credit)
# Decidimos que es interesante analizar V2 ya que su media y mediana son
# parecidas y seguramente sea algo simétrica (distribución normal)
hist(credit$V2, probability = TRUE, main = "Histograma de V2 con Curva de Densidad")
lines(density(credit$V2, na.rm = TRUE), col = "blue")

myHist = ggplot(data=credit, aes(credit$V2)) +
          geom_histogram(col="orange", fill="orange", alpha=0.2,
                         breaks=seq(0,80,by=5)) +
          labs(title="Histograma para la variable V2 con linea de densisdad")


myHist = myHist + geom_vline(xintercept = mean(credit$V2, na.rm = TRUE), col = "blue")
myHist = myHist + geom_vline(xintercept = median(credit$V2, na.rm = TRUE), col = "red")



myHist
# Como vemosV2 no sigue exactamente una normal, la media y la mediana no
# no se parecen realmente.

# Para confirmar formalmente la falta de normalidad:

# Plot Q-Q
library(gridExtra)
p1 = ggplot(data=credit,aes(sample=V2)) +
  ggtitle("QQ plot para V2") +
  geom_qq() + 
  stat_qq_line() + 
  xlab("Distribución teórica") + ylab("Distribución muestral")


p1

# Como vemos hay una desviación de la diagonal. Por tanto, podemos
# concluir que no se trata de una distribución normal.







# Comenzamos con el análsis de la variable V16
summary(credit$V16)
str(credit$V16)

porcent <- prop.table(table(credit$V16))*100
porcent_table <- cbind(total=table(credit$V16), porcentaje=porcent)
porcent

# Ahora que nos hacemos una idea del porcentaje total de cada una, vamos
# a hacer una gráfica de barras para ilustrarlo mejor:

pie(porcent, main = "Diagrama de Quesos para A16", col = rainbow(length(porcent)))

# Como las categorías tienen frecuencias similares, la distribución es uniforme.
# Identificamos que no hay categorías outliners. 



# Analizamos la variable categórica V6
summary(credit$V6)
str(credit$V6)

porcent <- prop.table(table(credit$V6))*100
porcent_table <- cbind(total=table(credit$V6), porcentaje=porcent)
porcent

# Ahora que nos hacemos una idea del porcentaje total de cada una, vamos
# a hacer una gráfica de barras para ilustrarlo mejor:

pie(porcent, main = "Diagrama de Quesos para A6", col = rainbow(length(porcent)))

porcent
sum(summary(credit$V6))
porcentaje_na = 9/690 * 100
porcentaje_na

# Siendo realistas la emininación de un 1.3% de NA's es razonable.


# Vamos a analizar V11:
# Análisis monovariable
summary(credit$V15)
# Decidimos que es interesante analizar V2 ya que su media y mediana son
# parecidas y seguramente sea algo simétrica (distribución normal)
hist(credit$V15, probability = TRUE,
              main = "Histograma de V15 con Curva de Densidad")
lines(density(credit$V15, na.rm = TRUE), col = "blue")

# Para que se vea mejor, vamos a recortar la mayoría de los 
# valores altos para poder al menos ver los pequeños
myHist = ggplot(data=credit, aes(credit$V15)) +
  geom_histogram(col="orange", fill="orange", alpha=0.2,
                 breaks=seq(0,1000,by=10)) +
  labs(title="Histograma para la variable V15 con linea de densisdad")


myHist = myHist + geom_vline(xintercept = mean(credit$V15, na.rm = TRUE), col = "blue")
myHist = myHist + geom_vline(xintercept = median(credit$V15, na.rm = TRUE), col = "red")
myHist


myHist
# Como podemos ver la mayoría de los valores están cercanos al 0, hecho que podemos
# intuir viendo que la media es 1017.4 y la mediana es 5.0. Esto es porque la mayoría
# de los valores son 0, pero como luego hay valores muy altos (pocos) la media se
# dispara. Haciendo que sean muy distintos.


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
