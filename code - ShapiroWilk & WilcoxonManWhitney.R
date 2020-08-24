#install.packages('coin')
#install.packages("dplyr")
#install.packages("ggpubr")

require(coin)
library("dplyr")
library("ggpubr")


setwd("/Users/benjamincarcamo/Documents/Magister/Analisis notas finales")

my_data <- read.table("data2.txt", header=TRUE)

# Realizamos el test de SW
my_data <- ToothGrowth
shapiro.test(my_data$x)
shapiro.test(my_data$y)


#####
# Como ambos conjuntos no distribuyen normal, se realiza el test de WMW

data <- read.table("data2.txt", header=TRUE)

muestraX <- data$x
muestraY <- data$y

valores = c(muestraX, muestraY)
grupo = rep(c("A", "B"), c(27, 27))
datos <- data.frame(grupo = rep(c("A", "B"), c(27, 27)), valores = c(muestraX, muestraY))

# Realizando prueba de WMW
wilcox_test(valores ~ grupo, data = datos, distribution = "exact", conf.int=0.95)

# Obteniendo el tamaño del efecto
tamanyo_efecto <- 0.16464/sqrt(27 + 27)
tamanyo_efecto


