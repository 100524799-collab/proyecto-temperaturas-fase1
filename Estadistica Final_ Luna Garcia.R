# instalar packages
library(readr)      
library(dplyr)     
library(DescTools)  
 
#  1. IMPORTACIÓN
setwd("~/Desktop/Estadistica")
dir.create("figuras", showWarnings = FALSE)    # gráficos del informe
dir.create("tablas",  showWarnings = FALSE)    # tablas en CSV

glob <- read_csv("GlobalTemperatures.csv", show_col_types = FALSE)

str(glob)
summary(glob)
head(glob)
dim(glob)    

glob$dt <- as.Date(glob$dt) #Convertir texto a fecha
range(glob$dt)     

# 2. NUEVAS VARIABLES 
glob <- glob %>%
  mutate(anio = as.numeric(format(dt, "%Y")),
         mes  = as.numeric(format(dt, "%m")),
         estacion = ifelse(mes %in% c(12, 1, 2), "Invierno",
                           ifelse(mes %in% c(3, 4, 5),  "Primavera",
                                  ifelse(mes %in% c(6, 7, 8),  "Verano", "Otoño"))),
         periodo = ifelse(anio < 1850, "1750-1849", "1850-2015"))

glob$estacion <- factor(glob$estacion, levels = c("Invierno", "Primavera", "Verano", "Otoño"))
glob$periodo  <- factor(glob$periodo)

# 3. CALIDAD DE DATOS
# Datos ausentes (NA)
Abstract(glob)                    # resumen de NA
colSums(is.na(glob))              # NA por variable

png("figuras/datos_ausentes.png", width = 2200, height = 1200, res = 200)
PlotMiss(glob, main = "Datos ausentes por variable")
dev.off()

#¿Cuándo faltan los datos?
glob %>% filter(is.na(LandAverageTemperature)) %>% select(dt, LandAverageTemperature)
glob %>% filter(is.na(LandMaxTemperature)) %>% summarize(desde = min(anio), hasta = max(anio))

# Tratamiento:
glob_noNA <- glob[!is.na(glob$LandAverageTemperature), ]
nrow(glob_noNA)                      

# Estimar los NA con la media
glob2 <- glob
glob2$LandAverageTemperature[is.na(glob2$LandAverageTemperature)] <-
  mean(glob2$LandAverageTemperature, na.rm = TRUE)

mean(glob_noNA$LandAverageTemperature)   # media sin los 12 NA
mean(glob2$LandAverageTemperature)       # media tras imputar
sd(glob_noNA$LandAverageTemperature)     # desviación típica sin los 12 NA
sd(glob2$LandAverageTemperature)         # desviación típica tras imputar

# Duplicados
glob[duplicated(glob), ]                       
sum(duplicated(glob))                              
sum(duplicated(glob$dt))           

# Consistencia: mínima <= media <= máxima
sum(glob$LandMinTemperature > glob$LandAverageTemperature, na.rm = TRUE)   
sum(glob$LandAverageTemperature > glob$LandMaxTemperature, na.rm = TRUE)  
sum(glob$LandAverageTemperatureUncertainty <= 0, na.rm = TRUE)            

# Valores atípicos
vars <- c("LandAverageTemperature", "LandMaxTemperature", "LandMinTemperature",
          "LandAndOceanAverageTemperature", "LandAverageTemperatureUncertainty")

# box-plot 
Outlier(glob_noNA$LandAverageTemperatureUncertainty, method = "boxplot")
length(Outlier(glob_noNA$LandAverageTemperatureUncertainty, method = "boxplot")) 
range(atipicos_box$anio)

# Registros completos de esos atípicos
atipicos_box <- glob_noNA[Outlier(glob_noNA$LandAverageTemperatureUncertainty,
                                  method = "boxplot", value = FALSE), ]
head(atipicos_box[, c("dt", "LandAverageTemperatureUncertainty")])

# Método z-score 
atipicos_z <- glob_noNA[abs(scale(glob_noNA$LandAverageTemperatureUncertainty)) > 3, ]
nrow(atipicos_z)                                  
range(atipicos_z$anio) # ¿en qué años caen?


# Las temperaturas:
sapply(glob_noNA[, vars[1:4]], function(x) length(Outlier(na.omit(x), method = "boxplot")))
sapply(glob_noNA[, vars[1:4]], function(x) sum(abs(scale(na.omit(x))) > 3))

# Decisión: se mantienen los atípicos de la incertidumbre (no son errores; corresponden a los siglos XVIII-XIX, con pocas estaciones de medida)

png("figuras/boxplot_incertidumbre.png", width = 1600, height = 900, res = 200)
boxplot(glob_noNA$LandAverageTemperatureUncertainty, horizontal = TRUE,
        main = "Incertidumbre de la temperatura media terrestre",
        xlab = "Incertidumbre (°C)", col = "lightblue")
dev.off()

#Comprobación
sum(duplicated(glob))                    
sum(glob$LandMinTemperature > glob$LandAverageTemperature, na.rm = TRUE)   
sum(glob$LandAverageTemperature > glob$LandMaxTemperature, na.rm = TRUE) 

# 4.ANÁLISIS UNIVARIANTE
# Variables cualitativas: estación y periodo 
# Tabla de frecuencias (absoluta, relativa y acumuladas)
tabla_cual <- function(x) {
  ni <- table(x)
  data.frame(Clase = names(ni),
             ni    = as.numeric(ni),
             fi    = round(as.numeric(prop.table(ni)), 4),
             Ni    = cumsum(as.numeric(ni)),
             Fi    = round(cumsum(as.numeric(prop.table(ni))), 4))
}
t_estacion <- tabla_cual(glob_noNA$estacion)
t_periodo  <- tabla_cual(glob_noNA$periodo)
t_estacion
t_periodo
write.csv(t_estacion, "tablas/frec_estacion.csv", row.names = FALSE)
write.csv(t_periodo,  "tablas/frec_periodo.csv",  row.names = FALSE)

# Diagrama de barras
png("figuras/barras_estacion.png", width = 1600, height = 1000, res = 200)
barplot(table(glob_noNA$estacion), main = "Observaciones por estación del año",
        xlab = "Estación", ylab = "Frecuencia absoluta", col = "steelblue",
        ylim = c(0, 900))
dev.off()

# Diagrama de sectores
png("figuras/sectores_periodo.png", width = 1600, height = 1000, res = 200)
pie(table(glob_noNA$periodo),
    labels = paste0(names(table(glob_noNA$periodo)), " (",
                    round(100 * prop.table(table(glob_noNA$periodo)), 1), " %)"),
    main = "Observaciones por periodo", col = c("lightblue", "steelblue"))
dev.off()

# Variables cuantitativas: tabla de frecuencias por intervalos 
x <- glob_noNA$LandAverageTemperature
intervalos <- cut(x, breaks = seq(-4, 20, by = 2), right = FALSE)
ni <- table(intervalos)
t_media <- data.frame(Intervalo = names(ni),
                      ni = as.numeric(ni),
                      Ni = cumsum(as.numeric(ni)),
                      fi = round(as.numeric(prop.table(ni)), 4),
                      Fi = round(cumsum(as.numeric(prop.table(ni))), 4))
t_media
write.csv(t_media, "tablas/frec_temperatura_media.csv", row.names = FALSE)

# Histograma y polígono de frecuencias

# Temperatura media
png("figuras/hist_media.png", width = 1600, height = 1000, res = 200)
h <- hist(glob_noNA$LandAverageTemperature, main = "Temperatura media terrestre",
          xlab = "Temperatura (°C)", ylab = "Frecuencia absoluta", col = "lightblue")
lines(h$mids, h$counts, type = "b", col = "darkred", pch = 16)
dev.off()

# Temperatura máxima
png("figuras/hist_maxima.png", width = 1600, height = 1000, res = 200)
h <- hist(glob_noNA$LandMaxTemperature, main = "Temperatura máxima terrestre",
          xlab = "Temperatura (°C)", ylab = "Frecuencia absoluta", col = "lightblue")
lines(h$mids, h$counts, type = "b", col = "darkred", pch = 16)
dev.off()

# Temperatura mínima
png("figuras/hist_minima.png", width = 1600, height = 1000, res = 200)
h <- hist(glob_noNA$LandMinTemperature, main = "Temperatura mínima terrestre",
          xlab = "Temperatura (°C)", ylab = "Frecuencia absoluta", col = "lightblue")
lines(h$mids, h$counts, type = "b", col = "darkred", pch = 16)
dev.off()

# Temperatura media tierra y océano
png("figuras/hist_oceano.png", width = 1600, height = 1000, res = 200)
h <- hist(glob_noNA$LandAndOceanAverageTemperature, main = "Temperatura media tierra y océano",
          xlab = "Temperatura (°C)", ylab = "Frecuencia absoluta", col = "lightblue")
lines(h$mids, h$counts, type = "b", col = "darkred", pch = 16)
dev.off()

# Incertidumbre
png("figuras/hist_incertidumbre.png", width = 1600, height = 1000, res = 200)
h <- hist(glob_noNA$LandAverageTemperatureUncertainty, main = "Incertidumbre de la temperatura media",
          xlab = "Incertidumbre (°C)", ylab = "Frecuencia absoluta", col = "lightblue")
lines(h$mids, h$counts, type = "b", col = "darkred", pch = 16)
dev.off()

# Diagramas de caja
png("figuras/boxplot_temperaturas.png", width = 1600, height = 1000, res = 200)
boxplot(glob_noNA[, c("LandAverageTemperature", "LandMaxTemperature",
                      "LandMinTemperature", "LandAndOceanAverageTemperature")],
        names = c("Media", "Máxima", "Mínima", "Tierra y océano"),
        main = "Comparación de las temperaturas", ylab = "Temperatura (°C)",
        col = "lightblue")
dev.off()

# Resumen numérico
resumen <- function(x) {
  x <- na.omit(x)
  c(n = length(x),
    Media = mean(x), Mediana = median(x),
    Q1 = unname(quantile(x, 0.25)), Q3 = unname(quantile(x, 0.75)),
    Mínimo = min(x), Máximo = max(x),
    Rango = diff(range(x)), RIC = IQR(x),
    Desv_típica = sd(x), Varianza = var(x),
    CV = sd(x) / abs(mean(x)),
    Asimetría = Skew(x), Curtosis = Kurt(x))
}
tabla_desc <- t(sapply(glob_noNA[, vars], resumen))
round(tabla_desc, 2)
write.csv(round(tabla_desc, 2), "tablas/medidas_descriptivas.csv")

# Valores de referencia: media (LandAverageTemperature) 8.37, mediana 8.61, desv. típica 4.38;
# LandMax 14.35 (n = 1992); LandMin 2.74; LandAndOcean 15.21 (desv. 1.27);
# Incertidumbre: media 0.94, mediana 0.39, desv. 1.10.

# Medias por grupos (dplyr) 
glob_noNA %>% group_by(estacion) %>%
  summarize(n = n(), media = mean(LandAverageTemperature), desv = sd(LandAverageTemperature))

glob_noNA %>% group_by(periodo) %>%
  summarize(n = n(), media = mean(LandAverageTemperature),
            incertidumbre_media = mean(LandAverageTemperatureUncertainty))

# Media anual: solo años completos (12 meses), para que sea comparable
media_anual <- glob_noNA %>% group_by(anio) %>%
  summarize(n = n(), media = mean(LandAverageTemperature)) %>%
  filter(n == 12) %>%
  arrange(anio)

nrow(media_anual)               
range(media_anual$anio)          

png("figuras/serie_anual.png", width = 1600, height = 1000, res = 200)
plot(media_anual$anio, media_anual$media, type = "l", col = "steelblue",
     main = "Temperatura media terrestre anual", xlab = "Año",
     ylab = "Temperatura media (°C)")
dev.off()

# 5. ANÁLISIS BIVARIANTE
# Cuantitativa vs Cuantitativa: Correlaciones y Dispersión. Seleccionamos solo las variables térmicas continuas 
vars_termicas <- glob_noNA[, c("LandAverageTemperature", "LandMaxTemperature", "LandMinTemperature")]

# Matriz de correlación de Pearson
matriz_cor <- cor(vars_termicas, use = "complete.obs", method = "pearson")
write.csv(round(matriz_cor, 4), "tablas/matriz_correlacion.csv")

# Gráfico de dispersión múltiple para ver la relación lineal entre temperaturas
png("figuras/dispersion_temperaturas.png", width = 1800, height = 1800, res = 200)
pairs(vars_termicas, 
      main = "Matriz de dispersión: Relación entre mediciones térmicas",
      col = "steelblue", pch = 16, cex = 0.5)
dev.off()

# Dispersión y Regresión Lineal Simple: Temperatura Media Anual vs Año
png("figuras/dispersion_tendencia_anual.png", width = 1800, height = 1200, res = 200)
plot(media_anual$anio, media_anual$media, 
     main = "Tendencia Histórica de la Temperatura Media Terrestre",
     xlab = "Año", ylab = "Temperatura Media Anual (°C)",
     col = "darkgray", pch = 16)

#recta de regresión lineal (tendencia)
modelo_tendencia <- lm(media ~ anio, data = media_anual)
abline(modelo_tendencia, col = "darkred", lwd = 2)
dev.off()

# R-cuadrado y pendiente 
resumen_modelo <- summary(modelo_tendencia)
sink("tablas/resumen_regresion_tendencia.txt")
print(resumen_modelo)
sink()

# Cualitativa vs Cuantitativa: Comparativas por grupos
# Temperatura media por Periodo Histórico
png("figuras/boxplot_periodo_temp.png", width = 1600, height = 1000, res = 200)
boxplot(LandAverageTemperature ~ periodo, data = glob_noNA,
        main = "Distribución de la Temperatura Media por Periodo Histórico",
        xlab = "Periodo", ylab = "Temperatura Media (°C)",
        col = c("lightblue", "lightcoral"))
dev.off()

# Temperatura media por Estación del Año
png("figuras/boxplot_estacion_temp.png", width = 1600, height = 1000, res = 200)
boxplot(LandAverageTemperature ~ estacion, data = glob_noNA,
        main = "Variación Térmica Estacional",
        xlab = "Estación", ylab = "Temperatura Media (°C)",
        col = c("aliceblue", "lightgreen", "khaki", "tan"))
dev.off()

# Cualitativa vs Cualitativa
tabla_contingencia <- table(glob_noNA$estacion, glob_noNA$periodo)

# Frecuencias conjuntas (absolutas)
write.csv(as.data.frame(unclass(tabla_contingencia)), "tablas/contingencia_absoluta.csv")

# Frecuencias relativas condicionadas por columna (Periodo)
tabla_condicionada <- prop.table(tabla_contingencia, margin = 2)
write.csv(as.data.frame(unclass(round(tabla_condicionada, 4))), "tablas/contingencia_condicionada.csv")

# Gráfico de barras apiladas cruzando Estación y Periodo
png("figuras/barras_apiladas_estacion_periodo.png", width = 1600, height = 1000, res = 200)
barplot(tabla_contingencia, 
        main = "Distribución Estacional de Mediciones por Periodo",
        xlab = "Periodo Histórico", ylab = "Número de Observaciones",
        col = c("aliceblue", "lightgreen", "khaki", "tan"),
        legend = rownames(tabla_contingencia), 
        args.legend = list(x = "topleft", cex = 0.8))
dev.off()