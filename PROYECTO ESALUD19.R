
install.packages('viridis')

# Instalar y cargar librerías necesarias

library(tidyverse)
library(readxl)
library(ggplot2)
library(reshape2)
library(ggcorrplot)
library(rpart)
library(plotly)
library(gtsummary)
library(viridis)

# Cargar los datos desde el archivo Excel
datos <- read_excel("C:/DATOS/ESalud19.xlsx", sheet = "ESalud19")

# Eliminar columnas no deseadas
datos <- datos %>% select(-c(N_EDU, PANTORRILLA, FRUTA, PAD1, PAD2, PRES1, PRES2, PAS1, PAS2, FEXP))


# Convertir las columnas específicas a numéricas
datos <- datos %>% mutate(
  PESO = as.numeric(PESO),
  ESTATURA = as.numeric(ESTATURA),
  CINTURA = as.numeric(CINTURA),
  IMC = as.numeric(IMC),
  SODIO_ORINA = as.numeric(SODIO_ORINA),
  GLUCOSA = as.numeric(GLUCOSA),
  HDL = as.numeric(HDL),
  LDL = as.numeric(LDL),
  COLESTEROL = as.numeric(COLESTEROL),
  PAS3 = as.numeric(PAS3),
  PAD3 = as.numeric(PAD3),
  PRES3 = as.numeric(PRES3)
)

# Eliminar filas donde SINT_DEPRESION sea exactamente "NA" como texto
datos <- datos %>% filter(SINT_DEPRESION != "NA")

# Eliminar filas donde E_CIVIL sea exactamente "NA" como texto
datos <- datos %>% filter(E_CIVIL != "NA")


# Eliminar filas donde DIABETES sea "NO RECUERDO"
datos <- datos %>% filter(DIABETES != "NO RECUERDO")

# Asegurar que CANSANCIO es un factor
datos$CANSANCIO <- as.factor(datos$CANSANCIO)

# Convertir 'NA' como texto en NA reales
datos$CANSANCIO[datos$CANSANCIO == "NA"] <- NA

# Entrenar el modelo solo con filas sin NA en CANSANCIO
modelo_cansancio <- rpart(CANSANCIO ~ EDAD + PESO + IMC + GLUCOSA, 
                          data = datos, method = "class", na.action = na.omit)

# Filtrar filas con NA en CANSANCIO
na_rows <- is.na(datos$CANSANCIO)

# Predecir los valores faltantes
predicciones <- predict(modelo_cansancio, newdata = datos[na_rows, ], type = "class")

# Asignar las predicciones a las filas con NA
datos$CANSANCIO[na_rows] <- as.character(predicciones)

# Convertir de nuevo a factor
datos$CANSANCIO <- as.factor(datos$CANSANCIO)

# Verificar los valores únicos
table(datos$CANSANCIO)

# Eliminar filas con valores NA
datos <- na.omit(datos)

# Convertir variables categóricas en factores
datos$SEXO <- as.factor(datos$SEXO)
datos$E_CIVIL <- as.factor(datos$E_CIVIL)
datos$CANSANCIO <- as.factor(datos$CANSANCIO)
datos$DIABETES <- as.factor(datos$DIABETES)  # Suponiendo que existe esta variable

# Convertir las variables categóricas a formato largo
datos_largos <- datos %>%
  select_if(is.factor) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Valor")

# Obtener lista de variables categóricas únicas
variables_categoricas <- unique(datos_largos$Variable)

# Iterar sobre cada variable y graficar individualmente
for (var in variables_categoricas) {
  p <- ggplot(datos_largos %>% filter(Variable == var), aes(x = Valor, fill = Valor)) +
    geom_bar() +
    scale_fill_viridis_d(option = "plasma") +  # Usa una paleta con más variedad de colores
    theme_minimal() +
    theme(legend.position = "none") +  # Ocultar la leyenda para que no se repita
    labs(title = paste("Histograma de", var), x = "Categorías", y = "Frecuencia")
  
  print(p)  # Mostrar el gráfico
  
  readline(prompt = "Presiona Enter para continuar...")  # Espera hasta que el usuario presione Enter
}


# Gráficos de barras comparando diabetes con otras variables
g1 <- ggplot(datos, aes(x = SEXO, fill = DIABETES)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Casos de Diabetes por Sexo", x = "Sexo", y = "Frecuencia") + 
  theme_minimal()

g2 <- ggplot(datos, aes(x = E_CIVIL, fill = DIABETES)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Casos de Diabetes por Estado Civil", x = "Estado Civil", y = "Frecuencia") + 
  theme_minimal() + 
  coord_flip()

g3 <- ggplot(datos, aes(x = CANSANCIO, fill = DIABETES)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Casos de Diabetes por Cansancio", x = "Cansancio", y = "Frecuencia") + 
  theme_minimal()

# Mostrar los gráficos
g1
g2
g3

#Variables numericas
numericas <- datos %>% select_if(is.numeric)
#Graficos de Caja de cada Variable Numerica
for (var in colnames(numericas)) {
  boxplot(numericas[[var]], main = paste("Boxplot de", var), col = "blue")
  locator(1)  # Espera un clic antes de mostrar el siguiente gráfico
}

# ANALISIS DE NORMALIDAD:


# Aplicar el test de Shapiro-Wilk a cada variable numérica
for (var in colnames(numericas)) {
  resultado <- shapiro.test(numericas[[var]])
  
  cat("Variable:", var, "\n")
  cat("Estadístico de Shapiro-Wilk:", resultado$statistic, "\n")
  cat("p-valor:", resultado$p.value, "\n")
  
  # Evaluar normalidad según p-valor
  if (resultado$p.value > 0.05) {
    cat("Conclusión: La variable parece seguir una distribución normal.\n")
  } else {
    cat("Conclusión: La variable NO sigue una distribución normal.\n")
  }
  
  cat("\n--------------------------------------\n")
}

# Correlaciones de la Glucosa

# Calcular correlación de Glucosa con otras variables
correlaciones <- cor(numericas, use = "complete.obs", method = "pearson")

# Mostrar solo las correlaciones con Glucosa
correlaciones["GLUCOSA", ]

# Interpretacion:

# Las variables que tienen mayor relación con la glucosa (aunque sigue siendo débil) son:
#Cintura (0.238)
#LDL - colesterol malo (0.226)
#Edad (0.225)
#PAS3 (presión sistólica) (0.206)

#Esto sugiere que las personas con mayor edad, colesterol malo, presión arterial y perímetro de cintura tienden a tener niveles de glucosa más altos.





# Calcular la matriz de correlación
cor_matrix <- cor(datos %>% select_if(is.numeric), use = "complete.obs")

# Graficar Heatmap de Correlaciones
ggcorrplot(cor_matrix, method = "circle", type = "lower", lab = TRUE, 
           title = "Mapa de Correlaciones", 
           colors = c("red", "white", "blue"))

# Variables con mayor correlación con glucosa
variables_interes <- c("GLUCOSA", "CINTURA", "LDL", "EDAD", "PAS3")

# Crear gráficos de dispersión entre glucosa y variables relacionadas
par(mfrow = c(2,2)) # Para organizar los gráficos en 2 filas y 2 columnas

for (var in variables_interes[-1]) { # Excluir GLUCOSA
  plot(datos[[var]], datos$GLUCOSA, 
       xlab = var, ylab = "Glucosa", 
       main = paste("Glucosa vs", var),
       col = "blue", pch = 19)
  abline(lm(GLUCOSA ~ get(var), data = datos), col = "red", lwd = 2) # Línea de tendencia
}

# ANOVA

# Prueba t para SEXO (2 grupos)
t.test(GLUCOSA ~ SEXO, data = datos)

# p-value = 0.8347, mucho mayor a 0.05, lo que indica que no hay evidencia de que la glucosa sea diferente entre hombres y mujeres.


anova_T <- aov(GLUCOSA ~ ., data = datos)
summary(anova_T)

# Convertir el modelo ANOVA en una tabla
tabla_anova <- tbl_regression(anova_T)

# Mostrar la tabla
tabla_anova


# INTERPRETACION:

# Las variables más relacionadas con la glucosa son: diabetes, edad, peso, cintura, presión arterial y LDL.
# Factores como el sexo, el colesterol total y el cansancio no parecen influir en la glucosa en esta muestra.

# PREDICCION:


# Selección de las variables más relacionadas con la glucosa
modelo_glucosa <- lm(GLUCOSA ~ EDAD + E_CIVIL + IMC + DIABETES + LDL + PRES3 + PAS3 + DEPORTE + SINT_DEPRESION + HDL, data = datos)


Tabla_Regresion<-tbl_regression(modelo_glucosa)

plot(Tabla_Regresion)

# Obtener las predicciones de glucosa sin ingresar datos manualmente
datos$GLUCOSA_PRED <- predict(modelo_glucosa, datos)

# Visualizar un resumen del modelo
summary(modelo_glucosa)

# Graficar la correlación entre valores reales y predichos

ggplot(datos, aes(x = GLUCOSA, y = GLUCOSA_PRED)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Comparación entre Glucosa Real y Predicha",
       x = "Glucosa Real",
       y = "Glucosa Predicha") +
  theme_minimal()

# ESTOS GRAFICOS SON PARA ENTENDER MEJOR ESTADO CIVIL

table(datos$E_CIVIL) 
contrasts(datos$E_CIVIL) # Muestra cómo se codificaron las categorías en el modelo


ggplot(datos, aes(x = E_CIVIL, y = GLUCOSA, fill = E_CIVIL)) +
  geom_boxplot() +
  theme_minimal()





