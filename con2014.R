# Cargar datos
data <- read.csv("fichero 2014 (1).csv", sep=";")

# Recodificar la variable EDAD
data <- data[data$EDAD >= 18 & data$EDAD <= 30, ]
data$EDAD <- ifelse(data$EDAD >= 18 & data$EDAD <= 24, 1, 
                    ifelse(data$EDAD >= 25 & data$EDAD <= 30, 2, NA))
data <- na.omit(data)

# Recodificar la variable RELIGION
data$RELIGION <- ifelse(data$RELIGION == 3 | data$RELIGION == 4, 0,
                        ifelse(data$RELIGION == 1 | data$RELIGION == 2, 1, NA))
data <- na.omit(data)

# Recodificar la variable POLITICA
data$POLITICA <- ifelse(data$POLITICA >= 1 & data$POLITICA <= 3, 1, 
                        ifelse(data$POLITICA >= 4 & data$POLITICA <= 6, 2, 
                               ifelse(data$POLITICA >= 7 & data$POLITICA <= 10, 3, NA)))
data <- na.omit(data)

# Recodificar la variable ESTUDIOS
data$ESTUDIOS <- ifelse(data$ESTUDIOS == 1, 1,
                        ifelse(data$ESTUDIOS == 2, 2,
                               ifelse(data$ESTUDIOS == 3 | data$ESTUDIOS == 4, 3,
                                      ifelse(data$ESTUDIOS == 5, 4,
                                             ifelse(data$ESTUDIOS == 6, 5, NA)))))
data <- na.omit(data)

# Recodificar la variable ESTATUS
data$ESTATUS <- ifelse(data$ESTATUS == 4 | data$ESTATUS == 5, 1,
                       ifelse(data$ESTATUS == 2 | data$ESTATUS == 3, 2,
                              ifelse(data$ESTATUS == 1, 3, NA)))
data <- na.omit(data)

# Guardar el nuevo fichero recodificado
write.csv(data, "fichero_2014deverdad.csv", row.names = FALSE)

##PRIMER LOGIT
# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)

# Cargar el archivo recodificado
data <- read.csv("fichero_2014deverdad.csv")

# Ajustar el modelo de regresión logística
modelo_logit <- glm(RELIGION ~ POLITICA, data = data, family = binomial)

# Resumen del modelo
summary(modelo_logit)

# Crear un gráfico para visualizar la influencia de POLITICA en RELIGION
# Predecir probabilidades
data$predicted_prob <- predict(modelo_logit, type = "response")

# Crear un dataframe con las medias por nivel de POLITICA
plot_data <- data %>%
  group_by(POLITICA) %>%
  summarize(mean_prob = mean(predicted_prob))

# Gráfico
ggplot(plot_data, aes(x = as.factor(POLITICA), y = mean_prob)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Influencia de POLITICA en RELIGION",
       x = "POLITICA",
       y = "Probabilidad Media de RELIGION (1)") +
  theme_minimal()

##SEGUNDO LOGIT. VARIABLE ESTATUS
# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)

# Cargar el archivo recodificado
data <- read.csv("fichero_2014deverdad.csv")

# Ajustar el modelo de regresión logística
modelo_logit <- glm(RELIGION ~ ESTATUS, data = data, family = binomial)

# Resumen del modelo
summary(modelo_logit)

# Crear un gráfico para visualizar la influencia de ESTATUS en RELIGION
# Predecir probabilidades
data$predicted_prob <- predict(modelo_logit, type = "response")

# Crear un dataframe con las medias por nivel de ESTATUS
plot_data <- data %>%
  group_by(ESTATUS) %>%
  summarize(mean_prob = mean(predicted_prob))

# Gráfico
ggplot(plot_data, aes(x = as.factor(ESTATUS), y = mean_prob)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Influencia de ESTATUS en RELIGION",
       x = "ESTATUS",
       y = "Probabilidad Media de RELIGION (1)") +
  theme_minimal()

##REGRESIÓN LINEAL MÚLTIPLE
# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)

# Cargar el archivo recodificado
data <- read.csv("fichero_2014deverdad.csv")

# Ajustar el modelo de regresión logística múltiple
modelo_logit_multiple <- glm(RELIGION ~ EDAD + SEXO + POLITICA + ESTATUS + ESTUDIOS, data = data, family = binomial)

# Resumen del modelo
summary(modelo_logit_multiple)

# Crear un gráfico para visualizar la influencia de las variables independientes en RELIGION
# Coeficientes del modelo
coeficientes <- summary(modelo_logit_multiple)$coefficients

# Crear un dataframe con los coeficientes
coef_data <- data.frame(
  Variable = rownames(coeficientes),
  Coeficiente = coeficientes[, "Estimate"],
  ErrorEstandar = coeficientes[, "Std. Error"],
  ValorP = coeficientes[, "Pr(>|z|)"]
)

# Gráfico
ggplot(coef_data[-1, ], aes(x = Variable, y = Coeficiente)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = Coeficiente - ErrorEstandar, ymax = Coeficiente + ErrorEstandar), width = 0.2) +
  labs(title = "Influencia de Variables Independientes en RELIGION",
       x = "Variables Independientes",
       y = "Coeficiente de Regresión") +
  theme_minimal()
