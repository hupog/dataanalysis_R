#Leemos los datasets
salarioSexoOcupacion<-read.csv("./datasets/salarioSexoOcupacion2022.csv", TRUE, sep=";")
salarioSexoSector<-read.csv("./datasets/salarioSexoSector2022.csv", TRUE, sep=";")
satisfaccionEmpleo<-read.csv("./datasets/satisfaccionEmpleo.csv", TRUE, sep=";")
tiempoEmpleo<-read.csv("./datasets/tiempoEmpleo.csv", TRUE, sep=";")
ocupadosSexoRama<-read.csv("./datasets/ocupadosSexoRama.csv", TRUE, sep=";")

#Echamos un vistazo inicial a los datos
head(salarioSexoOcupacion)
head(salarioSexoSector)
head(satisfaccionEmpleo)
head(tiempoEmpleo)
head(ocupadosSexoRama)

#Observamos que ciertas variables tienen nombres muy largos, por lo que los cambiamos por unos más fáciles de tratar
colnames(salarioSexoOcupacion)[colnames(salarioSexoOcupacion) =="Grupos.principales.de.la.CNO.11"] <- "Ocupación"

colnames(salarioSexoSector)[colnames(salarioSexoSector) =="Sectores.de.actividad"] <- "Sector"
colnames(salarioSexoSector)[colnames(salarioSexoSector) =="Comunidades.autónomas"] <- "CA"

colnames(satisfaccionEmpleo)[colnames(satisfaccionEmpleo) =="País.de.nacimiento"] <- "País"
colnames(satisfaccionEmpleo)[colnames(satisfaccionEmpleo) =="Tipo.de.dato"] <- "Unidad"
colnames(satisfaccionEmpleo)[colnames(satisfaccionEmpleo) =="Según.sea.el.nivel.de.satisfacción.en.su.empleo.actual"] <- "Satisfacción"

colnames(tiempoEmpleo)[colnames(tiempoEmpleo) =="Sectores.de.actividad.CNAE.2009"] <- "SectorActividad"
colnames(tiempoEmpleo)[colnames(tiempoEmpleo) =="Tiempo.de.trabajo"] <- "TiempoTrabajo"
colnames(tiempoEmpleo)[colnames(tiempoEmpleo) =="Total"] <- "HorasTotales"

colnames(ocupadosSexoRama)[colnames(ocupadosSexoRama) =="Rama.de.actividad.CNAE.2009"] <- "RamaActividad"
colnames(ocupadosSexoRama)[colnames(ocupadosSexoRama) =="Total"] <- "HorasTotales"
colnames(ocupadosSexoRama)[colnames(ocupadosSexoRama) =="Total"] <- "HorasTotales"

#Vemos de que tipo es cada variable y si está mal, la ajustamos
str(salarioSexoOcupacion)
salarioSexoOcupacion$Ocupación<-as.factor(salarioSexoOcupacion$Ocupación)
salarioSexoOcupacion$Sexo<-as.factor(salarioSexoOcupacion$Sexo) 

salarioSexoOcupacion$Total <- gsub("\\.", "", salarioSexoOcupacion$Total)
salarioSexoOcupacion$Total <- gsub(",", ".", salarioSexoOcupacion$Total)
salarioSexoOcupacion$Total <- gsub("-", "", salarioSexoOcupacion$Total)
salarioSexoOcupacion$Total<-as.numeric(salarioSexoOcupacion$Total) 
str(salarioSexoOcupacion)

str(salarioSexoSector)
salarioSexoSector$CA<-as.factor(salarioSexoSector$CA)
salarioSexoSector$Sector<-as.factor(salarioSexoSector$Sector)
salarioSexoSector$Sexo<-as.factor(salarioSexoSector$Sexo)

salarioSexoSector$Total <- gsub("\\.", "", salarioSexoSector$Total)
salarioSexoSector$Total <- gsub(",", ".", salarioSexoSector$Total)
salarioSexoSector$Total <- gsub("-", "", salarioSexoSector$Total)
salarioSexoSector$Total<-as.numeric(salarioSexoSector$Total)
str(salarioSexoSector)

str(satisfaccionEmpleo)
satisfaccionEmpleo$País<-as.factor(satisfaccionEmpleo$País)
satisfaccionEmpleo$Unidad<-as.factor(satisfaccionEmpleo$Unidad)
satisfaccionEmpleo$Satisfacción<-as.factor(satisfaccionEmpleo$Satisfacción)

satisfaccionEmpleo$Total <- gsub("\\.", "", satisfaccionEmpleo$Total)
satisfaccionEmpleo$Total <- gsub(",", ".", satisfaccionEmpleo$Total)
satisfaccionEmpleo$Total <- gsub("-", "", satisfaccionEmpleo$Total)
satisfaccionEmpleo$Total<-as.numeric(satisfaccionEmpleo$Total)
str(satisfaccionEmpleo)

str(tiempoEmpleo)
tiempoEmpleo$SectorActividad<-as.factor(tiempoEmpleo$SectorActividad)
tiempoEmpleo$TiempoTrabajo<-as.factor(tiempoEmpleo$TiempoTrabajo)

tiempoEmpleo$HorasTotales <- gsub("\\.", "", tiempoEmpleo$HorasTotales)
tiempoEmpleo$HorasTotales <- gsub(",", ".", tiempoEmpleo$HorasTotales)
tiempoEmpleo$HorasTotales <- gsub("-", "", tiempoEmpleo$HorasTotales)
tiempoEmpleo$HorasTotales<-as.numeric(tiempoEmpleo$HorasTotales)
tiempoEmpleo$Periodo <- factor(tiempoEmpleo$Periodo, levels = unique(tiempoEmpleo$Periodo))
str(tiempoEmpleo)

str(ocupadosSexoRama)
ocupadosSexoRama$RamaActividad<-as.factor(ocupadosSexoRama$RamaActividad)
ocupadosSexoRama$Sexo<-as.factor(ocupadosSexoRama$Sexo)
ocupadosSexoRama$Unidad<-as.factor(ocupadosSexoRama$Unidad)

ocupadosSexoRama$HorasTotales <- gsub("\\.", "", ocupadosSexoRama$HorasTotales)
ocupadosSexoRama$HorasTotales <- gsub(",", ".", ocupadosSexoRama$HorasTotales)
ocupadosSexoRama$HorasTotales <- gsub("-", "", ocupadosSexoRama$HorasTotales)
ocupadosSexoRama$HorasTotales<-as.numeric(ocupadosSexoRama$HorasTotales)
ocupadosSexoRama$Periodo <- factor(ocupadosSexoRama$Periodo, levels = unique(ocupadosSexoRama$Periodo))
str(ocupadosSexoRama)

install.packages("ggplot2")
library(ggplot2)
install.packages("dyplr")
library(dplyr)
############ COMPARACION SALARIOS ############


# SALARIO POR OCUPACION CONCRETA Y GENERO #
ggplot(salarioSexoOcupacion, aes(x = Ocupación, y = Total, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +  # Lo giramos para que se lea mejor debido a que la gráfica crece hacia la derecha
  labs(title = "Salario Promedio por Ocupación y Género",
       x = "Ocupación",
       y = "Salario Promedio") +
  theme_minimal()

# SALARIO POR SECTOR Y GENERO #
ggplot(salarioSexoSector, aes(x = Sector, y = Total, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +  # Giramos por lo mismo que la anterior
  labs(title = "Salario Promedio por Sector y Género",
       x = "Sector",
       y = "Salario") +
  theme_minimal()

# POR REGIONES #
ggplot(salarioSexoSector, aes(x = CA, y = Total, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Average Salary by Region and Gender",
       x = "Region",
       y = "Average Salary") +
  theme_minimal()


##############################

############ SATISFACCIÓN ###########


# SATISFACCIÓN POR PAISES #
# Filtrar por porcentajes
satisfaccionEmpleo_percentages <- subset(satisfaccionEmpleo, Unidad == "Porcentajes")

# Barra
ggplot(satisfaccionEmpleo_percentages, aes(x = País, y = Total, fill = Satisfacción)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Niveles de Satisfacción por País",
       x = "País",
       y = "Porcentaje") +
  theme_minimal()


# POR PAIS EN NIVELES #
# Barra
ggplot(satisfaccionEmpleo_percentages, aes(x = Satisfacción, y = Total, fill = País)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de Niveles de Satisfacción por País",
       x = "Nivel de Satisfacción",
       y = "Porcentaje") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##############################

############ TIEMPO TRABAJADO ############

# Agrupación para poder mostrar en gráfico
agg_data <- tiempoEmpleo %>%
  group_by(SectorActividad, TiempoTrabajo) %>%
  summarize(AverageHoras = mean(HorasTotales, na.rm = TRUE))

# Barra horas promedio
ggplot(agg_data, aes(x = TiempoTrabajo, y = AverageHoras, fill = SectorActividad)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Horas promedio basado en el Sector y Tipo de Hora",
       x = "Tipo de horas",
       y = "Horas promedio") +
  theme_minimal()

##############################

############ TIEMPO TRABAJADO ############

# Seleccionamos solo porcentajes
ocupadosSexoRama_pct <- subset(ocupadosSexoRama, Unidad == "Porcentaje")

# Diagrama barras por cada actividad y genero
ggplot(ocupadosSexoRama_pct, aes(x = Periodo, y = HorasTotales, fill = Sexo)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ RamaActividad, scales = "free_y") +
  labs(title = "Porcentaje de Distribución de Horas por Actividad y Género",
       x = "Período",
       y = "Porcentaje") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##############################

############ PROBABILIDAD ############

# Filtramos por actividad, en este ejemplo, la primera, Agricultura, ganderia, ..., y género para hacer la estimación
subset_data <- subset(ocupadosSexoRama, RamaActividad == "01 Agricultura, ganadería, caza y servicios relacionados con las mismas" & Sexo == "Ambos sexos" &  Unidad == "Valor absoluto")

mu <- mean(subset_data$HorasTotales, na.rm = TRUE)
sigma <- sd(subset_data$HorasTotales, na.rm = TRUE)

# Probabilidad de que las horas totales sea mayor que 50000: P(HorasTotales <= 50000)
p_value <- pnorm(50000, mean = mu, sd = sigma)
print(paste("P(HorasTotales <= 50000):", p_value))
# Si lo pensamos es un resultado lógico porque no hay ningún valor igual ni por encima de 50000

# Percentil 90
quantile_90 <- qnorm(0.90, mean = mu, sd = sigma)
print(paste("Percentil 90 de HorasTotales:", quantile_90))


####
# Estimamos lambda como la media de horas totales
lambda <- mean(subset_data$HorasTotales, na.rm = TRUE)

# Probabilidad: P(HorasTotales = 5000)
prob <- dpois(5000, lambda)
print(paste("P(HorasTotales = 5000):", prob))

# Hacemos una simulación con 100 valores autogenerados de normal
simulated_data <- rpois(100, lambda)
hist(simulated_data, main = "Simulación de horas trabajadas (Poisson)", xlab = "Horas Totales")


###
# Definimos parametros: Número total de respuestas (n) y probabilidad de "Satisfecho en gran medida" (p), escojo "Satisfecho en gran medida" ya que es la muestra poblacional mas grande en el dataset.
n <- 100  # tamaño de ejemplo
p <- 0.54  # from the percentage of "Satisfecho en gran medida"

# Probabilidad: P(X <= 60)
prob_binom <- pbinom(60, size = n, prob = p)
print(paste("P(X <= 60):", prob_binom))

# Simulación de una binomial
simulated_satisfaction <- rbinom(100, size = n, prob = p)
hist(simulated_satisfaction, main = "Respuestas de la Simulación", xlab = "Cantidad de Satisfecho en gran medida")


###
# Calculamos lambda como 1 / media de horas totales
lambda_exp <- 1 / mean(tiempoEmpleo$HorasTotales, na.rm = TRUE)

# Probabilidad: P(HorasTotales <= 200)
prob_exp <- pexp(200, rate = lambda_exp)
print(paste("P(HorasTotales <= 200):", prob_exp))

# Simulación de una exponencial
simulated_time <- rexp(100, rate = lambda_exp)
hist(simulated_time, main = "Tiempo trabajado Simulado (Exponencial)", xlab = "Horas")
##############################

# Filtramos por ocupaciones
grouped_data <- salarioSexoOcupacion %>%
  group_by(Ocupación) %>%
  summarise(mean_salary = mean(Total), sd_salary = sd(Total), n = n())

# Calculamos intervalos de confianza
grouped_data <- grouped_data %>%
  mutate(
    se = sd_salary / sqrt(n),
    ci_lower = mean_salary - qt(0.975, df = n - 1) * se,
    ci_upper = mean_salary + qt(0.975, df = n - 1) * se
  )
print(grouped_data)
###AÑADIR CONCLUSIONES###

# Probamos si la varianza de HorasTotales difiere entre dos sectores de actividad

sector1 <- subset(tiempoEmpleo, SectorActividad == "Construcción")
sector2 <- subset(tiempoEmpleo, SectorActividad == "Servicios")

# Variance test
var.test(sector1$HorasTotales, sector2$HorasTotales)
###AÑADIR CONCLUSIONES###

