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

############ COMPARACION SALARIOS ############


# SALARIO POR OCUPACION CONCRETA Y GENERO #
ggplot(salarioSexoOcupacion, aes(x = Ocupación, y = Total, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +  # Flip for better readability
  labs(title = "Average Salary by Occupation and Gender",
       x = "Occupation",
       y = "Average Salary",
       fill = "Gender") +
  theme_minimal()

# POR GENERO Y OCUPACIÓN #
ggplot(salarioSexoOcupacion, aes(x = Periodo, y = Total, color = Sexo)) +
  geom_line(size = 1) +
  facet_wrap(~ Ocupación, scales = "free_y") +
  labs(title = "Salary Trends Over Time by Gender and Occupation",
       x = "Year",
       y = "Salary",
       color = "Gender") +
  theme_minimal()

# SALARIO POR SECTOR Y GENERO #
ggplot(salarioSexoSector, aes(x = Sector, y = Total, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +  # Flip for better readability
  labs(title = "Average Salary by Sector and Gender",
       x = "Sector",
       y = "Average Salary",
       fill = "Gender") +
  theme_minimal()

# POR REGIONES #
ggplot(salarioSexoSector, aes(x = CA, y = Total, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Average Salary by Region and Gender",
       x = "Region",
       y = "Average Salary",
       fill = "Gender") +
  theme_minimal()

##############################

############ SATISFACCIÓN ###########


# SATISFACCIÖN POR PAISES #
# Filtrar por porcentajes
satisfaccionEmpleo_percentages <- subset(satisfaccionEmpleo, Unidad == "Porcentajes")

# Bar chart for satisfaction levels
ggplot(satisfaccionEmpleo_percentages, aes(x = País, y = Total, fill = Satisfacción)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Satisfaction Levels by Country",
       x = "Country",
       y = "Percentage",
       fill = "Satisfaction Level") +
  theme_minimal()


# POR PAIS EN NIVELES #
# Grouped bar chart for satisfaction levels
ggplot(satisfaccionEmpleo_percentages, aes(x = Satisfacción, y = Total, fill = País)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Satisfaction Levels by Country",
       x = "Satisfaction Level",
       y = "Percentage",
       fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##############################

############ TIEMPO TRABAJADO ############

# Aggregate data for average hours
agg_data <- tiempoEmpleo %>%
  group_by(SectorActividad, TiempoTrabajo) %>%
  summarize(AverageHoras = mean(HorasTotales, na.rm = TRUE))

# Bar chart for average hours
ggplot(agg_data, aes(x = TiempoTrabajo, y = AverageHoras, fill = SectorActividad)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Average Hours by Sector and Time Category",
       x = "Time Category",
       y = "Average Hours",
       fill = "Sector") +
  theme_minimal()

##############################

############ TIEMPO TRABAJADO ############

# Filter for percentage data
ocupadosSexoRama_pct <- subset(ocupadosSexoRama, Unidad == "Porcentaje")

# Stacked bar chart for percentage distribution
ggplot(ocupadosSexoRama_pct, aes(x = Periodo, y = HorasTotales, fill = Sexo)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ RamaActividad, scales = "free_y") +
  labs(title = "Percentage Distribution of Hours by Activity and Gender",
       x = "Period",
       y = "Percentage",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##############################
