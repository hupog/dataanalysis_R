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
salarioSexoOcupacion$Total<-as.numeric(salarioSexoOcupacion$Total) 
str(salarioSexoOcupacion)

str(salarioSexoSector)
salarioSexoSector$CA<-as.factor(salarioSexoSector$CA)
salarioSexoSector$Sector<-as.factor(salarioSexoSector$Sector)
salarioSexoSector$Sexo<-as.factor(salarioSexoSector$Sexo)

salarioSexoSector$Total <- gsub("\\.", "", salarioSexoSector$Total)
salarioSexoSector$Total <- gsub(",", ".", salarioSexoSector$Total)
salarioSexoSector$Total<-as.numeric(salarioSexoSector$Total)
str(salarioSexoSector)

str(satisfaccionEmpleo)
satisfaccionEmpleo$País<-as.factor(satisfaccionEmpleo$País)
satisfaccionEmpleo$Unidad<-as.factor(satisfaccionEmpleo$Unidad)
satisfaccionEmpleo$Satisfacción<-as.factor(satisfaccionEmpleo$Satisfacción)

satisfaccionEmpleo$Total <- gsub("\\.", "", satisfaccionEmpleo$Total)
satisfaccionEmpleo$Total <- gsub(",", ".", satisfaccionEmpleo$Total)
satisfaccionEmpleo$Total<-as.numeric(satisfaccionEmpleo$Total)
str(satisfaccionEmpleo)

str(tiempoEmpleo)
tiempoEmpleo$SectorActividad<-as.factor(tiempoEmpleo$SectorActividad)
tiempoEmpleo$TiempoTrabajo<-as.factor(tiempoEmpleo$TiempoTrabajo)

tiempoEmpleo$HorasTotales <- gsub("\\.", "", tiempoEmpleo$HorasTotales)
tiempoEmpleo$HorasTotales <- gsub(",", ".", tiempoEmpleo$HorasTotales)
tiempoEmpleo$HorasTotales<-as.numeric(tiempoEmpleo$HorasTotales)
str(tiempoEmpleo)

str(ocupadosSexoRama)
ocupadosSexoRama$RamaActividad<-as.factor(ocupadosSexoRama$RamaActividad)
ocupadosSexoRama$Sexo<-as.factor(ocupadosSexoRama$Sexo)
ocupadosSexoRama$Unidad<-as.factor(ocupadosSexoRama$Unidad)

ocupadosSexoRama$HorasTotales <- gsub("\\.", "", ocupadosSexoRama$HorasTotales)
ocupadosSexoRama$HorasTotales <- gsub(",", ".", ocupadosSexoRama$HorasTotales)
ocupadosSexoRama$HorasTotales<-as.numeric(ocupadosSexoRama$HorasTotales)
str(ocupadosSexoRama)


library(ggplot2)

ggplot(salarioSexoOcupacion, aes(x = Periodo, y = Total, color = Sexo)) +
  geom_line() +
  facet_wrap(~ Ocupación, scales = "free_y") +
  labs(title = "Salaries Over Time by Occupation and Gender",
       x = "Year",
       y = "Salary") +
  theme_minimal()

