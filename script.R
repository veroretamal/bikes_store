#Primer paso: paquetes y librerias
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("rmarkdown")
library("tidyverse")
library("lubridate")
library("ggplot2")
library("dplyr")
library("rmarkdown")

### Setear el directorio desde el que se trabajará
getwd()
setwd("/cloud/project/q")
list.files() ##chequear que se haya cargado correctamente el directorio
list.files(path = "/cloud/project/q", pattern = "*.csv")

### Configurar data frames

q1 <- read_csv("/cloud/project/q/q1.csv")
q2 <- read_csv("/cloud/project/q/q2.csv")
q3 <- read_csv("/cloud/project/q/q3.csv")
q4 <- read_csv("/cloud/project/q/q4.csv")

#Manejo de archivos para combinar en uno

#Comparar columnas y chequear que correspondan entre si
colnames(q1)
colnames(q2)
colnames(q3)
colnames(q4)

#Inspeccionar el data frame
str(q1)
str(q2)
str(q3)
str(q4)

#Combinar los 4 dataframes en una 
all_trips <- bind_rows(q1, q2, q3, q4)

#Ver df
View(all_trips)

#Mutar variables temporales a POSIXct y num
str(all_trips)

    #started y ended at
all_trips <- all_trips %>%
  mutate(
    started_at = dmy_hm(started_at),  # Convertir a formato fecha-hora
    ended_at = dmy_hm(ended_at)       # Convertir a formato fecha-hora
  )

    #ride length
all_trips <- all_trips %>%
  mutate(
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins"))
  )
str(all_trips)
View(all_trips)

#Añadir columnas de dia, mes, año de cada viaje

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$year <- format(as.Date(all_trips$date),"%y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

str(all_trips)

#Analisis descriptivo

summary(all_trips)

# DESCRIPTIVO DE DURACIÓN DE LOS VIAJES

mean(all_trips$ride_length)
median(all_trips$ride_length)
max(all_trips$ride_length)
min(all_trips$ride_length)
sd(all_trips$ride_length, na.rm = TRUE)  # Desviación estándar

## Descriptivo de cantidad de viajes por día de semana. 
all_trips %>%
  mutate(weekday = wday(started_at, label = TRUE, abbr = FALSE)) %>%
  group_by(weekday) %>%
  summarise(total_rides = n()) %>%
  arrange(weekday)


## Análisis por tipo de usuario (Casual v/s Member)

### Número de viajes por tipo de usuario
all_trips %>%
  count(member_casual)

### Duración promedio del viaje por tipo de usuario
all_trips %>%
  group_by(member_casual) %>%
  summarise(
    avg_ride_length = mean(ride_length, na.rm = TRUE),
    median_ride_length = median(ride_length, na.rm = TRUE),
    sd_ride_length = sd(ride_length, na.rm = TRUE)
  )

### Comparación de tiempos (max-min) de viaje por tipo de usuario
aggregate(all_trips$ride_length~all_trips$member_casual, FUN=max)
aggregate(all_trips$ride_length~all_trips$member_casual, FUN=min)

### Tiempo promedio de viaje por dia de la semana y tipo de usuario
all_trips %>%
  mutate(weekday = wday(started_at, label = TRUE, abbr = FALSE)) %>%  # Extrae el día de la semana con nombre completo
  group_by(member_casual, weekday) %>%
  summarise(
    number_of_rides = n(),
    average_duration = mean(ride_length, na.rm = TRUE)  # Evita errores con valores NA
  ) %>%
  arrange(member_casual, weekday)

#ANÁLISIS VISUAL
### Número de Viajes por Tipo de Usuario y Día de la Semana
all_trips%>%
  mutate(weekday=wday(started_at, label=TRUE)
  )%>%
  group_by(member_casual, weekday)%>%
  summarise(number_of_rides=n(), 
            average_duration=mean(ride_length))%>%
  arrange(member_casual, weekday)%>%
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual))+
  geom_col(position="dodge")+  labs(title = "Número de viajes por día de la semana",
                                    x = "Día de la semana", 
                                    y = "Número de viajes") +
  theme_minimal()

### Visualizacion por duracion promedio de viaje por tipo de usuario y dia de la semana.

all_trips%>%
  mutate(weekday=wday(started_at, label=TRUE))%>%
  group_by(member_casual, weekday)%>%
  summarise(number_of_rides=n(), 
            average_duration=mean(ride_length))%>%
  arrange(member_casual, weekday)%>%
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual))+
  geom_col(position="dodge") + labs(title = "Número de viajes por día de la semana",
                                    x = "Día de la semana", 
                                    y = "Número de viajes") +
  theme_minimal()


