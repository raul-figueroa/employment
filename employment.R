## Syntax to prepare unemployment data by county for Puerto Rico
library(readr)
library(dplyr)
library(tidyr)
library(dygraphs)


# Set the working directory to location of data
#etwd("~/MediaFire/Aplicaciones/desempleo")

# Import the data
data <- read_tsv("la.data.46.PuertoRico.txt")

## Divide the series_id variable into various usable variables

data$prefix <- substr(data$series_id, 1, 2)
data$sa_code <- substr(data$series_id, 3, 3)
data$at_code <- substr(data$series_id, 4, 5)
data$a_code <- substr(data$series_id, 6, 18)
data$m_code <- substr(data$series_id, 19, 20)

## Filter the desired county data
muestra <- data %>% filter(at_code =="CN")

# Fips codes for Puerto Rico
fips <- c("7200100000000",
          "7200300000000",
          "7200500000000",
          "7200700000000",
          "7200900000000",
          "7201100000000",
          "7201300000000",
          "7201500000000",
          "7201700000000",
          "7201900000000",
          "7202100000000",
          "7202300000000",
          "7202500000000",
          "7202700000000",
          "7202900000000",
          "7203100000000",
          "7203300000000",
          "7203500000000",
          "7203700000000",
          "7203900000000",
          "7204100000000",
          "7204300000000",
          "7204500000000",
          "7204700000000",
          "7204900000000",
          "7205100000000",
          "7205300000000",
          "7205400000000",
          "7205500000000",
          "7205700000000",
          "7205900000000",
          "7206100000000",
          "7206300000000",
          "7206500000000",
          "7206700000000",
          "7206900000000",
          "7207100000000",
          "7207300000000",
          "7207500000000",
          "7207700000000",
          "7207900000000",
          "7208100000000",
          "7208300000000",
          "7208500000000",
          "7208700000000",
          "7208900000000",
          "7209100000000",
          "7209300000000",
          "7209500000000",
          "7209700000000",
          "7209900000000",
          "7210100000000",
          "7210300000000",
          "7210500000000",
          "7210700000000",
          "7210900000000",
          "7211100000000",
          "7211300000000",
          "7211500000000",
          "7211700000000",
          "7211900000000",
          "7212100000000",
          "7212300000000",
          "7212500000000",
          "7212700000000",
          "7212900000000",
          "7213100000000",
          "7213300000000",
          "7213500000000",
          "7213700000000",
          "7213900000000",
          "7214100000000",
          "7214300000000",
          "7214500000000",
          "7214700000000",
          "7214900000000",
          "7215100000000",
          "7215300000000")

# Names of the counties in Puerto Rico
municipios <- c("Adjuntas",
                "Aguada",
                "Aguadilla",
                "Aguas Buenas",
                "Aibonito",
                "Añasco",
                "Arecibo",
                "Arroyo",
                "Barceloneta",
                "Barranquitas",
                "Bayamón",
                "Cabo Rojo",
                "Caguas",
                "Camuy",
                "Canóvanas",
                "Carolina",
                "Cataño",
                "Cayey",
                "Ceiba",
                "Ciales",
                "Cidra",
                "Coamo",
                "Comerío",
                "Corozal",
                "Culebra",
                "Dorado",
                "Fajardo",
                "Florida",
                "Guánica",
                "Guayama",
                "Guayanilla",
                "Guaynabo",
                "Gurabo",
                "Hatillo",
                "Hormigueros",
                "Humacao",
                "Isabela",
                "Jayuya",
                "Juana Díaz",
                "Juncos",
                "Lajas",
                "Lares",
                "Las Marías",
                "Las Piedras",
                "Loíza",
                "Luquillo",
                "Manatí",
                "Maricao",
                "Maunabo",
                "Mayagüez",
                "Moca",
                "Morovis",
                "Naguabo",
                "Naranjito",
                "Orocovis",
                "Patillas",
                "Peñuelas",
                "Ponce",
                "Quebradillas",
                "Rincón",
                "Río Grande",
                "Sabana Grande",
                "Salinas",
                "San Germán",
                "San Juan",
                "San Lorenzo",
                "San Sebastián",
                "Santa Isabel",
                "Toa Alta",
                "Toa Baja",
                "Trujillo Alto",
                "Utuado",
                "Vega Alta",
                "Vega Baja",
                "Vieques",
                "Villalba",
                "Yabucoa",
                "Yauco")

# Some other codes for matching

codigo_periodo  <- c("M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", "M09", "M10", "M11", "M12")

meses <- c("01. Enero", "02. Febrero", "03. Marzo", "04. Abril", "05. Mayo", "06. Junio", "07. Julio", "08. Agosto", "09. Septiembre", "10. Octubre", "11. Noviembre", "12. Diciembre")

codigo_esta <- c("03", "04", "05", "06")
estadistica <- c("Tasa de desempleo", "Desempleo", "Empleo", "Fuerza laboral")

## Match the values to the labels and create new variables

muestra$place <- municipios[match(muestra$a_code, fips)]
muestra$month <- meses[match(muestra$period, codigo_periodo)]
muestra$stat <- estadistica[match(muestra$m_code, codigo_esta)]


# Export the data to csv
write.csv(muestra, "bls_data.csv")

# Test using one county and stat
submuestra <- muestra %>% 
    filter(place == "San Juan" & stat == "Fuerza laboral" & year >= 1990) %>% 
    arrange(year, period)

# Convert the data to time series format
plot1 <- ts(submuestra$value, start = c(1990, 1), end = c(2015, 12), frequency = 12)

# plot using dygraph
dygraph(plot1) %>% dyRangeSelector() 
