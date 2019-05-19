#INSTALL PACKAGES
#install.packages(ggplot2)
#install.packages(maps)
#install.packages(ggmap)

#LOAD LIBRARIES
library(ggplot2)
library(maps)
library(ggmap)

#FUNCTIONS
#Inicio de process_dataframes
process_dataframes <- function(logCity, logCrimes){
  data_city <- logCity
  data_city$crime_level <- 0 
  for(cityRow in 1:nrow(logCity)){
    for(criRow in 1:nrow(logCrimes)){
      if(data_city[cityRow,]$crimedescr == logCrimes[criRow,]$ï..crimedescr){
        data_city[cityRow,]$crime_level <- logCrimes[criRow,]$X1.9.level
      }
    }
  }
  return(data_city)
}
#Final de process_dataframes
#Inicio de map_data()
map_data <- function(city, maptype){ #Carga configuración de mapa
  register_google(key="AIzaSyB8zQaXq_d56KdQDpL-8zugItQk3aRtQ98")
  city_map <- get_map(location=city, maptype = maptype, zoom=11)
  return(city_map)
}
#final de map_data()
#Inicio de density_map()
density_map <- function(data_city){ #Carga mapa de densidades
  crime_density <- ggmap(city_map, extend = "device")+
    geom_density2d(data=data_city, aes(x=longitude, y=latitude),
                   size=0.3, alpha=1)+
    stat_density2d(data=data_city, aes(x=longitude, y=latitude, fill=..level..),
                   size=0.01, bins=16,  alpha=0.2, geom="polygon")+
    scale_fill_gradient(name="Total de crímenes",low="green", high="red")+
    theme(legend.background=element_rect(fill="lightblue",
                                         linetype="solid",
                                         colour="darkblue"),
          plot.title = element_text(hjust = 0.5))+
    labs(x="Longitud",
         y="Latitud",
         title="Criminalidad en Sacramento, California, Enero del 2016
            \nDensidad de criminalidad")
  dev.new()
  return(crime_density)
}
#Final de density_map()
#Inicio de city_map()
point_map <- function(data_city){ #Carga mapa de puntos
  crime_sites <- ggmap(city_map, extend="device")+
    geom_point(data=data_city, aes(x=longitude, y=latitude, colour=crime_level, alpha=crime_level), 
              size=2 )+
    scale_color_gradient(name="Gravedad de crimen", low="green", high="red")+
    theme(legend.background=element_rect(fill="lightblue",
                                         linetype="solid",
                                         colour="darkblue"),
          plot.title = element_text(hjust = 0.5))+
    guides(alpha=FALSE)+
    labs(x="Longitud",
         y="Latitud",
         title="Criminalidad en Sacramento, California, Enero del 2016
            \nSitios donde se cometieron los crímenes")
  dev.new()
  return(crime_sites)
}
#Final de city_map()
#Inicio de map_square()
squares_map <- function(data_city){ #Carga mapa de calor por cuadros
  crime_squares <- ggmap(city_map) %+% data_city +
    aes(x=longitude, y=latitude, z=crime_level)+
    stat_summary2d(alpha=0.5)+
    scale_fill_gradient(name="Grado de riesgo",
                        low="green",
                        high="red")+
    theme(legend.background=element_rect(fill="lightblue",
                                         linetype="solid",
                                         colour="darkblue"),
          plot.title = element_text(hjust = 0.5))+
    labs(x="Longitud",
         y="Latitud",
         title="Criminalidad en Sacramento, California, Enero del 2016
            \nGrado de riesgo por cuadrante")
  dev.new()
  return(crime_squares)
}
#Final de map_square()
#FINAL DE FUNCIONES

#MAIN

#START DATA
logCity <- read.csv("../data/SacramentocrimeJanuary2006.csv", stringsAsFactors = FALSE)
logCrimes <- read.csv("../data/crimes.csv", stringsAsFactors = FALSE)
logCity$cdatetime <- as.Date(logCity$cdatetime, format="%m/%d/%y")

#LOAD AND PROCESS DATA
data_city <- process_dataframes(logCity, logCrimes)

#roadmap
#hybrid

city_map <- map_data("Sacramento", "hybrid")
density_map(data_city)
point_map(data_city)
squares_map(data_city)

#FINAL DE MAIN