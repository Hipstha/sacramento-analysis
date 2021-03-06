#INSTALL PACKAGES
#install.packages(ggplot2)
#install.packages(tidyr)
#install.packages(gridExtra)

#LOAD LIBRARIES
library(ggplot2)
library(tidyr)
library(grid)
library(gridExtra)
library(maps)
library(ggmap)

#START DATA
logCity <- read.csv("../data/SacramentocrimeJanuary2006.csv", stringsAsFactors = FALSE)
logCrimes <- read.csv("../data/crimes.csv", stringsAsFactors = FALSE)
logCity$cdatetime <- as.Date(logCity$cdatetime, format="%m/%d/%y")

#FUNCTIONS
#Inicio de process_dataframes
process_dataframes <- function(logCity, logCrimes){
  dataCity <- logCity
  dataCity$crime_level <- 0 
  for(cityRow in 1:nrow(logCity)){
    for(criRow in 1:nrow(logCrimes)){
      if(dataCity[cityRow,]$crimedescr == logCrimes[criRow,]$crimedescr){
        dataCity[cityRow,]$crime_level <- logCrimes[criRow,]$X1.9.level
      }
    }
  }
  return(dataCity)
}
#Final de #Inicio de process_dataframes
#Inicio de separate_districts
separate_districts <- function(dataCity){ #separate districts
  number_of_districts <- unique(dataCity$district)
  districts <- NULL
  for(count in 1:length(number_of_districts)){
    districts <- c(districts, list(dataCity[dataCity$district==count,]))
  }
  return(districts)
}
#Final de separate_districts
#Inicio de process_crime_districts
process_crime_districts <- function(districts){ #get a list of processed information
  length(districts)
  list_crimes_analysis <- NULL
  #Se pregunta por los dias
  repeat{
    print("Seleccione el d�a de inicio, entre 1 y 31")
    first_date <- scan(nmax=1)
    if(first_date > 31 || !(length(first_date)>0)){
      print("D�a no v�lido")
    }else{
      break
    }
  }
  repeat{
    print("Seleccione el d�a de fin, entre 1 y 31, debe ser mayor o igual al primero")
    second_date <- scan(nmax=1)
    if(first_date > 31 || !(length(first_date)>0) || second_date<first_date){
      print("D�a no v�lido")
    }else{
      break
    }
  }
  #Le da formato a las fechas a buscar
  firts_date <- paste("2006-1-",first_date,sep="")
  second_date <- paste("2006-1-",second_date,sep="")
  #Busca dentro de lista de distritos
    for(count in 1:length(districts)){
      crimes_analysis <- NULL
      crimes_analysis <- districts[[count]][,c("cdatetime",
                                               "district",
                                               "crime_level", 
                                               "longitude",
                                               "latitude")]
      #Busca el numero de inicio y final seg�n la fecha
      from_day <- 0
      to_day <- 0
      #Busca el numero del inicio de la fecha
      for(row in 1:nrow(crimes_analysis)){
        if(crimes_analysis[row,]$cdatetime == firts_date){
          from_day <- row
          break
        }
      }
      #Busca el numero del final de la fecha
      for(row in 1:nrow(crimes_analysis)){
        if(crimes_analysis[row,]$cdatetime == second_date){
          to_day <- row
        }
      }
      #Regresa los datos en el intervalo de tiempo deseado
      crimes_analysis <- crimes_analysis[from_day:to_day,]
      crimes_analysis$registry_number <- 0
      crimes_analysis$sum_acomulated <- 0
      crimes_analysis$mean_acomulated <- 0
      #Asigna numero de registro
      for(row in 1:nrow(crimes_analysis)){
        crimes_analysis[row,]$registry_number <- row
      }
      sum_crimes <- 0
      for(row in 1:nrow(crimes_analysis)){
        sum_crimes = (sum_crimes + crimes_analysis[row,]$crime_level)
        crimes_analysis[row,]$sum_acomulated <- sum_crimes
      }
      crimes_analysis$mean_acomulated <- crimes_analysis$sum_acomulated/crimes_analysis$registry_number
      crimes_analysis$cdatetime <- as.Date(crimes_analysis$cdatetime)
      list_crimes_analysis <- c(list_crimes_analysis, list(crimes_analysis))
    }
    return(list_crimes_analysis)
}
#Final de process_crime_districts
#Inicio de process_crime_all
process_crime_all <- function(dataCity, firts_date, second_date){ #get a list of processed information
    crimes_analysis <- NULL
    crimes_analysis <- dataCity[,c("cdatetime",
                                   "district",
                                   "crime_level", 
                                   "longitude",
                                   "latitude")]
    #Busca el numero de inicio y final seg�n la fecha
    from_day <- 0
    to_day <- 0
    #Busca el numero del inicio de la fecha
    for(row in 1:nrow(crimes_analysis)){
      if(crimes_analysis[row,]$cdatetime == firts_date){
        from_day <- row
        break
      }
    }
    #Busca el numero del final de la fecha
    for(row in 1:nrow(crimes_analysis)){
      if(crimes_analysis[row,]$cdatetime == second_date){
        to_day <- row
      }
    }
    #Regresa los datos en el intervalo de tiempo deseado
    crimes_analysis <- crimes_analysis[from_day:to_day,]
    crimes_analysis$registry_number <- 0
    crimes_analysis$sum_acomulated <- 0
    crimes_analysis$mean_acomulated <- 0
    #Asigna numero de registro
    for(row in 1:nrow(crimes_analysis)){
      crimes_analysis[row,]$registry_number <- row
    }
    sum_crimes <- 0
    for(row in 1:nrow(crimes_analysis)){
      sum_crimes = (sum_crimes + crimes_analysis[row,]$crime_level)
      crimes_analysis[row,]$sum_acomulated <- sum_crimes
    }
    crimes_analysis$mean_acomulated <- crimes_analysis$sum_acomulated/crimes_analysis$registry_number
    crimes_analysis$cdatetime <- as.Date(crimes_analysis$cdatetime)
  return(crimes_analysis)
}
#Final de process_crime_all
#Inicio de draw_plots
draw_plots <- function(crimes_analysis_districts){ #Dibuja los graficos de los distritos
  list_plots = NULL
  for(count in 1:length(crimes_analysis_districts)){
    title_plot <- paste("Distrito", count )
    q1 <- quantile(crimes_analysis_districts[[count]]$mean_acomulated, c(0.25))
    q3 <- quantile(crimes_analysis_districts[[count]]$mean_acomulated, c(0.75))
    plot <- ggplot(data=crimes_analysis_districts[[count]], aes(x=cdatetime, y=mean_acomulated, colour="red"))+
      geom_smooth()+
      scale_y_continuous(limits=c(4,6))+
      guides(colour=FALSE)+
      labs(x="Fecha",
           y="Promedio de \ngravedad de cr�menes",
           title=title_plot)+
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x=element_text(angle=30))
      
    list_plots <- c(list_plots, list(plot))
  }
  return(list_plots)
}
#Final de draw_plots
#Inicio de get_graphic()
get_graphic <- function(crimes_analysis_districts, from_day, to_day){
  dev.new()
  plot_list <- draw_plots(crimes_analysis_districts)
  #grid.arrange(grobs=plot_list, ncol=3, 
  #             top="Nivel de crimen en la ciudad de Sacramento, California. Enero del 2006",
  #             bottom="jijiji")
  from_day_format <- format(from_day, format="%d de %B del %Y")
  to_day_format <- format(to_day, format="%d de %B del %Y")
  title_plot <- paste("Nivel de criminalidad en Sacramento, California.\nDel",from_day_format,"al",to_day_format )
  grid.arrange(grobs=plot_list, ncol=3, 
               top=textGrob(title_plot,
                            gp=gpar(fontsize=14, just="center")))
}
#Final de get_graphic()
#Inicio de map_data()
map_data <- function(city, maptype){ #Carga configuraci�n de mapa
  register_google(key="AIzaSyB8zQaXq_d56KdQDpL-8zugItQk3aRtQ98")
  city_map <- get_map(location=city, maptype = maptype, zoom=11)
  return(city_map)
}
#final de map_data()
#Inicio de squares_map_districts()
squares_map_districts <- function(crimes_analysis_districts, district_selected){ #Carga mapa de calor por cuadros
   from_day_format <- format(from_day, format="%d de %B del %Y")
   to_day_format <- format(to_day, format="%d de %B del %Y") 
   sub_title <- "Promedio de gravedad de crimenes por zona"
   if(district_selected=="all"){
    title_plot <- paste("Nivel de criminalidad en Sacramento, California.\nDel",from_day_format,"al",to_day_format )
    crime_squares <- ggmap(city_map) %+% crimes_analysis_all +
      aes(x=longitude, y=latitude, z=mean_acomulated)+
      stat_summary2d(alpha=0.5)+
      scale_fill_gradient(name="Grado de riesgo",
                          low="green",
                          high="red")+
      theme(legend.background=element_rect(fill="lightblue",
                                           linetype="solid",
                                           colour="darkblue"),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))+
      labs(x="Longitud",
           y="Latitud",
           title=title_plot,
           subtitle = sub_title)
    #dev.new()
  }else{
    title_plot <- paste("Nivel de criminalidad en Sacramento, California.\nDistrito",district_selected,"\nDel",from_day_format,"al",to_day_format)
    crime_squares <- ggmap(city_map) %+% crimes_analysis_districts[[district_selected]] +
      aes(x=longitude, y=latitude, z=mean_acomulated)+
      stat_summary2d(alpha=0.5)+
      scale_fill_gradient(name="Grado de riesgo",
                          low="green",
                          high="red")+
      theme(legend.background=element_rect(fill="lightblue",
                                           linetype="solid",
                                           colour="darkblue"),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))+
      labs(x="Longitud",
           y="Latitud",
           title=title_plot,
           subtitle = sub_title)
    #dev.new()
  }
  return(crime_squares)
  #return(crime_squares)
}
#Final de squares_map_districts()
#FINAL DE FUNCIONES

#MAIN
#---------------GRAFICAS SACRAMENTO------------------------------------
#dataCity <- process_dataframes(logCity, logCrimes) #Carga todos los datos (La funci�n mas tardada)
city_map <- map_data("Sacramento", "hybrid")
districts <- separate_districts(dataCity)
crimes_analysis_districts<-process_crime_districts(districts)
from_day <- crimes_analysis_districts[[1]][1,"cdatetime"]
to_day <-crimes_analysis_districts[[1]][length(crimes_analysis_districts[[1]]$cdatetime),
                                        "cdatetime"]
get_graphic(crimes_analysis_districts, from_day, to_day)

#---------------MAPA SACRAMENTO------------------------------------
crimes_analysis_all <- process_crime_all(dataCity, from_day, to_day)
squares_map_districts(crimes_analysis_districts, "all")

#FINAL DE MAIN


