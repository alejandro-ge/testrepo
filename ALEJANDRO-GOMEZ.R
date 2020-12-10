library(tidyverse)
library(readr)
peliculas <- read_csv("C:/Users/aleja/Desktop/Diplomado/02 R/Talleres/Taller 02/peliculas.csv")

#PARTE I
peliculas_parte1=peliculas %>% 
  dplyr::filter(country=="USA",rating=="R",budget>0) %>% 
  dplyr::select(name,budget,gross, score,votes,year) %>% 
  dplyr::mutate(utilidad=gross-budget) %>% 
  dplyr::mutate(score_corregido=score*((votes-1)/votes)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(mayorUtilidad=name[utilidad==max(utilidad)],mayorIngreso=name[gross==max(gross)],mayorScore=name[score_corregido==max(score_corregido)],mayorVotos=name[votes==max(votes)])
as.data.frame(peliculas_parte1)


#PARTE II
library(purrr)
seccion = function(x){
  print(paste("Genero",x$genre[1]))
  cat("\n")
  print(paste("Estrella película mayor ingreso: ",x$star[x$gross==max(x$gross)]))
  print(paste("Estrella película mayor puntaje: ",x$star[x$score==max(x$score)]))
  print("Las películas 5 más votadas del género son: ")
  print(as.data.frame(head(x[,c(7,13)],n=5)))
  
  
  #print(head(x$name,n=5))
  cat("\n")
  tablaParteII=x %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarise(totalPresupuesto=sum(budget),duracionPromedio=mean(runtime/60,na.rm=TRUE))
  
  print("Total de presupuesto y promedio de duración de las películas, en horas, por año.")
  print(as.data.frame(tablaParteII))
  cat("\n")
  cat("\n")
  cat("\n")
  cat("\n")
}

peliculas_parte2=peliculas%>% 
  dplyr::filter(year>=1990,year<2000) %>%
  dplyr::arrange(desc(votes)) %>% 
  dplyr::group_split(genre) %>% 
  purrr::map(seccion)

#ParteIII
parte3=function(x,y){
  df=as.data.frame.matrix(table(x,y))
  df$maximo=apply(df[,2:ncol(df)],1, max)
  df=dplyr::filter(df,maximo>1)
  return(df)
}
writersPerGenre=parte3(peliculas$writer,peliculas$genre)
starPerCountry=parte3(peliculas$star,peliculas$country)
directorPerCountry=parte3(peliculas$director,peliculas$country)

writersPerGenre
starPerCountry
directorPerCountry



