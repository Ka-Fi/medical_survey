# computer.name <- Sys.info()["nodename"] 
# if (computer.name == "JIDZIAK-L") 
#   wd.tmp <- "C:\\Users\\jidziak\\Desktop\\research medyczny"
# if (computer.name == "JANUSZ") 
#   wd.tmp <- "C:\\Users\\iWindows\\Desktop\\research medyczny"
# if (computer.name == "KASIA-KOMPUTER") 
#   wd.tmp <- "C:\\Users\\Kasia\\Desktop\\nowa_appka"
# # dodaj swoje working directory

# setwd(wd.tmp)

library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)

substrRight <- function(x){
  substr(x, 1, nchar(x) - 2)
}



# Wczytywanie danych
dane <- read.csv("dane_medyczne.csv", sep = ";")
dane$pacjent <- rep(1:35, rep(2,35))
# Dodawanie zmiennych (podzial zmienenj grupa na dwie niezalezne)
badane <- sapply(dane$grupa, function(x) 
  ifelse(x == 1 || x == 2, 1, 0))
metoda <- sapply(dane$grupa, function(x) 
  ifelse(x == 1 || x == 5, "metoda_1", 
         ifelse(x == 0 || x == 2, "metoda_2","-")))

dane <- cbind(badane, metoda, dane)
index <- seq(from = 7, to = 54, by = 3)
zmienne <- names(dane)
czynniki <- substrRight(zmienne[index])
badanie <- c("_0", "_2", "_90")

# Wybor zmiennych do badania
var_1 <- paste(czynniki[5], badanie[1], sep = "") 
var_2 <- paste(czynniki[5], badanie[2], sep = "")
var_3 <- paste(czynniki[5], badanie[3], sep = "")

ggplot(dane, aes_string(var_1)) + geom_histogram(bins = 6)
