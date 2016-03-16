library(dplyr)
dane_new <- read.csv("Dane_analiza_amplitudyM_rod_ospa.csv", sep=";")

substrRight <- function(x){
  substr(x, 1, nchar(x) - 2)
}

badane <- sapply(dane_new$grupa, function(x) 
  ifelse(x == 1 || x == 2, 1, 0))
metoda <- sapply(dane_new$grupa, function(x) 
  ifelse(x == 1 || x == 5, "metoda_1", 
         ifelse(x == 0 || x == 2, "metoda_2","-")))

dane_new <- cbind(badane, metoda, dane_new)
dane_new$pacjent <- rep(1:35, rep(2,35))

index_new <- seq(from = 5, to = 29, by = 3)
zmienne_new <- names(dane_new)
czynniki_new <- substrRight(zmienne_new[index_new])
badanie <- c("_0", "_2", "_90")


var_1 <- paste(czynniki_new[5], badanie[1], sep = "") 
var_2 <- paste(czynniki_new[5], badanie[2], sep = "")
var_3 <- paste(czynniki_new[5], badanie[3], sep = "")

# punkt_2_plot(dane_new, var_1, var_2, "metoda_2", TRUE)
# box_3_plot_all(dane_new, czynniki_new, badanie, 1)
# box_3_plot_all(dane_new, czynniki_new, badanie, 2)
# box_3_plot_all(dane_new, czynniki_new, badanie, 5)
# box_3_plot_all(dane_new, czynniki_new, badanie, 0)
# box_3_plot_all(dane_new, czynniki_new, badanie)
 # box_2_plot(dane_new, var_1, var_2, 1)
# box_2_plot_all(dane_new, czynniki_new, badanie, 2)
# box_2_plot_all(dane_new, czynniki_new, badanie, 5)
# box_2_plot_all(dane_new, czynniki_new, badanie, 0)
# box_2_plot_all(dane_new, czynniki_new, badanie)
# 
# box_2_plot_roznice_all(dane_new,czynniki_new, badanie, "metoda_2")
# box_2_plot_roznice_all(dane_new,czynniki_new, badanie, "metoda_1")
# 
# 
# 
# q <- wilcox_all(czynniki_new, dane_new, badanie)
# write.csv(q, "wilcoxon_all.csv")
# q <- wilcox_all(czynniki_new, dane_new, badanie, 1)
# write.csv(q, "wilcoxon_1.csv")
# q <- wilcox_all(czynniki_new, dane_new, badanie, 2)
# write.csv(q, "wilcoxon_2.csv")
# q <- wilcox_all(czynniki_new, dane_new, badanie, 5)
# write.csv(q, "wilcoxon_5.csv")
# q <- wilcox_all(czynniki_new, dane_new, badanie, 0)
# write.csv(q, "wilcoxon_0.csv")
# 
# #Testy anova dla różnych grup
# q <- anova_all(czynniki_new, dane_new, badanie)
# write.csv(q, "anova_all.csv")
# q <- anova_all(czynniki_new, dane_new, badanie, 1)
# write.csv(q, "anova_1.csv")
# q <- anova_all(czynniki_new, dane_new, badanie, 2)
# write.csv(q, "anova_2.csv")
# q <- anova_all(czynniki_new, dane_new, badanie, 5)
# write.csv(q, "anova_5.csv")
# q <- anova_all(czynniki_new, dane_new, badanie, 0)
# write.csv(q, "anova_0.csv")
# 
# #Podstawowe statystyki
# q <- statstyki_zmiennych_all(czynniki_new, dane_new, badanie)
# write.csv(q, "statystyki_podstawowe_all.csv")
# q <- statstyki_zmiennych_all(czynniki_new, dane_new, badanie,1)
# write.csv(q, "statystyki_podstawowe_1.csv")
# q <- statstyki_zmiennych_all(czynniki_new, dane_new, badanie,2)
# write.csv(q, "statystyki_podstawowe_2.csv")
# q <- statstyki_zmiennych_all(czynniki_new, dane_new, badanie,0)
# write.csv(q, "statystyki_podstawowe_0.csv")
# q <- statstyki_zmiennych_all(czynniki_new, dane_new, badanie,5)
# write.csv(q, "statystyki_podstawowe_5.csv")
# 
# q <-  wilcox_all_roznice(czynniki_new, dane_new, badanie)
# write.csv(q, ".\\wilcoxon_all_roznice.csv")