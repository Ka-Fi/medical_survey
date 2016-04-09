# if (computer.name == "KASIA-KOMPUTER") 
#   wd.tmp <- "C:\\Users\\Kasia\\Desktop\\badania"
# # dodaj swoje working directory
# 
# setwd(wd.tmp)


anova_all <- read.csv("anova_all.csv", sep = ",")
anova_all <-anova_all[,2:6]

sp_all <- read.csv("statystyki_podstawowe_all.csv", sep = ",")
sp_all <-sp_all[,2:8]

wl_all <- read.csv("wilcoxon_all.csv", sep = ",")
wl_all <-wl_all[,2:6]
wl_all_roznice <- read.csv("wilcoxon_all_roznice.csv", sep = ",")
wl_all_roznice <-wl_all_roznice[,2:6]


anova_all_new <- read.csv("anova_all_new.csv", sep = ",")
anova_all_new <-anova_all_new[,2:6]

sp_all_new <- read.csv("statystyki_podstawowe_all_new.csv", sep = ",")
sp_all_new <-sp_all_new[,2:8]

wl_all_new <- read.csv("wilcoxon_all_new.csv", sep = ",")
wl_all_new <-wl_all_new[,2:6]
wl_all_roznice_new <- read.csv("wilcoxon_all_roznice_new.csv", sep = ",")
wl_all_roznice_new <-wl_all_roznice_new[,2:6]