cbPalette <- c("#99ccff", "#3399ff","#0066cc")




wilcox_3_test <- function(var, data_, badanie, group = "all"){
  if(group != "all")
    data_ <- data_ %>% filter(grupa == group)
  var_1 <- paste(var, badanie[1], sep = "") 
  var_2 <- paste(var, badanie[2], sep = "")
  var_3 <- paste(var, badanie[3], sep = "")
  ind <- which(is.na(data_[, var_1]))
  if(length(ind) == 0){
    b_0_2 <- wilcox.test(data_[, var_1], data_[, var_2])
    b_0_90 <- wilcox.test(data_[, var_1], data_[, var_3])
    b_2_90 <- wilcox.test(data_[, var_2], data_[, var_3])
  }
  else{
    b_0_2 <- wilcox.test(data_[, var_1][-ind], data_[, var_2][-ind])
    b_0_90 <- wilcox.test(data_[, var_1][-ind], data_[, var_3][-ind])
    b_2_90 <- wilcox.test(data_[, var_2][-ind], data_[, var_3][-ind])
  }
  stat<-data.frame(b_0_2$statistic,
                   b_0_90$statistic,
                   b_2_90$statistic)
  p_val <- data.frame(round(b_0_2$p.value, 3),
                      round(b_0_90$p.value, 3),
                      round(b_2_90$p.value, 3))
  if(length(ind) == 0)
    n_group <- data_ %>% summarise(n())
  else
    n_group <- data_[-ind,] %>% summarise(n())
  test <- c("0-2", "0-90", "2-90")
  wynik <- data.frame(p_val=as.numeric(p_val), v_stat = as.numeric(stat), test = test)
  var = data.frame(var = rep(var,3))
  wynik$n_group <- as.numeric(n_group)
  wynik$group <- group
  wynik <- cbind(var, wynik)
  return(wynik)
}


wilcox_all<- function(czynniki, data_, badanie, group = "all"){
  wynik <- data.frame(var = "0", p_val = 0, v_stat = 0, test = 0, n_group = 0, group = 0)
  for(czynnik in czynniki){
    tmp <- wilcox_3_test(czynnik, dane, badanie, group)
    wynik <- rbind(wynik, tmp)
  }  
  return(wynik[-1,])
}


anova_3_test <- function(var, data_, badanie, group = "all"){
  if(group != "all")
    data_ <- data_ %>% filter(grupa == group)
  var_1 <- paste(var, badanie[1], sep = "") 
  var_2 <- paste(var, badanie[2], sep = "")
  var_3 <- paste(var, badanie[3], sep = "")
  ind <- which(is.na(data_[, var_1]))
  if(length(ind) == 0){
    b_0_2_90 <- friedman.test(cbind(data_[, var_1], 
                                    data_[, var_2], 
                                    data_[, var_3]))
  }
  else{
    b_0_2_90 <- friedman.test(cbind(data_[, var_1][-ind], 
                                    data_[, var_2][-ind], 
                                    data_[, var_3][-ind]))
  }
  stat<-data.frame(b_0_2_90$statistic)
  p_val <- data.frame(round(b_0_2_90$p.value, 3))
  if(length(ind) == 0)
    n_group <- data_ %>% summarise(n())
  else
    n_group <- data_[-ind,] %>% summarise(n())
  test <- c("0-2-90")
  wynik <- data.frame(p_val=as.numeric(p_val), v_stat = as.numeric(stat), test = test)
  var = data.frame(var = var)
  wynik$n_group <- as.numeric(n_group)
  wynik$group <- group
  wynik <- cbind(var, wynik)
  return(wynik)
}  


anova_all <- function(czynniki, data_, badanie, group = "all"){
  wynik <- data.frame(var = "0", p_val = 0, v_stat = 0, test = 0, n_group = 0, group = 0)
  for(czynnik in czynniki){
    tmp <- anova_3_test(czynnik, dane, badanie, group)
    wynik <- rbind(wynik, tmp)
  }  
  return(wynik[-1,])
}


wilcox_3_test_roznice <- function(var, data_, badanie){
  met_1_b_0 <- data_ %>% filter(metoda == 'metoda_1' & badane == 0)
  met_1_b_1 <- data_ %>% filter(metoda == 'metoda_1' & badane == 1)
  dane_met_1 <- met_1_b_0 %>% left_join(met_1_b_1, by = "pacjent")
  
  met_2_b_0 <- data_ %>% filter(metoda == 'metoda_2' & badane == 0)
  met_2_b_1 <- data_ %>% filter(metoda == 'metoda_2' & badane == 1)
  dane_met_2 <- met_2_b_0 %>% left_join(met_2_b_1, by = "pacjent")
  
  var_1x <- paste(var, badanie[1], ".x", sep = "") 
  var_2x <- paste(var, badanie[2], ".x", sep = "")
  var_3x <- paste(var, badanie[3], ".x", sep = "")
  var_1y <- paste(var, badanie[1], ".y", sep = "") 
  var_2y <- paste(var, badanie[2], ".y", sep = "")
  var_3y <- paste(var, badanie[3], ".y", sep = "")
  
  b_0_2m1 <- wilcox.test(dane_met_1[, var_2x] - dane_met_1[, var_1x], 
                         dane_met_1[, var_2y] - dane_met_1[, var_1y])
  b_0_90m1 <- wilcox.test(dane_met_1[, var_3x] - dane_met_1[, var_1x], 
                          dane_met_1[, var_3y] - dane_met_1[, var_1y])
  b_2_90m1 <- wilcox.test(dane_met_1[, var_3x] - dane_met_1[, var_2x], 
                          dane_met_1[, var_3y] - dane_met_1[, var_2y])
  
  b_0_2m2 <- wilcox.test(dane_met_2[, var_2x] - dane_met_2[, var_1x], 
                         dane_met_2[, var_2y] - dane_met_2[, var_1y])
  b_0_90m2 <- wilcox.test(dane_met_2[, var_3x] - dane_met_2[, var_1x], 
                          dane_met_2[, var_3y] - dane_met_2[, var_1y])
  b_2_90m2 <- wilcox.test(dane_met_2[, var_3x] - dane_met_2[, var_2x], 
                          dane_met_2[, var_3y] - dane_met_2[, var_2y])
  
  stat_m1 <- data.frame(b_0_2m1$statistic,
                        b_0_90m1$statistic,
                        b_2_90m1$statistic)
  p_val_m1 <- data.frame(round(b_0_2m1$p.value, 3),
                         round(b_0_90m1$p.value, 3),
                         round(b_2_90m1$p.value, 3))
  
  stat_m2 <- data.frame(b_0_2m2$statistic,
                        b_0_90m2$statistic,
                        b_2_90m2$statistic)
  p_val_m2 <- data.frame(round(b_0_2m2$p.value, 3),
                         round(b_0_90m2$p.value, 3),
                         round(b_2_90m2$p.value, 3))
  
  n_group_m1 <- dane_met_1 %>% summarise(n())
  n_group_m2 <- dane_met_2 %>% summarise(n())
  test <- rep(c("0-2", "0-90", "2-90"),2)
  wynik <- data.frame(p_val=c(as.numeric(p_val_m1),
                              as.numeric(p_val_m2)),
                      v_stat = c(as.numeric(stat_m1),
                                 as.numeric(stat_m2)), test = test)
  var = data.frame(var = rep(var,6))
  wynik$n_group <- c(rep(as.numeric(n_group_m1),3),
                     rep(as.numeric(n_group_m2),3))
  wynik$metoda <- c(rep("metoda_1",3), rep("metoda_2",3))
  wynik <- cbind(var, wynik)
  return(wynik)
}


wilcox_all_roznice<- function(czynniki, data_, badanie){
  wynik <- data.frame(var = "0", p_val = 0, v_stat = 0, test = 0, n_group = 0, metoda = 0)
  for(czynnik in czynniki){
    tmp <- wilcox_3_test_roznice(czynnik, dane, badanie)
    wynik <- rbind(wynik, tmp)
  }  
  return(wynik[-1,])
}


statstyki_zmiennych <- function(var, data_, badanie, group = "metoda_1"){
  if(group != "all")
    data_ <- data_ %>% filter(grupa == group)
  var_1 <- paste(var, badanie[1], sep = "") 
  var_2 <- paste(var, badanie[2], sep = "")
  var_3 <- paste(var, badanie[3], sep = "")
  ind <- which(is.na(data_[, var_1]))
  stats_0 <- c(summary(data_[,var_1])[c(1,3,4,6)], 
               sd = sd(na.omit(data_[,var_1])))
  stats_0 <- as.data.frame(t(stats_0))
  stats_2 <- as.data.frame(t(c(summary(data_[,var_2])[c(1,3,4,6)], 
                               sd = sd(na.omit(data_[,var_2])))))
  stats_90 <- as.data.frame(t(c(summary(data_[,var_3])[c(1,3,4,6)], 
                                sd = sd(na.omit(data_[,var_3])))))
  wynik <- rbind(stats_0, stats_2, stats_90)
  wynik$czas <- c(0,2,90)
  wynik$grupa <- group
  wynik$czynnik <- var
  return(wynik)
}


statstyki_zmiennych_all<- function(czynniki, data_, badanie, group = "all"){
  wynik <- statstyki_zmiennych(czynniki[13], dane, badanie)
  for(czynnik in czynniki){
    tmp <- statstyki_zmiennych(czynnik, dane, badanie, group)
    wynik <- rbind(wynik, tmp)
  }  
  return(wynik[-c(1,2,3),])
}


box_2_plot <- function(df, var_1, var_2, group = "all"){
  if(group != "all")
    {df<-df[df$metoda==group,]}
    # {df <- df %>% filter(grupa == group) }
  values <- c(df[, var_1], df[,var_2])
  names <- factor(c(rep(var_1, length(df[,var_1])),
                    rep(var_2, length(df[,var_2]))))
  temp_df <- data.frame(values=values, names=names)
  p <- ggplot(temp_df, aes(names, values))
  p <- p + geom_boxplot(aes(fill = names)) + geom_jitter()
  p <- p+ theme(legend.justification=c(1,0), legend.position=c(1,0))
  p <- p + xlab("czynniki") +ylab("wartosci") 
  p <- p+scale_fill_discrete(name="Czynniki")+scale_fill_manual(values=cbPalette)  


  
  return(p)
}


box_3_plot <- function(df, var_1, var_2, var_3, group = "all"){
  if(group != "all")
  {df<-df[df$metoda==group,]}
    # df <- df %>% filter(grupa == group) 
  values <- c(df[, var_1], df[,var_2], df[,var_3])
  names <- factor(c(rep(var_1, length(df[,var_1])),
                    rep(var_2, length(df[,var_2])),
                    rep(var_3, length(df[,var_3]))))
  temp_df <- data.frame(values=values, names=names)
  p <- ggplot(temp_df, aes(names, values))
  p <- p + geom_boxplot(aes(fill = names)) + geom_jitter()
  p <- p+ theme(legend.justification=c(1,0), legend.position=c(1,0)) + scale_fill_manual(values=cbPalette)
  
  return(p)
}


box_3_plot_all <- function(df, czynniki, badanie,group = "all"){
  for(czynnik in czynniki){
    var_1 <- paste(czynnik, badanie[1], sep = "") 
    var_2 <- paste(czynnik, badanie[2], sep = "")
    var_3 <- paste(czynnik, badanie[3], sep = "")
    p <- box_3_plot(dane, var_1, var_2, var_3, group)
    p
    ggsave(filename = paste("grupa_", group, "_wykresy_boxplot_3/", 
                            czynnik,"0-2-90.png", sep = ""))
  }
}


box_2_plot_all <- function(df, czynniki, badanie, group = "all"){
  for(czynnik in czynniki){
    var_1 <- paste(czynnik, badanie[1], sep = "")
    var_2 <- paste(czynnik, badanie[2], sep = "")
    var_3 <- paste(czynnik, badanie[3], sep = "")
    p1 <- box_2_plot(dane, var_1, var_2, group)
    ggsave(filename = paste("grupa_", group, "_wykresy_boxplot_2/", 
                            czynnik,"0-2.png", sep = ""))
    p2 <- box_2_plot(dane, var_1, var_3, group)
    ggsave(filename = paste("grupa_", group, "_wykresy_boxplot_2/", 
                            czynnik,"0-90.png", sep = ""))
    p3 <- box_2_plot(dane, var_2, var_3, group)
    ggsave(filename = paste("grupa_", group, "_wykresy_boxplot_2/", 
                            czynnik,"2-90.png", sep = ""))
  }
}


box_2_plot_roznice <- function(data_, var, badanie, metoda_ = 'metoda_1'){
  met_1_b_0 <- data_ %>% filter(metoda == metoda_ & badane == 0)
  met_1_b_1 <- data_ %>% filter(metoda == metoda_ & badane == 1)
  dane_met_1 <- met_1_b_0 %>% left_join(met_1_b_1, by = "pacjent")
  
  var_1x <- paste(var, badanie[1], ".x", sep = "") 
  var_2x <- paste(var, badanie[2], ".x", sep = "")
  var_3x <- paste(var, badanie[3], ".x", sep = "")
  var_1y <- paste(var, badanie[1], ".y", sep = "") 
  var_2y <- paste(var, badanie[2], ".y", sep = "")
  var_3y <- paste(var, badanie[3], ".y", sep = "")
  
  df_0_2 <- data.frame(values = c((dane_met_1[, var_2x] - dane_met_1[, var_1x]), 
                                  (dane_met_1[, var_2y] - dane_met_1[, var_1y])))
  df_0_2$names <- c(rep(paste(var, "_0-2_badany", sep = ""),
                        length(dane_met_1[, var_2x])),
                    rep(paste(var, "_0-2_niebadany", sep = ""),
                        length(dane_met_1[, var_2y])))
  p1 <- ggplot(df_0_2, aes(names, values))
  p1 <- p1 + geom_boxplot(aes(fill = names)) + geom_jitter()
  
  df_0_90 <- data.frame(values = c((dane_met_1[, var_3x] - dane_met_1[, var_1x]), 
                                   (dane_met_1[, var_3y] - dane_met_1[, var_1y])))
  df_0_90$names <- c(rep(paste(var, "_0-90_badany", sep = ""),
                         length(dane_met_1[, var_3x])),
                     rep(paste(var, "_0-90_niebadany", sep = ""),
                         length(dane_met_1[, var_3y])))
  p2 <- ggplot(df_0_90, aes(names, values))
  p2 <- p2 + geom_boxplot(aes(fill = names)) + geom_jitter()
  
  df_2_90 <- data.frame(values = c((dane_met_1[, var_3x] - dane_met_1[, var_2x]), 
                                   (dane_met_1[, var_3y] - dane_met_1[, var_2y])))
  df_2_90$names <- c(rep(paste(var, "_2-90_badany", sep = ""),
                         length(dane_met_1[, var_2x])),
                     rep(paste(var, "_2-90_niebadany", sep = ""),
                         length(dane_met_1[, var_2y])))
  p3 <- ggplot(df_2_90, aes(names, values))
  p3 <- p3 + geom_boxplot(aes(fill = names)) + geom_jitter()
  return(list(p1 = p1, p2 = p2, p3 = p3))
}


box_2_plot_roznice_all <- function(data_, czynniki, badanie,
                                   metoda = "metoda_1"){
  for(czynnik in czynniki){
    ploty <- box_2_plot_roznice(data_, czynnik, badanie, metoda)
    ploty$p1
    ggsave(filename = paste(metoda, "_wykresy_boxplot_2/", 
                            czynnik,"2-0.png", sep = ""), ploty$p1)
    ploty$p2
    ggsave(filename = paste(metoda, "_wykresy_boxplot_2/", 
                            czynnik,"90-0.png", sep = ""), ploty$p2)
    ploty$p3
    ggsave(filename = paste(metoda, "_wykresy_boxplot_2/", 
                            czynnik,"90-2.png", sep = ""), ploty$p3)
  }
}


punkt_2_plot <- function(data_, var_1, var_2, metoda_ = "all", 
                         czy_regresja = TRUE){
  df=data_
  if(metoda_ != "all")
  # {df<-df[df$metoda==group,]}
     {df <- data_ %>% filter(metoda == metoda_)}
  df$badane <- as.factor(df$badane)
  p <- ggplot(df, aes_string(var_1, var_2, color = "badane"))
  p <- p + geom_point(aes(size = 1.1)) + geom_jitter() + 
    scale_size(guide="none")   
  if( czy_regresja==TRUE) 
    p <- p + stat_smooth(method=lm, fullrange=TRUE, se = FALSE) 
  p <- p+ theme(legend.justification=c(1,0), legend.position=c(1,0)) 
  p<-p+ scale_color_manual(values=cbPalette)
  return(p)
}

# punkt_2_plot(dane, var_1, var_2, "metoda_2", TRUE)
# punkt_2_plot(dane, var_1, var_2, "metoda_2", FALSE)

#Plotowanie wszystkich boxplotĂłw i zapis do pliku
# box_3_plot_all(dane, czynniki, badanie, 1)
# box_3_plot_all(dane, czynniki, badanie, 2)
# box_3_plot_all(dane, czynniki, badanie, 5)
# box_3_plot_all(dane, czynniki, badanie, 0)
# box_3_plot_all(dane, czynniki, badanie)
# box_2_plot_all(dane, czynniki, badanie, 1)
# box_2_plot_all(dane, czynniki, badanie, 2)
# box_2_plot_all(dane, czynniki, badanie, 5)
# box_2_plot_all(dane, czynniki, badanie, 0)
# box_2_plot_all(dane, czynniki, badanie)
# 
# 
# box_2_plot_roznice_all(dane,czynniki, badanie, "metoda_1")
# box_2_plot_roznice_all(dane,czynniki, badanie, "metoda_2")
# 
# 
# 
# # Testy wilcox dla rĂłĹĽnic
# wilcox_all_roznice(czynniki, dane, badanie)
# 
# #Testy wilcoxona w rĂłĹĽnych grupach
# wilcox_all(czynniki, dane, badanie)
# wilcox_all(czynniki, dane, badanie, 1)
# wilcox_all(czynniki, dane, badanie, 2)
# wilcox_all(czynniki, dane, badanie, 5)
# wilcox_all(czynniki, dane, badanie, 0)
# 
# #Testy anova dla rĂłĹĽnych grup
# anova_all(czynniki, dane, badanie)
# anova_all(czynniki, dane, badanie, 1)
# anova_all(czynniki, dane, badanie, 2)
# anova_all(czynniki, dane, badanie, 5)
# anova_all(czynniki, dane, badanie, 0)
# 
# #Podstawowe statystyki
# statstyki_zmiennych_all(czynniki, dane, badanie)
# statstyki_zmiennych_all(czynniki, dane, badanie,1)
# statstyki_zmiennych_all(czynniki, dane, badanie,2)
# statstyki_zmiennych_all(czynniki, dane, badanie,0)
# statstyki_zmiennych_all(czynniki, dane, badanie,5)
# 
# 
# # Pojedyncze ploty boxplotĂłw
# box_3_plot(dane, var_1, var_2, var_3, 5)
# box_2_plot(dane, var_1, var_2, 1)
# #Pojedyncze przykĹ‚adu uĹĽycia wilcoxon oraz Anova
# wilcox_3_test_roznice(czynniki[13], dane, badanie, group = 5)
# wilcox_3_test(czynniki[13], dane, badanie, group = 5)
# anova_3_test(czynniki[13], dane, badanie, group = 5)
# statstyki_zmiennych(czynniki[13], dane, badanie,1)
