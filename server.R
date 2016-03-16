
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
#----------------------------------------------------------------------------------  
#------------------------------infobox---------------------------------------------
#----------------------------------------------------------------------------------   
  output$wszyscy <- renderValueBox({
    if (input$metoda=="all") {
      valueBox(length(dane$metoda)/2,strong("liczba badanych"),color = "blue")
    }
    else{
      valueBox(sum(dane$metoda==input$metoda)/2,strong("liczba badanych"),color = "blue")
    }
  })
  
  output$kobiety<- renderValueBox({
    if (input$metoda=="all") {
      valueBox(paste0(100*round(sum(dane$plec==1 )/length(dane$metoda),digits = 2),"%")
               ,strong("liczba kobiet w badaniu")
               ,color = "blue")
    }
    else{
      valueBox(paste0(100*round(sum(dane$plec==1 & dane$metoda==input$metoda)/sum(dane$metoda==input$metoda),digits = 2),"%"),strong("liczba kobiet w badaniu"),color = "blue")
    }
  })
  
  output$mezczyzni<- renderValueBox({
    if (input$metoda=="all") {
      valueBox(paste0(100*round(sum(dane$plec==0 )/length(dane$metoda),digits = 2),"%")
               ,strong("liczba kobiet w badaniu")
               ,color = "blue")
    }
    else{
      valueBox(paste0(100*round(sum(dane$plec==0 & dane$metoda==input$metoda)/sum(dane$metoda==input$metoda),digits = 2),"%"),strong("liczba kobiet w badaniu"),color = "blue")
    }
  })  
  
  output$wszyscy2 <- renderValueBox({
      valueBox(dim(dane_new)[1]/2,strong("liczba badanych"),color = "blue")
  })
  dz<-function(dane){
    if (dane=="wilcoxon")  {dz<-c("0-2","0-90","2-90")}
    if (dane=="anova")  {dz<-c("0-2-90")}
    
    return (dz)
  }  
  
  output$czas <- renderUI(function() {
    if(input$sbMenu == "dodatkowe_badania" || input$sbMenu == "dashboard" ){
      selectInput("czas", 
                  "Analizowany czas:", 
                  choices = dz(input$typ))
    }
  })
  
  
  
  output$zmienna <- renderUI(function() {
    if (input$sbMenu == "dashboard"){ 
      selectInput("zmienna","Czynniki",choices = c("ConeCzK"
                                                   ,"RodCzK"
                                                   ,"RodA"
                                                   ,"MRodCzK"
                                                   ,"MRodA"  
                                                   ,"objawy"
                                                   ,"ConeAa"
                                                   ,"ConeAb" 
                                                   ,"FlCzK" 
                                                   ,"FlA" 
                                                   ,"OsPA"  
                                                   ,"MDSAP" 
                                                   ,"PSDSAP"
                                                   ,"MDFDF"   
                                                   ,"PSDFDF"
                                                   ,"RB"))
      
      
      
    }
    else if (input$sbMenu == "dodatkowe_badania"){ 
      selectInput("zmienna_new","Czynniki_new",choices = c( "Alow" 
                                                            ,"OsPA_A_O1"
                                                            ,"OsPA_A_O2"
                                                            ,"OsPA_A_O3"
                                                            ,"OsPA_A_O4"
                                                            ,"OsPA_A_n1"
                                                            ,"OsPA_A_n2"
                                                            ,"OsPA_A_n3" 
                                                            ,"OsPA_A_n4"))
      
      
      
    }
  })
  output$metoda<-renderUI(function(){
    
    
    if(input$sbMenu == "dashboard" ){
      
      selectInput("metoda", "Metoda badania", choices = c("metoda nr 1"="metoda_1"
                                                          ,"metoda nr 2"="metoda_2"
                                                          ,"wszyscy"="all"))
      
    }
  })
  
  output$regresja<-renderUI(function(){
    
    
    if(input$sbMenu == "dodatkowe_badania" || input$sbMenu == "dashboard" ){
      
      selectInput("regresja", "Linia regresji", choices = c("Tak"
                                                            ,"Nie"))
      
    }
  })  
  output$typ<-renderUI(function(){
    
    
    if(input$sbMenu == "dodatkowe_badania" || input$sbMenu == "dashboard" ){
      
      radioButtons("typ","Typ analizy",choices = c("wilcoxon","anova"))
      
    }
  })    

  
  #----------------------------------------------------------------------------------  
  #------------------------------plot------------------------------------------------
  #----------------------------------------------------------------------------------
  output$wykres2 <- renderUI(function(){
    if (input$typ=="wilcoxon"){plotOutput("plot2")}
    else {h4("dopisać")}
  })
  output$plot1 <- renderPlot({
    var_1 <- paste(input$zmienna, badanie[1], sep = "") 
    var_2 <- paste(input$zmienna, badanie[2], sep = "")
    var_3 <- paste(input$zmienna, badanie[3], sep = "")
    if (input$czas=="0-2"){
      box_2_plot(dane, var_1, var_2,input$metoda)
    } else if (input$czas=="0-90"){
      box_2_plot(dane, var_1, var_3,input$metoda)
    }else if (input$czas=="2-90"){
      box_2_plot(dane, var_2, var_3,input$metoda)
    }else if (input$czas=="0-2-90"){
      box_3_plot(dane, var_1, var_2, var_3)
    }
    
  })
  output$plot2 <- renderPlot({
    var_1 <- paste(input$zmienna, badanie[1], sep = "") 
    var_2 <- paste(input$zmienna, badanie[2], sep = "")
    var_3 <- paste(input$zmienna, badanie[3], sep = "")
    
    if (input$czas=="0-2"){
      if (input$metoda=="all"){
        punkt_2_plot(dane,var_1, var_2, czy_regresja = (input$regresja == "Tak"))}
      else{
        punkt_2_plot(dane,var_1, var_2, metoda_ = input$metoda, 
                     czy_regresja = (input$regresja == "Tak"))}
    } else if (input$czas=="0-90"){
      if (input$metoda=="all"){punkt_2_plot(dane,var_1, var_3, czy_regresja =  (input$regresja == "Tak"))}
      else{
        punkt_2_plot(dane,var_1, var_3, metoda_ = input$metoda, 
                     czy_regresja =  (input$regresja == "Tak"))}
    }else if (input$czas=="2-90"){
      if (input$metoda=="all"){
        punkt_2_plot(dane,var_2, var_3, 
                     czy_regresja =  (input$regresja == "Tak"))
      }
      else{
        punkt_2_plot(dane,var_2, var_3, metoda_ = input$metoda, 
                     czy_regresja =  (input$regresja == "Tak"))}
    }
#     else if (input$czas=="0-2-90"){
#       
#     }
  })
  output$plot3 <- renderPlot({
    var_1 <- paste(input$zmienna_new, badanie[1], sep = "") 
    var_2 <- paste(input$zmienna_new, badanie[2], sep = "")
    var_3 <- paste(input$zmienna_new, badanie[3], sep = "")
    
    if (input$czas=="0-2"){
      box_2_plot(dane_new, var_1, var_2)
    } else if (input$czas=="0-90"){
      box_2_plot(dane_new, var_1, var_3)
    }else if (input$czas=="2-90"){
      box_2_plot(dane_new, var_2, var_3)
    }else if (input$czas=="0-2-90"){
      box_3_plot(dane_new, var_1, var_2, var_3)
    }
  })
  output$plot4 <- renderPlot({
    var_1 <- paste(input$zmienna_new, badanie[1], sep = "") 
    var_2 <- paste(input$zmienna_new, badanie[2], sep = "")
    var_3 <- paste(input$zmienna_new, badanie[3], sep = "")
    
    if (input$czas=="0-2"){
      if (input$metoda=="all"){
        punkt_2_plot(dane_new,var_1, var_2, czy_regresja = (input$regresja == "Tak"))}
      else{
        punkt_2_plot(dane_new,var_1, var_2, metoda_ = input$metoda, 
                     czy_regresja = (input$regresja == "Tak"))}
    } else if (input$czas=="0-90"){
      if (input$metoda=="all"){punkt_2_plot(dane_new,var_1, var_3, czy_regresja =  (input$regresja == "Tak"))}
      else{
        punkt_2_plot(dane_new,var_1, var_3, metoda_ = input$metoda, 
                     czy_regresja =  (input$regresja == "Tak"))}
    }else if (input$czas=="2-90"){
      if (input$metoda=="all"){
        punkt_2_plot(dane_new,var_2, var_3, 
                     czy_regresja =  (input$regresja == "Tak"))
      }
      else{
        punkt_2_plot(dane_new,var_2, var_3, metoda_ = input$metoda, 
                     czy_regresja =  (input$regresja == "Tak"))}
    }else if (input$czas=="0-2-90"){
      box_3_plot(dane_new, var_1, var_2, var_3)
    }
  })
 
  #----------------------------------------------------------------------------------  
  #------------------------------table-----------------------------------------------
  #----------------------------------------------------------------------------------   
  
  
  tab<-function(dane){
    
    if (dane=="anova_all")  {tab<-anova_all}
    if (dane=="sp_all")  {tab<-sp_all}
    if (dane=="wl_all")  {tab<-wl_all}
    if (dane=="wl_all_roznice")  {tab<-wl_all_roznice}
    if (dane=="anova_all_new")  {tab<-anova_all_new}
    if (dane=="sp_all_new")  {tab<-sp_all_new}
    if (dane=="wl_all_new")  {tab<-wl_all_new}
    if (dane=="wl_all_roznice_new")  {tab<-wl_all_roznice_new}
    return (tab)
  } 
  
  output$table1 <- DT::renderDataTable({
    datatable(tab(input$tabele))
  }) 
  output$table2 <- DT::renderDataTable({
    datatable(tab(input$tabele2))
  }) 
})
