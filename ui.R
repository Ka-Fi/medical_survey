
 source("conf_final.R")
 source("funkcje_final.R")
 source("testy.R")
 source("dodatkowe_badania_boxploty.R")
library(shiny)
library(shinydashboard)
dashboardPage(
  dashboardHeader(title = "Dane medyczne"),
  dashboardSidebar(
    sidebarMenu(
      id = "sbMenu",
      menuItem("Analiza czynnikow", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("dodatkowe badania", tabName = "dodatkowe_badania", icon = icon("dashboard")),
      menuItem("Table wynikowe", tabName = "widgets", icon = icon("th")),
      menuItem("badanie aparatura",tabName = "Aparatura",icon = icon("line-chart")),
      menuItem("metody statystyczne",tabName = "Metody",icon = icon("users"))
      
    ),
    uiOutput("typ"),
    uiOutput("zmienna"),
    uiOutput("metoda"),
    uiOutput("regresja"),
    uiOutput("czas")
    
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
        dashboardBody
.main-header {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                              }
                              .main-header .logo{
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                              }
                              .content-wrapper,
                              .right-side {background-color: #e6e6e6;
                              }


                              '))
    ),
    
    tabItems(
      tabItem(tabName = "dodatkowe_badania",
              fluidRow(
                valueBoxOutput("wszyscy2"),
                fluidRow(
                  box(h3(strong("Boxplot")),plotOutput("plot3"),background = "light-blue",width = 6)
                  # box(h3(strong("Regresja")),plotOutput("plot4"),background = "light-blue",width = 6)
                )
                
                )),
      # First tab content
      tabItem(tabName = "dashboard",
              fixedRow(
                # valueBox(length(dane$plec),"liczba badanych",color = "aqua"),
                valueBoxOutput("wszyscy"),
                valueBoxOutput("kobiety"),
                valueBoxOutput("mezczyzni")
              ),
              fluidRow(
                box(h3(strong("Boxplot")),plotOutput("plot1"),background = "light-blue",width = 6),
                # uiOutput("wykres2")
                 box(h3(strong("Regresja")),uiOutput("wykres2"),background = "light-blue",width = 6)
                    # plotOutput("plot2"),background = "light-blue",width = 6)
              )
              
              
      ),
      # Second tab content
      tabItem(tabName = "widgets",
              fluidRow(
                fluidRow(box(background = "blue"
                             ,selectInput("tabele","Tabele dla danych starych"
                                          ,choices = c("Anova"="anova_all"
                                                       ,"Statystyki podstawowe"="sp_all"
                                                       ,"Wilcoxon"="wl_all"
                                                       ,"Wilcoxon różnice"="wl_all_roznice"))),
                    box(background = "blue"
                              ,selectInput("tabele2","Tabele dla danych nowych"
                                           ,choices = c("Anova"="anova_all_new"
                                                        ,"Statystyki podstawowe"="sp_all_new"
                                                        ,"Wilcoxon"="wl_all_new"
                                                        ,"Wilcoxon różnice"="wl_all_roznice_new")))),
                fluidRow(
                  box(DT::dataTableOutput("table1")),
                  box(DT::dataTableOutput("table2"))
                )
                
              )
      ),
      #Third tab content
      tabItem(tabName = "Aparatura",
              
              h3("Aparatura pomiarowa"),
              img(src="banner.png", height = 150, width = 150),
              img(src="oczy.jpg", height = 150, width = 150)),
      tabItem(tabName = "Metody",fluidRow(
        fluidRow(box(h3("Test Friedmana"),background ="navy",width=6),
                 box(h3("Test Wilcoxona"),background ="navy",width=6)),
              fluidRow(box(
              HTML('
                   <p style="text-align:justify">
<font size="4" face="Verdana">
                   &nbsp;&nbsp;&nbsp;&nbsp;Test Friedmana jest nieparametrycznym testem służącym do testowania różnic między kilkoma
                   
                   powiązanymi próbkami losowymi. Test Friedmana jest alternatywą dla testu ANOVA dla
                   
                   powtórzonych pomiarów. Jest on używany w przypadku kiedy, ten sam parametr został
                   
                   zmierzony kilkukrotnie dla różnych warunków (np. różnych momentów po lub w trakcie leczenia).</p>
            
                   <p style="text-align:justify">
                   &nbsp;&nbsp;&nbsp;&nbsp;Hipotezą zerową w teście Friedmana jest brak różnic między zmiennymi, (ściślej testujemy czy
                   
                   mediany dla każdej zmiennej są różne). Hipotezą alternatywną jest, przynajmniej jedna zmienna
                   
                   jest różna od pozostałych (dla przynajmniej jednej zmiennej mediana jest różna od pozostałych).</p>
                   \n
                   <p style="text-align:justify">
                   &nbsp;&nbsp;&nbsp;&nbsp;Jeśli p wartość jest niska (mniejsza od zadanego poziomu istotności), hipoteza zerowa jest
                   
                   odrzucana. Oznacz to, że istnieje statystycznie istotna różnica między dwiema zmiennymi.<br/>
 </font> 
                   '),background="blue",width=6),
              box(
                HTML('
<font size="4" face="Verdana">
                     <p style="text-align:justify">
                     &nbsp;Test Wilcoxona jest nieparametrycznym odpowiednikiem testu dla sparowanych próbek. Może
                     
                     on zostać użyty w przypadku, kiedy dane nie mają rozkładu normalnego oraz nie mogą być
                     
                     sprowadzone do rozkładu normalnego przy użyciu transformacji logarytmicznej. Może on zostać
                     
                     użyty również dla danych, które mają rozkład normalny, należy jednak wtedy pamiętać, że test T
                     
                     ma w takim przypadku nieco większą moc.</p>
 </font> 
                     <br/>')
                
                ,background="blue",width=6),
                img(src="oko.jpg", height = 150, width = 300)
              
              )
              )
              )
              )
              )
              )
