## app.R ##

#### ELEMENTOS INTERFACE ####

source("R/app_utilidades.R", encoding = "UTF-8")
source("R/app_elementos.R", encoding = "UTF-8")

shinyOptions(cache = cachem::cache_disk(dir = "cache"))

sidebar <- dashboardSidebar(width = 250,
                            sidebarMenu(
                              menuItem("Análise dos Dados de Carga", tabName = "analise_carga", startExpanded = TRUE,
                                       menuSubItem("Comparativo Mensal - CEMIG-D", tabName = "analise_mensal"),
                                       menuSubItem("Comparativo Mensal (LÍQ) - CEMIG-D", tabName = "analise_mensal_liq"),
                                       menuSubItem("Comparativo Mensal - SP", tabName = "analise_mensal_sp"),
                                       menuSubItem("Comparativo Mensal (LÍQ) - SP", tabName = "analise_mensal_liq_sp")
                                       
                              ),
                              menuItem("Ferramentas Gerais" , tabName = "ferramentas_gerais",
                                       menuSubItem("Análise de Dados do Organon", tabName = "analise_organon")
                              )
                            )
)


body <- dashboardBody(
  includeCSS("www/custom.css"),
  withMathJax(),
  useShinyjs(), 
  use_waiter(),
  theme_custom,
  tabItems(
    tabItem(tabName = "analise_mensal",
            
            modulosUI(namespace = "analise_mensal", dados_painel =  dados_comparativo_cemigd, modelo = "normal")
    ),
    tabItem(tabName = "analise_mensal_liq",
            
            modulosUI(namespace = "analise_mensal_liq", dados_painel = dados_comparativo_cemigd, modelo = "liquido")
            
    ),
    tabItem(tabName = "analise_mensal_sp",
            
            modulosUI(namespace = "analise_mensal_sp", dados_painel =  dados_sp, modelo = "normal")
    ),
    tabItem(tabName = "analise_mensal_liq_sp",
            
            modulosUI(namespace = "analise_mensal_liq_sp", dados_painel = dados_sp, modelo = "liquido")
            
    ),
    
    
    
    
    tabItem(tabName = "analise_organon",
            
            modulosUIF(namespace = "analise_organon", modelo = "organon")
            
    )
  ),
  tags$head(
    # tags$style(HTML(".main-sidebar { font-size: 10px; }")),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
  
)



ui <- dashboardPage(
  title = "PLM | MG",
  dashboardHeader(
    
    title = HTML('<a href="https://www.ons.org.br/"> <img class=logo-epe src=logo_ons.png width="65" > </a> <b> PLM | MG </b>'),
    titleWidth = 270
  ),
  sidebar,
  body
)


server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 50*1024^2)
  
  modulosServer(namespace = "analise_mensal", dados_painel =  dados_comparativo_cemigd, modelo = "normal", pinst_mmgd = pinst_mmgd)
  modulosServer(namespace = "analise_mensal_liq", dados_painel =  dados_comparativo_cemigd, modelo = "liquido", pinst_mmgd = NULL)
  modulosServer(namespace = "analise_mensal_sp", dados_painel =  dados_sp, modelo = "normal", pinst_mmgd = pinst_mmgd)
  modulosServer(namespace = "analise_mensal_liq_sp", dados_painel =  dados_sp, modelo = "liquido", pinst_mmgd = NULL)
  
}



shinyApp(ui, server)







