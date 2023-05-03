## app.R ##

#### ELEMENTOS INTERFACE ####

source("R/app_utilidades.R", encoding = "UTF-8")
source("R/app_elementos_carga.R", encoding = "UTF-8")
source("R/app_elementos_ferramentas.R", encoding = "UTF-8")

shinyOptions(cache = cachem::cache_disk(dir = "cache"))

sidebar <- dashboardSidebar(width = 250,
                            sidebarMenu(
                              menuItem("Análise dos Dados de Carga", tabName = "analise_carga", startExpanded = TRUE,
                                       
                                       menuSubItem("Comparativo Mensal QUADRI", tabName = "analise_mensal_quadri"),
                                       # menuSubItem("Comparativo Mensal NNE", tabName = "analise_mensal_nne"),
                                       # menuSubItem("Comparativo Mensal SECO 1", tabName = "analise_mensal"),
                                       # menuSubItem("Comparativo Mensal SECO 2", tabName = "analise_mensal_sp"),
                                       # menuSubItem("Comparativo Mensal SUL", tabName = "analise_mensal_sul")
                                       menuSubItem("Exemplo Carga", tabName = "exemplo_carga")

                                       
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
    tabItem(tabName = "analise_mensal_quadri",
            
            modulosUI(namespace = "analise_mensal_quadri", dados_painel =  dados_quadri, modelo = "QUA")
    ),
    # tabItem(tabName = "analise_mensal_nne",
    #         
    #         modulosUI(namespace = "analise_mensal_nne", dados_painel =  dados_nne, modelo = "PAR")
    # ),
    # tabItem(tabName = "analise_mensal",
    # 
    #         modulosUI(namespace = "analise_mensal", dados_painel =  dados_mg_go_mt, modelo = "PAR")
    # ),
    # tabItem(tabName = "analise_mensal_sp",
    # 
    #         modulosUI(namespace = "analise_mensal_sp", dados_painel =  dados_sp_rj_es, modelo = "PAR")
    # ),
    # tabItem(tabName = "analise_mensal_sul",
    # 
    #         modulosUI(namespace = "analise_mensal_sul", dados_painel =  dados_sul, modelo = "PAR")
    # ),
    tabItem(tabName = "exemplo_carga",
            
            HTML('<img class=logo-epe src=exemplo.png width="1000" >')
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
  title = "PL | SPCB",
  dashboardHeader(
    
    title = HTML('<a href="https://www.ons.org.br/"> <img class=logo-epe src=logo_ons.png width="65" > </a> <b> PL | SCPCB </b>'),
    titleWidth = 270
  ),
  sidebar,
  body
)


server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 50*1024^2)
  
  modulosServer(namespace = "analise_mensal_quadri", dados_painel =  dados_quadri, modelo = "QUA", pinst_mmgd = dados_quadri_mmgd)
  # modulosServer(namespace = "analise_mensal_nne", dados_painel =  dados_nne, modelo = "PAR", pinst_mmgd = pinst_mmgd)
  # modulosServer(namespace = "analise_mensal", dados_painel =  dados_mg_go_mt, modelo = "PAR", pinst_mmgd = pinst_mmgd)
  # modulosServer(namespace = "analise_mensal_sp", dados_painel =  dados_sp_rj_es, modelo = "PAR", pinst_mmgd = pinst_mmgd)
  # modulosServer(namespace = "analise_mensal_sul", dados_painel =  dados_sul, modelo = "PAR", pinst_mmgd = pinst_mmgd)
  
}



shinyApp(ui, server)







