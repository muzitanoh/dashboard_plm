library(shiny)
library(tidyverse)
library(shinydashboard)
library(dashboardthemes)
library(patchwork)
library(ggiraph)
library(readxl)
library(RColorBrewer)
library(scales)
library(latex2exp)
library(colorspace)
library(reactable)
library(shinydashboardPlus)
library(shinyjs)   
library(shinyWidgets)
library(DBI)
library(tibble)
library(ggrepel)
library(ggnewscale)
library(waiter)
library(gghighlight)
library(plotly)
library(waiter)
library(systemfonts)
library(shinyBS)
library(DBI)
library(janitor)
library(magrittr)
library(lubridate)
library(stringr)
library(scales)
library(dbplyr, warn.conflicts = FALSE)
library(readr)
library(stringi)
library(purrr)
library(furrr)
library(writexl)
library(tictoc)

options(warn = - 1) # Disable warning messages globally

theme_custom <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Helvetica",
  appFontColor = "#486018" #001F66
  ,primaryFontColor = "rgb(15,15,15)"
  ,infoFontColor = "rgb(15,15,15)"
  ,successFontColor = "rgb(15,15,15)"
  ,warningFontColor = "rgb(15,15,15)"
  ,dangerFontColor = "rgb(15,15,15)"
  ,bodyBackColor = "rgb(240,240,240)"
  
  ### header
  ,logoBackColor = "#486018"
  
  ,headerButtonBackColor = "#486018"
  ,headerButtonIconColor = "rgb(220,220,220)"
  ,headerButtonBackColorHover = "rgb(100,100,100)"
  ,headerButtonIconColorHover = "rgb(60,60,60)"
  
  ,headerBackColor = "#486018"
  ,headerBoxShadowColor = "#dfdfdf"
  ,headerBoxShadowSize = "3px 5px 5px"
  
  ### sidebar
  ,sidebarBackColor = "rgb(255,255,255)"
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#dfdfdf"
  
  ,sidebarUserTextColor = "rgb(115,115,115)"
  
  ,sidebarSearchBackColor = "rgb(240,240,240)"
  ,sidebarSearchIconColor = "rgb(100,100,100)"
  ,sidebarSearchBorderColor = "rgb(220,220,220)"
  
  ,sidebarTabTextColor = "#486018"
  ,sidebarTabTextSize = 11
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = "rgb(230,230,230)"
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "rgb(245,245,245)"
  ,sidebarTabTextColorHover = "rgb(0,0,0)"
  ,sidebarTabBorderStyleHover = "none solid none none"
  ,sidebarTabBorderColorHover = "rgb(200,200,200)"
  ,sidebarTabBorderWidthHover = 4
  ,sidebarTabRadiusHover = "0px"
  ,boxBackColor = "rgb(255, 255, 255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = 18
  ,boxDefaultColor = "rgb(255,255,255)"
  ,boxPrimaryColor = "rgb(255, 255, 255)"
  ,boxInfoColor = "rgb(180,180,180)"
  ,boxSuccessColor = "rgb(112,173,71)"
  ,boxWarningColor = "rgb(237,125,49)"
  ,boxDangerColor = "rgb(232,76,34)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 6
  ,tabBoxTabTextColor = "rgb(100,100,100)"
  ,tabBoxTabTextColorSelected = "rgb(45,45,45)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgb(200,200,200)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(215,215,215)"
  ,buttonTextColor = "rgb(45,45,45)"
  ,buttonBorderColor = "rgb(150,150,150)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(190,190,190)"
  ,buttonTextColorHover = "rgb(0,0,0)"
  ,buttonBorderColorHover = "rgb(150,150,150)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(118,118,118)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(108,108,108)"
  
  ### tables
  ,tableBackColor = "rgb(248,248,248)"
  ,tableBorderColor = "rgb(238,238,238)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

help_org <- "Anexe a(s) planilha(s) do modelo CMN07 exportadas pelo Case Manager (Static Analysis) do Organon desejadas em formato .csv"
help_orgt <- "NA"

icone_help <- icon(
  name = "question-circle",
  # name = "question",
  lib = "font-awesome"
)

verde_ons <- "#486018"
paleta_graficos <- c("#B4DA68", "#FFBF65", "#00A5E3", "#FFDACC", "#747367", "#FF5768", "#CFF800", "#FF96C5")

fill_interactive_selected <- str_glue("fill:{verde_ons};stroke:black;")
opts_interactive_hover <- str_glue("{fill_interactive_selected};cursor:pointer;")

dim_mes <- tibble(
  mes = c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez"),
  nome_mes = c("janeiro", "fevereiro", "marco", "abril", "maio", "junho", "julho", "agosto", "setembro", "outubro", "novembro", "dezembro"),
  n_mes = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
)


numero_br <- function(numero, scale = 1, acuracia = 1){
  
  number(numero, scale = scale, accuracy = acuracia, big.mark = ".", decimal.mark = "," , trim = TRUE)
  
}

numero_br_mw <- function(numero){
  
  str_glue("{number(numero, scale = 1, accuracy = 1, big.mark = '.', decimal.mark = ',' , trim = TRUE)} MW")
  
}

str_to_better <- function(str) {
  str_to_title(str) %>% str_replace_all("_", " ")
}
str_to_clean <- function(str) {
  str_to_lower(str) %>% str_replace_all(" ", "_")
}


grafico_barras <- function (dados_grafico, tooltip = tooltip) {
  
  # if (particao_ == c("carga, carga_mmgd")) {
  #   
  #   particao_adap <- "CARGA TOTAL"
  #   
  # } else if (particao_ == c("ger_tipo_iib_ou_iii, ger_mmgd")) {
  #   
  #   particao_adap <- "GERACAO TOTAL"
  #   
  # } else {
  #   
  #   particao_adap <- particao_
  # }
  # 
  # titulo_ <- str_glue(
  #   "{str_to_upper(particao_adap)} DO ANO DE {ano_} NA CARGA {str_to_upper(patamar_)} ({str_to_upper(padrao_dia_)}) - REGIÃO {str_to_upper(agrupamento2_)}"
  # ) %>% as.character()
  
  dados_grafico$nome_mes <- factor(
    dados_grafico$nome_mes,
    levels = c("janeiro", "fevereiro", "marco", "abril", "maio", "junho",
               "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")
  )
  
  
  grafico <- ggplot(
    dados_grafico, 
    aes(
      x = nome_mes, y = mw,
      tooltip = tooltip,
      data_id = nome_mes
    )
  ) + 
    geom_bar_interactive(
      stat = "identity", 
      color = verde_ons,
      aes(fill = ciclo), 
      position = "dodge",
      width = 0.7
    ) +
    xlab("Mês") + ylab("MW") +
    # ggtitle(titulo_) +
    theme_bw() +
    theme_minimal() +
    scale_color_manual(values = paleta_graficos) +
    scale_fill_manual(values = paleta_graficos) +
    # geom_text(
    #   aes(
    #     label = numero_br(mw), 
    #     group = ciclo
    #   ), 
    #   position = position_dodge(0.8),
    #   vjust = -0.3, size = 2
    # ) +
    labs(
      y = "",
      x = "",
      color = "",
      fill = ""
    ) +
    theme(
      axis.line.x = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
      # panel.background = element_rect(fill = "#f5f5f5"),
      # legend.key = element_rect(fill = "#f5f5f5"),
      # axis.line.x = element_line(color = "#f5f5f5"),
      # axis.line.y = element_line(color = "#f5f5f5")
    ) +
    scale_y_continuous(
      labels = numero_br_mw
    ) +
    scale_x_discrete(
      label = str_to_title
    )
  
  
  return(grafico)
}

add_linha_pinst <- function(grafico, valor){
  
  grafico_ <- grafico +
    geom_line_interactive(
      aes(
        x = nome_mes,
        y = valor,
        data_id = nome_mes,
        tooltip = str_glue(
          "Pot. Instalada: {numero_br(valor)} MW"
        )
      ),
      size = 1.,
      color = "black",
      group = 1
    )
    
  return(grafico_)
  
}





##### Aquisição - Dados ####

#mg:
dados_cemigd <- read_rds("rds/dados_cemigd.rds") 

dados_sens_cemigd <- read_rds("rds/sensi_cemig.rds") 

dados_mg <- read_rds("rds/dados_mg.rds")

dados_mg <- bind_rows(
  dados_mg, dados_cemigd, dados_sens_cemigd
)

# sp:
dados_sp <- read_rds("rds/dados_sp.rds")

# go:
dados_go <- read_rds("rds/dados_go.rds")

#mt_ac_ro:
dados_mt_ac_ro <- read_rds("rds/dados_mt_ac_ro.rds")


# Dados SCPCB:
dados_scpcb <- bind_rows(
  dados_mg, dados_sp, dados_go, dados_mt_ac_ro
) %>% 
  filter(
    padrao_dia %in% c("dia_util", "domingo")
  )



#potência instalada de mmgd por barramento:
pinst_mmgd <- read_rds("rds/pinst_mmgd.rds") %>% 
  rename(
    n_barramento = barra
  )





