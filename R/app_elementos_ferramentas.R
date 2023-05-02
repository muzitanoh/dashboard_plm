# waiting_screen <- tagList(
#   spin_flower()
# )

### Aba de Ferramentas Gerais


modulosUIF <- function(namespace, modelo){
  
  input_dadosf <- fileInput(
    inputId = NS(namespace, "anexa_dadosf"),
    placeholder = "Nenhum arquivo anexado.",
    accept = c(".csv"),
    multiple = TRUE,
    # label = ""
    label = list(
      tags$div(
        "Anexe a(s) planilhas desejadas:",
        style = "font-size: 1.6vh;",
        tipify(
          title = help_org,
          el = bsButton(inputId = "help_org", label = "", icon = icone_help, style = "inverse", size = "extra-small")
        )
      )
    )
  )
  
  input_dadost <- fileInput(
    inputId = NS(namespace, "anexa_dadost"),
    placeholder = "Nenhum arquivo anexado.",
    accept = c(".csv"),
    multiple = TRUE,
    label = list(
      tags$div(
        "Anexe a(s) planilhas desejadas:",
        style = "font-size: 1.6vh;",
        tipify(
          title = help_orgt,
          el = bsButton(inputId = "help_orgt", label = "", icon = icone_help, style = "inverse", size = "extra-small")
        )
      )
    )
  )
  
  
  select_modelof <- selectInput(
    inputId = NS(namespace, "escolhe_modelof"),
    label = tags$div("Modelo:", style = "font-size: 1.6vh;"), 
    choices = c("Normal", "Comparativo"),
    selected = "Normal",
    multiple = FALSE
  )
  
  select_modelot <- selectInput(
    inputId = NS(namespace, "escolhe_modelot"),
    label = tags$div("Modelo:", style = "font-size: 1.6vh;"), 
    choices = c("Normal", "Comparativo"),
    selected = "Normal",
    multiple = FALSE
  )
  
  download_f <- downloadLink(
    outputId = "baixa_f",
    label = "Download:"
  )
  
  download_t <- downloadLink(
    outputId = "baixa_t",
    label = "Download:"
  )
  
  texto_help <- helpText(
    HTML(
      "Para o correto funcionamento do programa devemos nomear os casos de acordo com o seguinte padrão: <br>",
      "<br>",
      "{Número do Caso}_{Sazonalidade}_{Ano}_{Patamar de Carga}_{Cenário}_{Complemento} <br>" ,
      "<br>",
      "Ou seja, as informações devem ser definidas nos campos espcificados e espaçadas por um '_' (Underline). <br>",
      "<br>",
      "Seguem abaixo alguns exemplos: <br>",
      "1 -> 01_INV_28_MED_REF_COM <br>",
      "2 -> 01_VER_27/28_LEV_REF_COM UFV <br>",
      "3 -> 01_INV_22_LEV UFV30_REF_SEM <br>",
      "4 -> 01_INV_30+_PESADA_N EXP SE_COM CL <br>",
      "5 -> 01_VER_26_MED_NE EXP NESE 8000_SEM <br>",
      "<br>",
      "Qualquer sugestão ou dúvida adicional, favor entrar em contato com Luis Arthur Andrade e Hugo Muzitano."
      #,style = "font-size: 1.6vh;color: #486018;"
    )
  )
  
  
  # downloadButton(label = "Download Dados Usinas", outputId = 'download_usinas')
  
  
  painel_fluxo_tensao <- splitLayout(
    style = "overflow:hidden;",
    cellWidths = c("50%", "50%"),
    wellPanel(
      tags$div("Fluxo", class = "titulo_box"),
      verticalLayout(input_dadosf, select_modelof, download_f)
    ),
    wellPanel(
      tags$div("Tensão", class = "titulo_box"),
      verticalLayout(input_dadost, select_modelot, download_t)
    )
  )
  
  verticalLayout(
    painel_fluxo_tensao, 
    wellPanel(
      tags$div("Observações:", class = "titulo_box", style = "text-align: initial"),
      texto_help
    )
  )
  
  
  
}





