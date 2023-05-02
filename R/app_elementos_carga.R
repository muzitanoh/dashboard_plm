# waiting_screen <- tagList(
#   spin_flower()
# )


modulosUI <- function(namespace, dados_painel, modelo){
  
  select_particao <- selectInput(
    inputId = NS(namespace, "escolhe_particao"),
    label = "Grandeza:",
    choices = c("Carga", "Carga MMGD", "Carga Total", "Geração Tipo IIB e/ou III", "Geração MMGD", "Geração Total", "Carga Líquida"),
    selected = "Carga Líquida",
    multiple = FALSE
  )
  
  select_ano <- selectInput(
    inputId = NS(namespace, "escolhe_ano"),
    label = "Ano:", 
    choices = sort(unique(dados_painel$ano)),
    selected = min(sort(unique(dados_painel$ano))),
    multiple = FALSE
  )
  
  select_padrao_dia <- selectInput(
    inputId = NS(namespace, "escolhe_padrao_dia"),
    label = "Padrão Dia:", 
    choices = str_to_better(sort(unique(dados_painel$padrao_dia))),
    selected = str_to_better(sort(unique(dados_painel$padrao_dia)))[1],
    multiple = FALSE
  )
  
  select_patamar <- selectInput(
    inputId = NS(namespace, "escolhe_patamar"),
    label = "Patamar:", 
    choices = str_to_better(sort(unique(dados_painel$patamar))),
    selected = str_to_better(sort(unique(dados_painel$patamar)))[1],
    multiple = FALSE
  )
  
  select_subsistema <- selectInput(
    inputId = NS(namespace, "escolhe_subsistema"),
    label = "Subsistema:",
    choices = sort(unique(dados_painel$subsistema)),
    selected = NULL,
    multiple = TRUE
  )
  
  select_uf <- selectInput(
    inputId = NS(namespace, "escolhe_uf"),
    label = "UF:",
    choices = sort(unique(dados_painel$uf)),
    selected = NULL,
    multiple = TRUE
  )
  
  select_distribuidora <- selectInput(
    inputId = NS(namespace, "escolhe_distribuidora"),
    label = "Distribuidora:", 
    choices = sort(unique(dados_painel$distribuidora)),
    selected = NULL,
    multiple = TRUE
  )
  
  select_agrupamento2 <- selectInput(
    inputId = NS(namespace, "escolhe_agrupamento2"),
    label = "Região:",
    choices = str_to_title(sort(unique(dados_painel$agrupamento2))),
    selected = NULL,
    multiple = TRUE
  )
  
  select_n_barramento <- selectInput(
    inputId = NS(namespace, "escolhe_n_barramento"),
    label = "Barra:",
    choices = sort(unique(dados_painel$n_barramento)),
    selected = NULL,
    multiple = TRUE
  )
  
  # select_agrupamento2 <- pickerInput(
  #   inputId = NS(namespace, "escolhe_agrupamento2"),
  #   label = "Região:", 
  #   choices = str_to_title(sort(unique(dados_painel$agrupamento2))),
  #   selected = NULL,
  #   multiple = TRUE,
  #   width = "auto"
  # )

  
  
  if (modelo == "PAR") {
    coluna_filtros <- verticalLayout(
      select_particao, select_ano, select_padrao_dia, select_patamar, select_distribuidora, select_agrupamento2, tags$hr()
    )
  } else if (modelo == "QUA") {
    coluna_filtros <- verticalLayout(
      select_particao, select_padrao_dia, select_patamar, select_subsistema, select_uf, select_distribuidora, select_agrupamento2, select_n_barramento
    )
  } 
  
  
  painel_filtros <- wellPanel(
    tags$div("", class = "titulo_box"),
    coluna_filtros
  )
  
  
  grafico <- wellPanel(
    girafeOutput(NS(namespace, "grafico"), height = "65vh")
  )
  
  
  splitLayout(
    style = "overflow:hidden;",
    cellWidths = c("23%", "77%"),
    painel_filtros,
    grafico
  )
  
  # tagList(
  #   wellPanel(faixa_filtros),
  #   grafico
  # )
  
  
  
}



modulosServer <- function(namespace, dados_painel, modelo, pinst_mmgd){
  
  
  moduleServer(namespace, function(input, output, session){
    

    #### React UI ####
    
    particao_escolhido <- reactive({
      if (input$escolhe_particao == "Carga Total") {
        c("carga", "carga_mmgd")
      } else if (input$escolhe_particao == "Carga") {
        "carga"
      } else if (input$escolhe_particao == "Carga MMGD") {
        "carga_mmgd"
      } else if (input$escolhe_particao == "Geração Total") {
        c("ger_tipo_iib_ou_iii", "ger_mmgd")
      } else if (input$escolhe_particao == "Geração Tipo IIB e/ou III") {
        "ger_tipo_iib_ou_iii"
      } else if (input$escolhe_particao == "Geração MMGD") {
        "ger_mmgd"
      } else if (input$escolhe_particao == "Carga Líquida") {
        c("carga", "carga_mmgd", "ger_tipo_iib_ou_iii", "ger_mmgd")
      }
    })
    
    ano_escolhido <- reactive({
      input$escolhe_ano
    })
    
    patamar_escolhido <- reactive({
      str_to_clean(input$escolhe_patamar)
    })
    
    padrao_dia_escolhido <- reactive({
      str_to_clean(input$escolhe_padrao_dia)
    })
    
    subsistema_escolhido <- reactive({
      
      if (is.null(input$escolhe_subsistema)) {
        sort(unique(dados_painel$subsistema))
      } else{
        input$escolhe_subsistema
      }
    })
    
    uf_escolhido <- reactive({
      
      if (is.null(input$escolhe_uf)) {
        sort(unique(dados_painel$uf))
      } else{
        input$escolhe_uf
      }
    })
    
    distribuidora_escolhido <- reactive({
      
      if (is.null(input$escolhe_distribuidora)) {
        sort(unique(dados_painel$distribuidora))
      } else{
        input$escolhe_distribuidora
      }
    })
    
    agrupamento2_escolhido <- reactive({
    
      if (is.null(input$escolhe_agrupamento2)) {
        sort(unique(dados_painel$agrupamento2))
      } else{
        str_to_upper(input$escolhe_agrupamento2)
      }
    })
    
    n_barramento_escolhido <- reactive({
      
      if (is.null(input$escolhe_n_barramento)) {
        sort(unique(dados_painel$n_barramento))
      } else{
        input$escolhe_n_barramento
      }
    })
    
    
    
    
    #### Observe UI ####
    
    observeEvent(input$escolhe_padrao_dia, {
      
      padrao_dia_escolhido_local <- padrao_dia_escolhido()
      
      dados_filtrados <- filter(dados_painel, padrao_dia %in% padrao_dia_escolhido_local)
      
      updateSelectInput(
        # session = session,
        inputId = "escolhe_patamar",
        choices = str_to_better(sort(unique(dados_filtrados$patamar))),
        selected = str_to_better(sort(unique(dados_filtrados$patamar)))[1]
      )
    })
    
    
    observeEvent(input$escolhe_subsistema, {
      
      subsistema_escolhido_local <- subsistema_escolhido()
      
      dados_filtrados <- filter(dados_painel, subsistema %in% subsistema_escolhido_local)
      
      updateSelectInput(
        # session = session,
        inputId = "escolhe_uf",
        choices = sort(unique(dados_filtrados$uf)),
        selected = NULL
      )
      
      updateSelectInput(
        inputId = "escolhe_distribuidora",
        choices = sort(unique(dados_filtrados$distribuidora)),
        selected = NULL
      )
      
      updateSelectInput(
        inputId = "escolhe_agrupamento2",
        choices = str_to_title(sort(unique(dados_filtrados$agrupamento2))),
        selected = NULL
      )
      
      updateSelectInput(
        inputId = "escolhe_n_barramento",
        choices = sort(unique(dados_filtrados$n_barramento)),
        selected = NULL
      )
    })

    observeEvent(input$escolhe_uf, {
      
      uf_escolhido_local <- uf_escolhido()
      
      dados_filtrados <- filter(dados_painel, uf %in% uf_escolhido_local)
      
      updateSelectInput(
        inputId = "escolhe_distribuidora",
        choices = sort(unique(dados_filtrados$distribuidora)),
        selected = NULL
      )
      
      updateSelectInput(
        inputId = "escolhe_agrupamento2",
        choices = str_to_title(sort(unique(dados_filtrados$agrupamento2))),
        selected = NULL
      )
      
      updateSelectInput(
        inputId = "escolhe_n_barramento",
        choices = sort(unique(dados_filtrados$n_barramento)),
        selected = NULL
      )
    })
    
    observeEvent(input$escolhe_distribuidora, {
      
      distribuidora_escolhido_local <- distribuidora_escolhido()
      
      dados_filtrados <- filter(dados_painel, distribuidora %in% distribuidora_escolhido_local)
      
      updateSelectInput(
        inputId = "escolhe_agrupamento2",
        choices = str_to_title(sort(unique(dados_filtrados$agrupamento2))),
        selected = NULL
      )
      
      updateSelectInput(
        inputId = "escolhe_n_barramento",
        choices = sort(unique(dados_filtrados$n_barramento)),
        selected = NULL
      )
    })
    
    observeEvent(input$escolhe_agrupamento2, {
      
      agrupamento2_escolhido_local <- agrupamento2_escolhido()
      
      dados_filtrados <- filter(dados_painel, agrupamento2 %in% agrupamento2_escolhido_local)
      
      updateSelectInput(
        inputId = "escolhe_n_barramento",
        choices = sort(unique(dados_filtrados$n_barramento)),
        selected = NULL
      )
    })
    
    
    
    
    
    
    #### Tratamento #### 
    
    dados_tratados <- reactive({
        
      if (modelo == "PAR") {
        
        particao_escolhido_local <- particao_escolhido()
        ano_escolhido_local <- ano_escolhido()
        patamar_escolhido_local <- patamar_escolhido()
        padrao_dia_escolhido_local <- padrao_dia_escolhido()
        distribuidora_escolhido_local <- distribuidora_escolhido()
        agrupamento2_escolhido_local <- agrupamento2_escolhido()
        
        dados_filtrados <- dados_painel %>% 
          filter(
            particao %in% particao_escolhido_local,
            ano == ano_escolhido_local,
            patamar == patamar_escolhido_local,
            padrao_dia == padrao_dia_escolhido_local,
            distribuidora %in% distribuidora_escolhido_local,
            # agrupamento1 %in% agrupamento1_,
            agrupamento2 %in% agrupamento2_escolhido_local
          ) 
      } else if (modelo == "QUA") {
        
        particao_escolhido_local <- particao_escolhido()
        ano_escolhido_local <- ano_escolhido()
        patamar_escolhido_local <- patamar_escolhido()
        padrao_dia_escolhido_local <- padrao_dia_escolhido()
        subsistema_escolhido_local <- subsistema_escolhido()
        uf_escolhido_local <- uf_escolhido()
        distribuidora_escolhido_local <- distribuidora_escolhido()
        agrupamento2_escolhido_local <- agrupamento2_escolhido()
        n_barramento_escolhido_local <- n_barramento_escolhido()
        
        dados_filtrados <- dados_painel %>% 
          filter(
            particao %in% particao_escolhido_local,
            patamar == patamar_escolhido_local,
            padrao_dia == padrao_dia_escolhido_local,
            subsistema %in% subsistema_escolhido_local,
            uf %in% uf_escolhido_local,
            distribuidora %in% distribuidora_escolhido_local,
            agrupamento2 %in% agrupamento2_escolhido_local,
            n_barramento %in% n_barramento_escolhido_local
          ) 
      }
    
        
        if (input$escolhe_particao != "Carga Líquida") {
          
          dados <- dados_filtrados %>%
            group_by(
              nome_mes = mes, ciclo
            ) %>% 
            summarise(
              mw = sum(mw), .groups = "keep"
            ) %>% 
            ungroup() %>% 
            left_join(
              dim_mes, by = "nome_mes"
            ) %>% 
            arrange(
              ciclo
            ) %>% 
            arrange(
              n_mes
            ) %>% 
            mutate(
              tooltip = str_glue(
                # "{numero_br(mw)} MW"
                "Ciclo: {ciclo}
          {numero_br(mw)} MW"
              )
            )
          
        } else {
          dados <- dados_filtrados %>% 
            mutate(
              particao = if_else(str_detect(particao, "^carg"), "carga_total", particao),
              particao = if_else(str_detect(particao, "^ger"), "ger_total", particao)
            ) %>% 
            group_by(
              particao, nome_mes = mes, ciclo
            ) %>% 
            summarise(
              mw = sum(mw), .groups = "keep"
            ) %>% 
            ungroup() %>% 
            pivot_wider(
              values_from = "mw",
              names_from = "particao"
            ) %>% 
            mutate(
              mw = carga_total - ger_total
            ) %>% 
            left_join(
              dim_mes, by = "nome_mes"
            ) %>% 
            arrange(
              ciclo
            ) %>% 
            arrange(
              n_mes
            )
            
        }
        
        dados <- dados %>% 
          mutate(
            tooltip = str_glue(
              # "{numero_br(mw)} MW"
              "Ciclo: {ciclo}
          {numero_br(mw)} MW"
            )
          )
        
      })
      
    pot_instalada_mmgd <- reactive({
      
      if (modelo == "QUA") {
        
        particao_escolhido_local <- particao_escolhido()
        ano_escolhido_local <- ano_escolhido()
        patamar_escolhido_local <- patamar_escolhido()
        padrao_dia_escolhido_local <- padrao_dia_escolhido()
        subsistema_escolhido_local <- subsistema_escolhido()
        uf_escolhido_local <- uf_escolhido()
        distribuidora_escolhido_local <- distribuidora_escolhido()
        agrupamento2_escolhido_local <- agrupamento2_escolhido()
        n_barramento_escolhido_local <- n_barramento_escolhido()
        
        dados_dim <- dados_painel %>%
          select(
            n_barramento, subsistema, uf, distribuidora, agrupamento1, agrupamento2
          ) %>%
          distinct_all()
        
        dados_agrupados <- pinst_mmgd %>% 
          left_join(
            dados_dim, by = c("n_barramento", "distribuidora")
          ) %>% 
          left_join(
            dim_mes, by = c("mes" = "n_mes")
          )
        
        dados_filtrados <- dados_agrupados %>%
          filter(
            particao %in% particao_escolhido_local,
            patamar == patamar_escolhido_local,
            padrao_dia == padrao_dia_escolhido_local,
            subsistema %in% subsistema_escolhido_local,
            uf %in% uf_escolhido_local,
            distribuidora %in% distribuidora_escolhido_local,
            agrupamento2 %in% agrupamento2_escolhido_local,
            n_barramento %in% n_barramento_escolhido_local
          ) 
        
        dados <- dados_filtrados %>% 
          filter(
            ano == 2023
          ) %>% 
          group_by(
            mes, nome_mes
          ) %>% 
          summarise(
            pinst = sum(pinst_gd_mw, na.rm = TRUE), .groups = "keep"
          ) %>% 
          ungroup() %>% 
          arrange(
            mes
          )
        
        
        dados
      } else {
        NULL
      }


      })

    
    #### Gráficos + Bind ####  

    if (modelo == "PAR") {
      
      output$grafico <- renderGirafe({
        
        dados_grafico <- dados_tratados()
        
        grafico <- grafico_barras(dados_grafico)
      
        girafe(
          code = {print(grafico)},
          width_svg = 8,
          height_svg = 4.5,
          options = list(
            opts_selection(type = "single", css = ""),
            opts_tooltip(css = NULL, opacity = 0.9, delay_mouseover = 200, delay_mouseout = 500),
            opts_hover(css = "")
          )
        )
        
      }) %>%
        bindCache(
          particao_escolhido(),
          ano_escolhido(),
          patamar_escolhido(),
          padrao_dia_escolhido(),
          distribuidora_escolhido(),
          agrupamento2_escolhido()
        )
      
    } else if (modelo == "QUA") {
      
        
      output$grafico <- renderGirafe({
        
        dados_grafico <- dados_tratados()
        
        grafico <- grafico_barras(dados_grafico)
        
        
        # if (input$escolhe_particao == "Geração MMGD") {
        # 
        #   dados_pot_instalada_mmgd <- pot_instalada_mmgd()
        # 
        #   if (!is.null(dados_pot_instalada_mmgd)) {
        # 
        #     grafico <- grafico_barras_pinst_mmgd(dados_grafico, dados_pot_instalada_mmgd)
        #   }
        # }
        

        girafe(
          code = {print(grafico)},
          width_svg = 8,
          height_svg = 4.5,
          options = list(
            opts_selection(type = "single", css = ""),
            opts_tooltip(css = NULL, opacity = 0.9, delay_mouseover = 200, delay_mouseout = 500),
            opts_hover(css = "")
          )
        )
        
      }) %>%
        bindCache(
          particao_escolhido(),
          patamar_escolhido(),
          padrao_dia_escolhido(),
          subsistema_escolhido(),
          uf_escolhido(),
          distribuidora_escolhido(),
          agrupamento2_escolhido(),
          n_barramento_escolhido()
        )
      
      
      }  
        
    

  })
  
  
}



