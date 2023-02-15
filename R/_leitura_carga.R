library(readr)
library(readxl)
library(tidyverse)
library(stringr)
library(stringi)
library(purrr)
library(furrr)
library(janitor)
library(writexl)
library(tictoc)

options(warn = - 1) # Disable warning messages globally



#### ANALISE PEC ####
# Analise das barras de geracao com planilha de carga da pec para cada distribuidora

# Distribuidoras relevantes MG: CEMIG-D e ENERGISA-MG
# Distribuidoras relevantes GO/DF: ENEL-GO e CEB D


##### Leitura DIM #####

dim_mes <- tibble(
  mes = c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez"),
  nome_mes = c("janeiro", "fevereiro", "marco", "abril", "maio", "junho", "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")
)

meses_verao_ano_anterior <- c("janeiro", "fevereiro", "marco", "abril")
meses_verao_ano_posterior <- c("novembro", "dezembro")
meses_inverno <- c("maio", "junho", "julho", "agosto", "setembro", "outubro")


##### Leitura Dados #####

le_planilha_carga_dist <- function(path, sheet) {
  
  dataframe <- read_xlsx(path, sheet, col_names = FALSE) %>% 
    clean_names() %>% 
    rename(col1 = 1)
  
}

leitura_pec_dist <- function(dataframe) {
  
  
  nome_distribuidora <- slice(dataframe, 1) %>% pull(1) 
  
  numeros <- dataframe %>%
    filter(
      str_detect(col1, "CARGA POR")
    ) %>%
    pull(1) %>% 
    str_extract_all("\\(?[0-9,.-0-9,.]+\\)?") 
  
  ano <- numeros[[1]][1]
  ciclo <- numeros[[1]][3] 
  
  
  cabecalho1 <- dataframe %>% 
    filter(
      str_detect(col1, "^BARRAM")
    ) %>% 
    mutate_at(
      vars("x227", "x226"), as.character # Melhorar isso aqui depois
    ) %>% 
    pivot_longer(
      cols = everything(),
      names_to = "lixo",
      values_to = "cab1"
    ) %>% 
    fill(
      cab1, .direction = "down"
    ) %>% 
    select(
      cab1
    )
  
  cabecalho2 <- dataframe %>% 
    filter(
      str_detect(x5, "^JAN")
    ) %>% 
    mutate_at(
      vars("x227", "x226"), as.character # Melhorar isso aqui depois
    ) %>% 
    pivot_longer(
      cols = everything(),
      names_to = "lixo",
      values_to = "cab2"
    ) %>% 
    fill(
      cab2, .direction = "down"
    ) %>% 
    select(
      cab2
    )
  
  cabecalho3 <- dataframe %>% 
    mutate(
      col1 = make_clean_names(col1)
    ) %>% 
    filter(
      col1 == "no"
    ) %>% 
    mutate_at(
      vars("x227", "x226"), as.character # Melhorar isso aqui depois
    ) %>% 
    pivot_longer(
      cols = everything(),
      names_to = "lixo",
      values_to = "cab3"
    ) %>% 
    fill(
      cab3, .direction = "down"
    ) %>% 
    select(
      cab3
    )
  
  cabecalho <- bind_cols(cabecalho1, cabecalho2, cabecalho3) %>% 
    mutate(
      cab = str_glue("{cab3}_{cab2}_{cab1}") %>% as.character()
    )
  
  
  planilha_carga_dist_int <- dataframe %>% 
    row_to_names(
      row_number = 6
    ) %>% 
    clean_names() %>% 
    mutate_at(
      vars(starts_with(c("mw", "mvar"))), as.numeric
    )
  
  planilha_carga_dist <- set_names(planilha_carga_dist_int, cabecalho$cab) %>% 
    clean_names() %>% 
    rename(
      n_barramento = starts_with("no_na_b"),
      nome_barramento = starts_with("nome_na_b"),
      g_particao = starts_with("g_p_na"),
      nome_particao = starts_with("nome_na_p")
    ) %>% 
    mutate_at(
      vars(n_barramento), as.numeric
    ) %>%
    filter(
      !is.na(n_barramento)
    ) %>% 
    select(
      -contains("linha_sem_dados")
    ) %>% 
    rename(
      agrupamento = contains("agrupamento")
    ) %>% 
    pivot_longer(
      cols = -c(
        n_barramento, nome_barramento, g_particao, nome_particao, 
        agrupamento1, agrupamento2, agrupamento3, agrupamento4
      ),
      names_to = "infos",
      values_to = "pot"
    ) %>% 
    separate(
      col = infos,
      into = c("unidade", "mes", "patamar", "padrao_dia1", "padrao_dia2", "padrao_dia3", "padrao_dia4", "padrao_dia5"),
      sep = "_"
    ) %>% 
    mutate(
      padrao_dia = str_glue("{padrao_dia1}_{padrao_dia2}_{padrao_dia3}_{padrao_dia4}_{padrao_dia5}"),
      padrao_dia = str_remove_all(padrao_dia,"_NA")
    ) %>% 
    left_join(
      dim_mes, by = "mes"
    ) %>% 
    select(
      -c(padrao_dia1, padrao_dia2, padrao_dia3, padrao_dia4, padrao_dia5, mes)
    ) %>% 
    rename(
      mes = nome_mes
    )
  
  
  planilha_dist <- planilha_carga_dist %>%
    mutate(
      particao = case_when(
        !str_detect(agrupamento1, " GD") & str_detect(g_particao, "G") ~ "ger_tipo_iib_ou_iii",
        str_detect(agrupamento1, " GD") & str_detect(g_particao, "G") ~ "ger_mmgd",
        !str_detect(agrupamento1, " GD") & !str_detect(g_particao, "G") ~ "carga",
        str_detect(agrupamento1, " GD") & !str_detect(g_particao, "G") ~ "carga_mmgd",
        TRUE ~ "carga"
      ),
      distribuidora = nome_distribuidora,
      ano = as.numeric(ano),
      ciclo = ciclo,
      ano_ciclo = case_when(
        mes %in% meses_verao_ano_anterior ~ as.numeric(ano) - 1,
        mes %in% meses_verao_ano_posterior ~ as.numeric(ano) + 1,
        mes %in% meses_inverno ~ as.numeric(ano)
      ),
      agrupamento1 = str_remove_all(agrupamento1, " GD"),
      agrupamento2 = str_remove_all(agrupamento2, " GD"),
      agrupamento1 = str_trim(agrupamento1),
      agrupamento2 = str_trim(agrupamento2)
    ) %>%
    pivot_wider(
      names_from = unidade,
      values_from = pot
    ) %>% 
    select(
      n_barramento, nome_barramento, distribuidora, mes, ano, patamar,
      padrao_dia, ano_ciclo, ciclo, agrupamento1, agrupamento2, particao, mw, mvar
    )
  
  
  
  planilha_dist
  
}

le_todas_abas_ger_dist <- function(caminho_arquivo_dit) {
  
  dados <-
    tibble(
      sheet = excel_sheets(as.character(str_glue("Dados Distribuidoras/{caminho_arquivo_dit}")))
    ) %>%
    slice(
      if_else(str_detect(sheet, "PAR"), row_number(), NULL)
    ) %>%
    mutate(
      ano = str_sub(sheet, start = -4L)
    ) %>% 
    filter(
      !str_detect(ano, "-")
    ) %>% 
    mutate(
      dados = future_map(sheet, ~le_planilha_carga_dist(as.character(str_glue("Dados Distribuidoras/{caminho_arquivo_dit}")), .x) %>% as.data.frame()),
      dados_tidy = future_map(dados, ~leitura_pec_dist(.x))
    ) %>%
    select(
      dados_tidy
    ) %>% 
    unnest(
      cols = c(dados_tidy)
    ) %>% 
    filter(
      !is.na(mw)
    )
  
  
  
}


caminho_arquivo_dit <- c(
  "CEMIG D_PAR-PEL_2023-2027-Of-att.xlsx",
  "CEMIG D_PAR-PEL_2024-2028-Of.xlsx"
)

##### Escrita Dados #####

# dados_pec <- NULL
# 
# tic()
# 
# for (n in 1:length(caminho_arquivo_dit)) {
#   
#   dados_dit <- le_todas_abas_ger_dist(caminho_arquivo_dit[n])
#   
#   
#   dados_pec <- bind_rows(dados_pec, dados_dit)
#   
# }
# 
# toc()
# 
# 
# write_rds(dados_pec, "rds/dados_cemigd_att.rds")













