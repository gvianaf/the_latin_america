
library(tidyverse)
library(readxl)
library(janitor)

# ---- leitura do arquivo

arquivo <- "the_latin_america.xlsx"

the_latam <- arquivo %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map_df(read_excel, path = arquivo, .id = "year") %>% 
  clean_names()

# ---- limpeza dos dados

the_latam_clean <- the_latam %>% 
  filter(!name %in% c("Country/Region", "Explore"))

# pegar as linhas que têm os nomes dos países para juntar à base
paises <- the_latam_clean %>% 
  filter(is.na(rank)) %>% 
  select(country = name)

# retira as linhas dos países e junta com a coluna
the_latam_clean <- the_latam_clean %>% 
  filter(!is.na(rank)) %>% 
  bind_cols(paises)

# transforma todos os números em números
# com exceção do overall, que recalcularei
# e do rank, que também vou recalcular
the_latam_clean <- the_latam_clean %>% 
  mutate(across(c(year, citations:teaching), as.double))

# cálculo do overall score
# 36% teaching, 34% research, 20% citations, 7,5% international outlook, 2,5% industry income
the_latam_clean <- the_latam_clean %>% 
  mutate(overall_calc = 0.36*teaching + 0.34*research + 0.2*citations + 0.075*international_outlook + 0.025*industry_income)

# não bateu 100%... nem está próximo
# refazer o rank agora
# the_latam_clean <- the_latam_clean %>% 
#   group_by(year) %>% 
#   arrange(desc(overall_calc)) %>% 
#   mutate(rank_calc = row_number()) %>% 
#   ungroup()

# muitas diferenças entre o score calculado e o score oficial
# criar coluna de inconsistências
# the_latam_clean <- the_latam_clean %>% 
#   # não é muito confiável, pois o rank original tem intervalos
#   mutate(inconsistencia = ifelse(rank != rank_calc,
#                                  "Sim",
#                                  "Não"))

# salva o arquivo geral
rio::export(the_latam_clean, "the_latin_america_limpo.xlsx")

# cria arquivo do BR
the_latam_br <- the_latam_clean %>% 
  filter(country == "Brazil")

# calcula o rank nacional & federal
the_latam_br <- the_latam_br %>% 
  group_by(year) %>% 
  # arrange(desc(overall_calc)) %>% 
  mutate(country_rank = row_number()) %>% 
  left_join(the_latam_br %>% 
              group_by(year) %>% 
              # arrange(desc(overall_calc)) %>% 
              filter(str_detect(name, "Federal") | name == "University of Brasília") %>% 
              mutate(federal_rank = row_number()) %>% 
              select(year, name, federal_rank)) %>% 
  ungroup()

# vou precisar das siglas
siglas <- the_latam_br %>% 
  distinct(name)

# exportar para preencher na mão
rio::export(siglas, "siglas.xlsx")

# importa o arquivo preenchido
siglas <- rio::import("siglas_preenchida.xlsx")

# insere as siglas no arquivo original
the_latam_br <- the_latam_br %>% 
  left_join(siglas)

# salva o arquivo BR
rio::export(the_latam_br, "the_latin_america_limpo_br.xlsx")
