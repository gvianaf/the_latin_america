
library(tidyverse)
library(ggbump)
library(showtext)
library(ggtext)
library(ggrepel)
library(patchwork)

options(OutDec = ",")

# importa os dados já tratados
dados_br <- rio::import("the_latin_america_limpo_br.xlsx")

# vou manter apenas as federais
dados_federais <- dados_br %>% 
  filter(str_detect(name, "Federal") | sigla == "UnB")

dados_federais %>% count(year)

# tem observações demais...
# quero deixar só as 20 primeiras de cada ano
selecao <- dados_federais %>% 
  group_by(year) %>% 
  slice(1:20) %>% 
  ungroup() %>% 
  distinct(sigla)

dados_federais_20p <- dados_federais %>% 
  filter(sigla %in% selecao$sigla)

# vou fazer dois gráficos
# um da evolução dos indicadores
# outro da evolução das posições

# ---- gráfico de evolução dos indicadores da UnB

# cores que serão usadas:
# pantone classic blue #0F4C81
# pantone opal gray #A49E9E

dados_graf_ind <- dados_federais_20p %>% 
  select(year, sigla, citations:teaching) %>% 
  pivot_longer(cols = citations:teaching,
               names_to = "indicador",
               values_to = "valor") %>% 
  mutate(indicador = case_when(
    
    indicador == "citations" ~ "CITAÇÕES\nPeso: 20%",
    indicador == "industry_income" ~ "INDÚSTRIA\nPeso: 2,5%",
    indicador == "international_outlook" ~ "INTERNACIONALIZAÇÃO\nPeso: 7,5%",
    indicador == "research" ~ "PESQUISA\nPeso: 34%",
    indicador == "teaching" ~ "ENSINO\nPeso: 36%"
    
  ),
  # o ano completo atrapalha a visualização, cortei
  year = as.double(str_remove(year, "^\\d{2}")))

temp <- dados_graf_ind %>% 
  filter(year == 20) %>% 
  group_by(indicador) %>% 
  arrange(desc(valor)) %>% 
  mutate(rank = row_number())

# quero saber a posição exata da UnB em cada indicador, em 2020
# primeiro faço um exemplo que funciona
dados_federais_20p %>% 
  filter(year == max(year)) %>% 
  arrange(desc(research)) %>%
  mutate(rank_unb = row_number()) %>% 
  filter(sigla == "UnB") %>% 
  select(rank_unb)

# depois generalizo numa função
posicao_unb <- function(variavel){
  
  dados_federais_20p %>% 
    filter(year == max(year)) %>% 
    arrange(desc({{ variavel }})) %>%
    mutate({{ variavel }} := row_number()) %>% 
    filter(sigla == "UnB") %>% 
    select({{ variavel }})
  
}

# testo
posicao_unb(research)

# e mapeio
varnames <- c("citations", "industry_income", "international_outlook", "research", "teaching")

map_dfc(varnames, ~posicao_unb(!!sym(.)))

font_add("charter", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/Charter Regular.otf")
font_add("charter-bold", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/Charter Bold.otf")
font_add("fira", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/FiraSans-Regular.ttf")
showtext_auto()

graf_ind_fed <- dados_graf_ind %>% 
  ggplot(aes(x = year, y = valor, fill = sigla)) +
  geom_line(aes(color = ifelse(sigla == "UnB", "#0F4C81", alpha("#A49E9E", 0.5)))) +
  geom_point(aes(color = ifelse(sigla == "UnB", "#0F4C81", "#A49E9E")),
             size = 0.25) +
  geom_text(data = dados_graf_ind %>% 
              filter(year == max(year)) %>% 
              group_by(indicador) %>% 
              arrange(desc(valor)) %>% 
              mutate(rank_unb = row_number()) %>% 
              filter(sigla == "UnB"), 
            family = "fira", size = 2,
            aes(x = year + 0.2, label = paste0(rank_unb, "ª"))) +
  facet_grid(cols = vars(indicador)) +   # switch = "both" faria a legenda do facet ir para baixo
  scale_x_continuous(breaks = c(17, 18, 19, 20),
                     limits = c(17, 20.2)) +
  scale_color_identity() +
  labs(title = "Evolução dos indicadores das Universidades Federais no Ranking THE Latin America",
       subtitle = "A <span style='color:#0F4C81'>Universidade de Brasília</span> é a segunda melhor em internacionalização e melhorou em quatro dos cinco<br>indicadores, em especial em renda com indústria e citações, que correspondem a 22,5% da nota",
       x = "Ano de divulgação do ranking",
       y = "Nota no indicador") +
  # modificações no tema
  theme_light(base_family = "charter") +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_markdown(family = "charter", lineheight = 1.2),
        plot.title = element_markdown(family = "charter-bold"),
        axis.title.x = element_text(hjust = 1, size = 8),
        axis.title.y = element_text(hjust = 1, size = 8),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 7))

graf_ind_fed
ggsave("the-latam-ind-federais.pdf", width = 8, height = 3, device = cairo_pdf)
pdftools::pdf_convert("the-latam-ind-federais.pdf", format = "png", dpi = 350)

