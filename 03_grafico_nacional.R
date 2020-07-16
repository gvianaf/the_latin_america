
library(tidyverse)
library(ggbump)
library(showtext)
library(ggtext)
library(patchwork)

options(OutDec = ",")

# importa os dados já tratados
dados_br <- rio::import("the_latin_america_limpo_br.xlsx")

dados_br %>% count(year)

# tem observações demais...
# quero deixar só as 20 primeiras de 2020
selecao <- dados_br %>% 
  filter(year == max(year)) %>% 
  slice(1:20) %>% 
  distinct(sigla)

dados_br_20p <- dados_br %>% 
  filter(sigla %in% selecao$sigla)

# vou fazer dois gráficos
# um da evolução dos indicadores
# outro da evolução das posições

# ---- gráfico de evolução dos indicadores da UnB

# cores que serão usadas:
# pantone classic blue #0F4C81
# pantone opal gray #A49E9E

dados_graf_ind <- dados_br_20p %>% 
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

# quero saber a posição exata da UnB em cada indicador, em 2020
# primeiro faço um exemplo que funciona
dados_br_20p %>% 
  filter(year == max(year)) %>% 
  arrange(desc(research)) %>%
  mutate(rank_unb = row_number()) %>% 
  filter(sigla == "UnB") %>% 
  select(rank_unb)

# depois generalizo numa função
posicao_unb <- function(variavel){
  
  dados_br_20p %>% 
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

# voltando ao gráfico
font_add("charter", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/Charter Regular.otf")
font_add("charter-bold", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/Charter Bold.otf")
font_add("fira", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/FiraSans-Regular.ttf")
showtext_auto()

graf_ind_br <- dados_graf_ind %>% 
  ggplot(aes(x = year, y = valor, fill = sigla)) +
  geom_line(aes(color = ifelse(sigla == "UnB", "#0F4C81", alpha("#A49E9E", 0.5)))) +
  geom_point(aes(color = ifelse(sigla == "UnB", "#0F4C81", "#A49E9E")),
             size = 0.25) +
  # adiciono a última posição da UnB em cada indicador
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
  labs(title = "Evolução dos indicadores das Universidades Brasileiras no Ranking THE Latin America",
       subtitle = "No cenário nacional, a <span style='color:#0F4C81'>Universidade de Brasília</span> está entre as nove melhores universidades em citações, ensino e<br>internacionalização, indicadores que, conjuntamente, correspondem a 63,5% da nota",
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

graf_ind_br
ggsave("the-latam-ind-br.pdf", width = 8, height = 3, device = cairo_pdf)
pdftools::pdf_convert("the-latam-ind-br.pdf", format = "png", dpi = 350)

# ---- gráfico da evolução da posição

# destaquei a nota, estava muito grande
nota <- glue::glue("Fonte: timeshighereducation.com/world-university-rankings/2020/latin-america-university-rankings
                   Nota: foram avaliadas, na última versão do ranking, {rio::import('the_latin_america_limpo.xlsx') %>% filter(year == max(year)) %>% count()} universidades, sendo {dados_br %>% filter(year == max(year)) %>% count()} brasileiras
                   Elaboração: DAI/DPO/UnB")

graf_pos_br <- dados_br_20p %>% 
  ggplot(aes(x = year, y = country_rank, fill = sigla)) +
  geom_point(size = 4, aes(color = ifelse(sigla == "UnB", "#0F4C81", "#A49E9E"))) +
  geom_bump(size = 2, smooth = 8, aes(color = ifelse(sigla == "UnB", "#0F4C81", alpha("#A49E9E", 0.7)))) +
  scale_color_identity() +
  scale_y_reverse(breaks = c(seq(1, 23))) +
  scale_x_continuous(limits = c(2016.6, 2020.6),
                     breaks = c(2017, 2018, 2019, 2020)) +
  geom_text(data = dados_br_20p %>% filter(year == min(year)), family = "fira",
            aes(x = year - .1, label = sigla), size = 5, hjust = 1) +
  geom_text(data = dados_br_20p %>% filter(year == max(year)), family = "fira",
            aes(x = year + .1, label = sigla), size = 5, hjust = 0) +
  geom_text(data = dados_br_20p %>% filter(year == max(year)), family = "fira",
            aes(x = year + 0.6, label = glue::glue("{country_rank}ª"), size = 5, hjust = 0)) +
  labs(title = "Evolução das Universidades Brasileiras no Ranking THE Latin America",
       subtitle = "A <span style='color:#0F4C81'>Universidade de Brasília</span> está entre as melhores universidades brasileiras, avançando duas posições<br>desde seu ingresso no ranking (2017) e mantendo-se na 10ª colocação nos últimos dois anos",
       x = "Ano de divulgação do ranking",
       y = "",
       caption = nota) +
  theme_classic(base_family = "charter") +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_text(hjust = 0.78),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(family = "charter-bold"),
        plot.subtitle = element_markdown(lineheight = 1.2),
        plot.caption = element_text(margin = margin(10,0,0,0)))

graf_pos_br
ggsave("the-latam-posicao-br.pdf", width = 8, height = 6, device = cairo_pdf)
pdftools::pdf_convert("the-latam-posicao-br.pdf", format = "png", dpi = 350)

# ---- junta os gráficos

p <- graf_ind_br / graf_pos_br + plot_layout(heights = c(1,3))
ggsave("the-latam-br-conjunto.pdf", width = 8, height = 9, device = cairo_pdf)
pdftools::pdf_convert("the-latam-br-conjunto.pdf", format = "png", dpi = 350)

showtext_auto(FALSE)

