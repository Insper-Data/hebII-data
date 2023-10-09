library(tidyverse)
library(ggeasy)

df <- read_csv("data/data.csv")

#TRAJETÓRIA DA INFLAÇÃO
df %>% 
  filter(YEAR < 1970) %>% 
  ggplot(aes(x = factor(YEAR), group = 1)) +
    geom_line(aes(y = inflacao,)) +
    geom_point(aes(y = inflacao,)) +
    labs(x = "Ano", y = "Inflação (% a.a.)")

ggsave("graficos/inflacao.png", width = 9, height = 5, dpi = 600)

#TRAJETÓRIA DA INFLAÇÃO E PIB
df %>% 
  mutate(pib_variacao = (pib - lag(pib))/lag(pib) *100) %>% 
  filter(YEAR > 1951, YEAR < 1965) %>% 
  pivot_longer(
    cols = c(inflacao, pib_variacao), 
    names_to = "cor", 
    values_to = "valor") %>% 
  ggplot(aes(x = YEAR, y = valor, color = cor)) +
    geom_point() +
    geom_line() +
    labs(x = "Ano", y = "Inflação (% a.a.)")

#TRAJETÓRIA DA PIB INDUSTRIAL
df %>% 
  filter(YEAR > 1950, YEAR < 1965) %>% 
  ggplot(aes(x = factor(YEAR), y = pib_industria / pib, group = 1)) +
  geom_line() +
  geom_point() + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Ano", y = "Participação da indústria no PIB")

ggsave("graficos/industria.png", width = 9, height = 5, dpi = 600)

#TRAJETÓRIA DO SALÁRIO
df %>% 
  filter(YEAR > 1950, YEAR < 1970) %>% 
  ggplot(aes(x = factor(YEAR), y = salario, group = 1)) +
    geom_line() +
    geom_point() + 
    labs(x = "Ano", y = "Salário mínimo real (R$ do último mês) ")


ggsave("graficos/salario_minimo.png", width = 9, height = 5, dpi = 600)

#TRAJETÓRIA DA BALANÇA COMERCIAL
df %>% 
  mutate(
    saldo = exportacao - importacao,
    importacao = -importacao,
    ) %>% 
  pivot_longer(
    cols = c(exportacao, importacao), 
    names_to = "cor", 
    values_to = "valor") %>% 
  mutate(
    cor = ifelse(cor == "exportacao", "Exportação", "Importação"),
    valor = valor / 1e9,
    saldo = saldo / 1e9) %>% 
  ggplot() +
    geom_col(aes(x = YEAR, y = valor, fill = cor)) +
    geom_line(aes(x = YEAR, y = saldo), lwd = 1) +
    geom_point(aes(x = YEAR, y = saldo), size = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    xlim(c(1965, 1980)) +
    labs(
      y = "Bilhões US$ deflacionado, 2017 = 1",
      x = "Ano",
      fill = "Componente da BC"
    )

ggsave("graficos/balanca.png", width = 9, height = 5, dpi = 600)


#TRAJETÓRIA DO ÍNDICE DÍVIDA/PIB
df %>% 
  filter(YEAR > 1970, YEAR < 1985) %>% 
  mutate(divida_pib = divida_externa / pib) %>% 
  ggplot(aes(x = factor(YEAR), y = divida_pib, group = 1)) +
    geom_line() +
    geom_point() +
    labs(
      x = "Ano", 
      y = "Dívida Externa / PIB (US$ deflacionado)"
    ) 

ggsave("graficos/divida.png", width = 9, height = 5, dpi = 600)

#BASE MONETÁRIA
df %>% 
  filter(YEAR > 1950, YEAR < 2000) %>% 
  ggplot(aes(x = YEAR, y = (base_monetaria_m3/pib_nominal_brl))) +
  geom_line() +
  geom_point() +
  labs(
    x = "Ano", 
    y = "Base monetária (M3) / PIB nominal"
  ) 
ggsave("graficos/base_monetaria_m3.png", width = 9, height = 5, dpi = 600)

df %>% 
  filter(YEAR > 1950, YEAR < 2000) %>% 
  ggplot(aes(x = YEAR, y = (base_monetaria_m3/pib_nominal_brl)/(lag(base_monetaria_m3)/lag(pib_nominal_brl))-1)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Ano", 
    y = "Variação da razão Base monetária (M3) / PIB nominal"
  ) 

ggsave("graficos/base_monetaria_m3_variacao.png", width = 9, height = 5, dpi = 600)
