library(tidyverse)
library(ggeasy)

df <- read_csv("data.csv")

#TRAJETÓRIA DA INFLAÇÃO
df %>% 
  filter(YEAR > 1950, YEAR < 1961) %>% 
  ggplot(aes(x = factor(YEAR), y = inflacao, group = 1)) +
    geom_line() +
    geom_point() +
    labs(x = "Ano", y = "Inflação (% a.a.)")

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

#BASE MONETÁRIA
df %>% 
  mutate(base_monetaria = base_monetaria) %>% 
  ggplot(aes(x = YEAR, y = log(base_monetaria))) +
    geom_line() +
    geom_point() +
    xlim(c(1970, 1985)) +
    ylim(c(-20, -10)) +
    labs(
      x = "Ano", 
      y = "Log da Base monetária (R$)"
    ) 
