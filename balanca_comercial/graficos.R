library(readr)
library(tidyverse)

saldo <- read_csv("C:/Users/gumac/Downloads/saldo_deflacionado.csv")

df <- saldo %>% 
  mutate(exportacao = -exportacao) %>% 
  pivot_longer(cols = c(exportacao, importacao), names_to = "cor", values_to = "valor")

balanca <- ggplot(saldo %>% 
    mutate(exportacao = -exportacao) %>% 
    pivot_longer(cols = c(exportacao, importacao), names_to = "cor", values_to = "valor"),
    aes(x = data, y = valor)
  ) +
  geom_col(aes(x = data, y = valor, fill = cor)) +
  xlim(c(1950, 1980))


saldo <- ggplot(saldo) +
  geom_col(aes(x = data, y = saldo)) +
  xlim(c(1950, 1980))

ggsave("balanca.png", balanca, dpi = 600)
ggsave("saldo.png", saldo, dpi = 600)
