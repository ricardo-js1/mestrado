library(tidyverse)

# Carregando os dados de sobrevida
sobrevida = read.csv2('dados/sobrevida.csv') %>% 
  pivot_longer(cols = !'idade', names_to = 'sexo', values_to = 'prob')