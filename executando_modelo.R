library(tidyverse)

# Importando as funções
source("sobrevida.R")
source("gera_pop.R")
source('abm_funcoes.R')
source('abm_modelo.R')
source('abm_rede.R')
source('gera_rede.R')

# Criando a rede
W = rede_aleatoria(nrow(pop))

tic()
# Rodando o modelo com intervenção sempre
absim = data.frame()
for(i in 1:250){
  print(i)
  a = abm_rede(pop, limite_pop = 0.1, t_ini = 0, t_fim = 100, W = W)
  a$run = i
  absim = bind_rows(absim, a)
  absim$modelo = "Intervenção sempre"
}

# Rodando o modelo sem intervenção
absim2 = data.frame()
for(i in 1:250){
  print(i)
  a = abm_rede(pop, limite_pop = 0.1, t_ini = 100, t_fim = 100, W = W)
  a$run = i
  absim2 = bind_rows(absim2, a)
  absim2$modelo = "Intervenção nunca"
}

# Rodando o modelo com interv a partir do 25 ano
absim3 = data.frame()
for(i in 1:250){
  print(i)
  a = abm_rede(pop, limite_pop = 0.1, t_ini = 25, t_fim = 100, W = W)
  a$run = i
  absim3 = bind_rows(absim3, a)
  absim3$modelo = "Intervenção após 25 anos"
}

# Rodando o modelo com interv nos primeiros 25 anos
absim4 = data.frame()
for(i in 1:250){
  print(i)
  a = abm_rede(pop, limite_pop = 0.1, t_ini = 1, t_fim = 25, W = W)
  a$run = i
  absim4 = bind_rows(absim4, a)
  absim4$modelo = "Intervenção até 25 anos"
}
toc()

bind_rows(absim, absim2, absim3, absim4) %>% 
  group_by(modelo, iter) %>% 
  summarise(taxa = mean(taxa)) %>% 
  ggplot(aes(x = iter, y = taxa, group = modelo, color = modelo)) +
  geom_line() +
  geom_vline(xintercept = 25, linetype = 'dashed') +
  theme_classic() + 
  labs(x = 'Iteração/Ano', y = 'Incidência de HAS') +
  theme(legend.position = 'bottom')

bind_rows(absim, absim2, absim3, absim4) %>% 
  group_by(modelo, iter) %>% 
  summarise(ativ_fis = mean(ativ_fis)) %>% 
  ggplot(aes(x = iter, y = ativ_fis, group = modelo, color = modelo)) +
  geom_line() +
  geom_vline(xintercept = 25, linetype = 'dashed') +
  theme_classic() + 
  labs(x = 'Iteração/Ano', y = 'Incidência de HAS') +
  theme(legend.position = 'bottom')


bind_rows(absim, absim2, absim3, absim4) %>% 
  group_by(modelo, iter) %>% 
  summarise(dieta = mean(dieta)) %>% 
  ggplot(aes(x = iter, y = dieta, group = modelo, color = modelo)) +
  geom_line() +
  geom_vline(xintercept = 25, linetype = 'dashed') +
  theme_classic() + 
  labs(x = 'Iteração/Ano', y = 'Incidência de HAS') +
  theme(legend.position = 'bottom')

