library(tidyverse)

# Importando as funções
source("sobrevida.R")
source("gera_pop.R")
source('abm_funcoes.R')
source('abm_modelo.R')
source('abm_rede.R')
source('gera_rede.R')

# Gerando a população
pop = gera_pop(1000)

# Modelo da Bellido
modelo_bellido = function(pop, k){
  
  absim1 = data.frame()
  print("Modelo com intervenção sempre disponível")
  for(i in 1:k){
    print(i)
    a = abm_bellido(pop, limite_pop = 0.1, t_ini = 0, t_fim = 100)
    a$run = i
    absim1 = bind_rows(absim1, a)
    absim1$modelo = "Intervenção sempre"
  }
  
  # Rodando o modelo sem intervenção
  absim2 = data.frame()
  print("Modelo sem intervenção")
  for(i in 1:k){
    print(i)
    a = abm_bellido(pop, limite_pop = 0.1, t_ini = 100, t_fim = 100)
    a$run = i
    absim2 = bind_rows(absim2, a)
    absim2$modelo = "Intervenção nunca"
  }
  
  # Rodando o modelo com interv a partir do 25 ano
  absim3 = data.frame()
  print("Modelo com intervenção após 25 anos")
  for(i in 1:k){
    print(i)
    a = abm_bellido(pop, limite_pop = 0.1, t_ini = 25, t_fim = 100)
    a$run = i
    absim3 = bind_rows(absim3, a)
    absim3$modelo = "Intervenção após 25 anos"
  }
  
  # Rodando o modelo com interv nos primeiros 25 anos
  absim4 = data.frame()
  print("Modelo com intervenção até 25 anos")
  for(i in 1:k){
    print(i)
    a = abm_bellido(pop, limite_pop = 0.1, t_ini = 1, t_fim = 25)
    a$run = i
    absim4 = bind_rows(absim4, a)
    absim4$modelo = "Intervenção até 25 anos"
  }
  
  return(bind_rows(absim1, absim2, absim3, absim4))
  
}

mod_bellido = modelo_bellido(pop, k = 50)

# Modelo com rede

# Criando a rede
W_aleatoria = rede_aleatoria(nrow(pop))
W_ws = rede_watts_strogatz(nrow(pop))
W_sf = rede_scale_free(nrow(pop))

modelo_rede = function(k, W){
  
  # Rodando o modelo com intervenção sempre
  absim = data.frame()
  for(i in 1:k){
    print(i)
    a = abm_rede(pop, limite_pop = 0.1, t_ini = 0, t_fim = 100, W = W)[[2]]
    a$run = i
    absim = bind_rows(absim, a)
    absim$modelo = "Intervenção sempre"
  }
  
  # Rodando o modelo sem intervenção
  absim2 = data.frame()
  for(i in 1:k){
    print(i)
    a = abm_rede(pop, limite_pop = 0.1, t_ini = 100, t_fim = 100, W = W)[[2]]
    a$run = i
    absim2 = bind_rows(absim2, a)
    absim2$modelo = "Intervenção nunca"
  }
  
  # Rodando o modelo com interv a partir do 25 ano
  absim3 = data.frame()
  for(i in 1:k){
    print(i)
    a = abm_rede(pop, limite_pop = 0.1, t_ini = 25, t_fim = 100, W = W)[[2]]
    a$run = i
    absim3 = bind_rows(absim3, a)
    absim3$modelo = "Intervenção após 25 anos"
  }
  
  # Rodando o modelo com interv nos primeiros 25 anos
  absim4 = data.frame()
  for(i in 1:k){
    print(i)
    a = abm_rede(pop, limite_pop = 0.1, t_ini = 1, t_fim = 25, W = W)[[2]]
    a$run = i
    absim4 = bind_rows(absim4, a)
    absim4$modelo = "Intervenção até 25 anos"
  }

  return(bind_rows(absim, absim2, absim3, absim4))
    
}

mod_aleatorio = modelo_rede(k = 50, W_aleatoria)
mod_ws = modelo_rede(k = 50, W_ws)
mod_sf = modelo_rede(k = 50, W_sf)
mod_homofia = modelo_rede(k = 50, W_homophilia)

teste_modelo %>% 
  group_by(modelo, iter) %>% 
  summarise(taxa = mean(taxa)) %>% 
  ggplot(aes(x = iter, y = taxa, group = modelo, color = modelo)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 25, linetype = 'dashed') +
  theme_classic() + 
  labs(x = 'Iteração/Ano', y = 'Incidência de HAS') +
  theme(legend.position = 'bottom') +
  scale_x_continuous(breaks = 1:50 ) +
  ggtitle("Taxa de incidência de HAS")

teste_modelo %>% 
  group_by(modelo, iter) %>% 
  summarise(taxa = mean(risco)) %>% 
  ggplot(aes(x = iter, y = taxa, group = modelo, color = modelo)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 25, linetype = 'dashed') +
  theme_classic() + 
  labs(x = 'Iteração/Ano', y = 'Incidência de HAS') +
  theme(legend.position = 'bottom') +
  scale_x_continuous(breaks = 1:50 ) +
  ggtitle("População sob risco")

teste_modelo %>% 
  group_by(modelo, iter) %>% 
  summarise(imc = mean(imc)) %>% 
  ggplot(aes(x = iter, y = imc, group = modelo, color = modelo)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 25, linetype = 'dashed') +
  theme_classic() + 
  labs(x = 'Iteração/Ano', y = 'Incidência de HAS') +
  theme(legend.position = 'bottom') +
  scale_x_continuous(breaks = 1:50 ) +
  ggtitle("IMC")

teste_modelo %>% 
  group_by(modelo, iter) %>% 
  summarise(ativ_fis = mean(ativ_fis)) %>% 
  ggplot(aes(x = iter, y = ativ_fis, group = modelo, color = modelo)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 25, linetype = 'dashed') +
  theme_classic() + 
  labs(x = 'Iteração/Ano', y = 'Número de agentes na intervenção') +
  theme(legend.position = 'bottom') +
  ggtitle("Atividade física")

teste_modelo %>% 
  group_by(modelo, iter) %>% 
  summarise(prob_dieta = mean(prob_dieta)) %>% 
  ggplot(aes(x = iter, y = prob_dieta, group = modelo, color = modelo)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 25, linetype = 'dashed') +
  theme_classic() + 
  labs(x = 'Iteração/Ano', y = 'Probabilidade de adesão à dieta') +
  theme(legend.position = 'bottom') +
  ggtitle("Dieta")

teste_modelo %>% 
  group_by(modelo, iter) %>% 
  summarise(prob_af = mean(prob_af)) %>% 
  ggplot(aes(x = iter, y = prob_af, group = modelo, color = modelo)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 25, linetype = 'dashed') +
  theme_classic() + 
  labs(x = 'Iteração/Ano', y = 'Probabilidade de adesão à atividade física') +
  theme(legend.position = 'bottom') +
  ggtitle("Atividade física")

teste_modelo %>% 
  group_by(modelo, iter) %>% 
  summarise(dieta = mean(dieta)) %>% 
  ggplot(aes(x = iter, y = dieta, group = modelo, color = modelo)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 25, linetype = 'dashed') +
  theme_classic() + 
  labs(x = 'Iteração/Ano', y = 'Número de agentes na intervenção') +
  theme(legend.position = 'bottom')


# comparando os dois modelos
mod_bellido %>%
  filter(modelo == "Intervenção sempre") %>% 
  mutate(tipo = "Bellido") %>% 
  bind_rows(mod_aleatorio %>% 
              filter(modelo == "Intervenção sempre") %>% 
              mutate(tipo = "Rede aleatória")) %>% 
  bind_rows(mod_ws %>% 
              filter(modelo == "Intervenção sempre") %>% 
              mutate(tipo = "Rede Watts-Strogatz")) %>% 
  bind_rows(mod_sf %>% 
              filter(modelo == "Intervenção sempre") %>% 
              mutate(tipo = "Rede Scale-Free")) %>% 
  group_by(tipo, iter) %>% 
  summarise(taxa = mean(taxa)) %>% 
  ggplot(aes(x = iter, y = taxa, color = tipo)) +
  geom_line(linewidth = 1) +
  theme_classic() + 
  labs(x = 'Iteração/Ano', y = 'Número de agentes na intervenção') +
  theme(legend.position = 'bottom') +
  ggtitle("Incidência HAS")

mod_bellido %>%
  filter(modelo == "Intervenção sempre") %>% 
  mutate(tipo = "Bellido") %>% 
  bind_rows(mod_aleatorio %>% 
              filter(modelo == "Intervenção sempre") %>% 
              mutate(tipo = "Rede aleatória")) %>% 
  bind_rows(mod_ws %>% 
              filter(modelo == "Intervenção sempre") %>% 
              mutate(tipo = "Rede Watts-Strogatz")) %>% 
  bind_rows(mod_sf %>% 
              filter(modelo == "Intervenção sempre") %>% 
              mutate(tipo = "Rede Scale-Free")) %>% 
  group_by(tipo, iter) %>% 
  summarise(taxa = mean(imc)) %>% 
  ggplot(aes(x = iter, y = taxa, color = tipo)) +
  geom_line(linewidth = 1) +
  theme_classic() + 
  labs(x = 'Iteração/Ano', y = 'IMC médio') +
  theme(legend.position = 'bottom') +
  ggtitle("IMC") +
  ylim(0, NA)

mod_bellido %>%
  filter(modelo == "Intervenção sempre") %>% 
  mutate(tipo = "Bellido") %>% 
  bind_rows(mod_aleatorio %>% 
              filter(modelo == "Intervenção sempre") %>% 
              mutate(tipo = "Rede aleatória")) %>% 
  bind_rows(mod_ws %>% 
              filter(modelo == "Intervenção sempre") %>% 
              mutate(tipo = "Rede Watts-Strogatz")) %>% 
  bind_rows(mod_sf %>% 
              filter(modelo == "Intervenção sempre") %>% 
              mutate(tipo = "Rede Scale-Free")) %>% 
  group_by(tipo, iter) %>% 
  summarise(ativ_fis = mean(ativ_fis)) %>% 
  ggplot(aes(x = iter, y = ativ_fis,  color = tipo)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 25, linetype = 'dashed') +
  theme_classic() + 
  labs(x = 'Iteração/Ano', y = 'Número de agentes na intervenção') +
  theme(legend.position = 'bottom') +
  ggtitle("Atividade física")

mod_bellido %>%
  filter(modelo == "Intervenção sempre") %>% 
  mutate(tipo = "Bellido") %>% 
  bind_rows(mod_aleatorio %>% 
              filter(modelo == "Intervenção sempre") %>% 
              mutate(tipo = "Rede aleatória")) %>% 
  bind_rows(mod_ws %>% 
              filter(modelo == "Intervenção sempre") %>% 
              mutate(tipo = "Rede Watts-Strogatz")) %>% 
  bind_rows(mod_sf %>% 
              filter(modelo == "Intervenção sempre") %>% 
              mutate(tipo = "Rede Scale-Free")) %>% 
  group_by(tipo, iter) %>% 
  summarise(dieta = mean(dieta)) %>% 
  ggplot(aes(x = iter, y = dieta,  color = tipo)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 25, linetype = 'dashed') +
  theme_classic() + 
  labs(x = 'Iteração/Ano', y = 'Número de agentes na intervenção') +
  theme(legend.position = 'bottom') +
  ggtitle("Dieta")

