sobrevida = read.csv2('dados/sobrevida.csv') %>% 
  pivot_longer(cols = !'idade', names_to = 'sexo', values_to = 'prob')

abm_teste = function(pop, limite_pop = 0.1){
  
  # Iniciando os monitores
  idade = c()
  imc = c() 
  pas = c()
  pad = c()
  tamanho_pop = nrow(pop)
  pop_restante = c()
  pop_iter = c()
  pop_inicial = nrow(pop)
  hipertensos = c()
  taxa = c()
  iter = c()
  i = 1
  
  # excluindo quem já começa hipertenso
  pop = pop[pop$has != 9,]
  
#  for(i in 1:k){
  while(tamanho_pop/pop_inicial >= limite_pop & i <= 100){

    iter[i] = i
    
    # Ciclos de atualização
    
    pop_iter[i] = nrow(pop)
    
    # atualizando quem morreu
    pop$morte = atualiza_morto(pop$idade, pop$sexo)
   
    # exlcuindo os mortos
    pop = pop[pop$morte == 0,]
    
    # excluindo idade limite
    pop = pop[pop$idade <= 85,]
    
    # atualizando a idade dos agentes
    pop$idade = pop$idade + 1
     
    # atualizando o imc
    pop$imc = atualiza_imc(pop$sexo, pop$idade, pop$imc)
    
    # atualizando a pas
    pop$pas = atualiza_pas(pop$sexo, pop$grupo, pop$idade, pop$pas)
    
    # atualizando a pad
    pop$pad = atualiza_pad(pop$sexo, pop$grupo, pop$idade, pop$pad)
    
    # atualizando a has
    pop$has = atualiza_has(pop$idade, pop$sexo, pop$fumante, pop$hist_fam, pop$imc, pop$pas, pop$pad)
    
    # Atualizando os monitores
    idade[i] = median(pop$idade, na.rm = T)
    imc[i] = median(pop$imc, na.rm = T)
    pas[i] = median(pop$pas, na.rm = T)
    pad[i] = median(pop$pad, na.rm = T)
    hipertensos[i] = nrow(pop[pop$has == 1,])
    tamanho_pop = nrow(pop)
    pop_restante[i] = nrow(pop)
    taxa[i] = hipertensos[i]/(pop_iter[i]) 
    
    # excluindo os hipertensos
    pop = pop[pop$has != 1,]
    
    i = i + 1
    
  }

  return(data.frame(iter, idade, imc, pas, pad, hipertensos, pop_restante, taxa))

}

abm_teste(pop_teste, limite_pop = 0.1) 


absim = data.frame()
for(i in 1:1000){
  print(i)
  a = abm_teste(pop_teste, limite_pop = 0.1)
  a$run = i
  absim = bind_rows(absim, a)

}


absim %>%
  ggplot(aes(x = iter, y = taxa, group = run)) +
  geom_line() +
  stat_summary(aes(group = 1, color = "Mediana"), fun = median, geom = "line", 
               size = 0.8) +
  stat_summary(aes(group = 1, 
                   color = "Média"), fun = mean, geom = "line",  size = 0.8) +
  legend()
