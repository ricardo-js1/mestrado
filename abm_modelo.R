abm_bellido = function(pop, limite_pop = 0.1, max_iter = 100, tempo = 4, imc_min = 27,
                     pop_alvo = 1, t_ini = 0, t_fim = 100, wear_off = 4){
  
  pop$prob_af = 0.1
  pop$prob_dieta = 0.1
  
  # Variáveis do modelo
  pop$atividade_fisica = 0
  pop$dieta = 0
  pop$ured = 0
  pop$tdi = 0
  pop$imc0 = 0
  pop$cont = 0
  
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
  ativ_fis = c()
  dieta = c()
  
  
  # excluindo quem já começa hipertenso
  pop = pop[pop$has != 9,]
  
#  for(i in 1:k){
  while(tamanho_pop/pop_inicial >= limite_pop & i <= max_iter){

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
    
    # atualizando o risco e o tdiimc
    pop$risco = atualiza_risco(pop$imc, pop$tdiimc, tempo = tempo, imc_min = imc_min)
    pop$tdiimc = atualiza_tdiimc(pop$imc, pop$tdiimc, tempo = tempo, imc_min = imc_min)
    
    # sorteando os agentes
    sorteados = sorteia_agentes(pop$id, pop$risco, pop_alvo = pop_alvo)
    
    # intervindo no imc
    pop = atualiza_interv(pop, sorteados, imc_min = imc_min, t0 = i,
                          t_ini = t_ini, t_fim = t_fim, wear_off = wear_off)

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
    ativ_fis[i] = sum(pop$atividade_fisica)
    dieta[i] = sum(pop$dieta)
    
    # excluindo os hipertensos
    pop = pop[pop$has != 1,]
    
    i = i + 1
    
  }

  return(data.frame(iter, idade, imc, pas, pad, ativ_fis, dieta, hipertensos, pop_restante, taxa))

}

# absim %>%
#   ggplot(aes(x = iter, y = taxa, group = run)) +
#   geom_line(alpha = 0.5) +
#   stat_summary(aes(group = 1, color = "Mediana"), fun = median, geom = "line", 
#                size = 0.8) +
#   stat_summary(aes(group = 1, 
#                    color = "Média"), fun = mean, geom = "line",  size = 0.8) +
#   theme_classic() +
#   theme(legend.position = 'bottom')

