abm_teste = function(pop, k){
  
  # Iniciando os monitores
  idade = c()
  imc = c() 
  pas = c()
  pad = c()
  
  for(i in 1:k){
    
    # Ciclos de atualização
   
    # atualizando a idade dos agentes
    pop$idade = pop$idade + 1
     
    # atualizando o imc
    pop$imc = atualiza_imc(pop$sexo, pop$idade, pop$imc)
    
    # atualizando a pas
    pop$pas = atualiza_pas(pop$sexo, pop$grupo, pop$idade, pop$pas)
    
    # atualizando a pad
    pop$pad = atualiza_pad(pop$sexo, pop$grupo, pop$idade, pop$pad)
    
    # Atualizando os monitores
    idade[i] = median(pop$idade)
    imc[i] = median(pop$imc)
    pas[i] = median(pop$pas)
    pad[i] = median(pop$pad)
    
  }

  return(data.frame(idade, imc, pas, pad))

}

abm_teste(pop_teste, k =50)




