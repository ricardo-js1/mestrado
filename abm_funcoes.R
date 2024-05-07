atualiza_idade = function(pop, probs = sobrevida, idademax = 100){
  
  # Decidindo quem morreu
  
  pop$idade = ifelse(pop$idade > 100, 100, pop$idade)
  
  pop = dplyr::left_join(pop, sobrevida, by = c('idade', 'sexo'))
  
  pop$morto = rbinom(length(pop$morto), 1, pop$prob)
  
  # Atualizando a idade e matando quem atingiu a idade máxima
  pop$idade = ifelse(pop$morto == 0, pop$idade + 1, pop$idade)
  pop$morto = ifelse(pop$idade > idademax, 1, pop$morto)
  
  # Removendo a coluna com as probabilidades de sobrevida
  pop = pop[, -21]
  
  return(pop)
  
}

atualiza_imc = function(pop){
  

    pop$imc = ifelse(pop$morto == 0, 
                     ifelse(
      pop$sexo == 'M',
      pop$imc + 0.312 - 0.006712 + 0.00004288 - (2 * 0.006712 * (pop$idade - 18)) - (3 * 0.00004288 * (pop$idade - 18)) +
        (3 * 0.00004288 * (pop$idade - 18)^2) + rnorm(1, 0, abs(0.312 - 0.006712 + 0.00004288 - (2 * 0.006712 * (pop$idade - 18)) - (3 * 0.00004288 * (pop$idade - 18)) +
                                                           (3 * 0.00004288 * (pop$idade - 18)^2))),
      
      pop$imc + 0.186 - 0.0003295 - 0.00002687- (2 * 0.0003295 * (pop$idade - 18)) - (3 * 0.00002687 * (pop$idade - 18)) +
        (3 * 0.00002687 * (pop$idade - 18)^2) + rnorm(1, 0, abs(0.186 - 0.0003295 - 0.00002687- (2 * 0.0003295 * (pop$idade - 18)) - (3 * 0.00002687 * (pop$idade - 18)) +
                                                           (3 * 0.00002687 * (pop$idade - 18)^2)))
    ), pop$imc)
    
  
  return(pop)
  
}

atualiza_pas = function(morto, sexo, grupo, idade, pas){
  
  # Atualizando a pressão arterial sistólica
    pas = dplyr::case_when(
      morto == 0 & sexo == 'M' & grupo == 119 ~ pas + 0.47 + 0.014 + 2 * 0.014 * (idade - 60) + rnorm(1, 0, abs(0.47 + 0.014 + 2 * 0.014 * (idade - 60))),
      morto == 0 & sexo == 'M' & grupo == 139 ~ pas + 0.61 + 0.006 + 2 * 0.006 * (idade - 60) + rnorm(1, 0, abs(0.61 + 0.006 + 2 * 0.006 * (idade - 60))),
      morto == 0 & sexo == 'F' & grupo == 119 ~ pas + 0.62 + 0.013 + 2 * 0.013 * (idade - 60) + rnorm(1, 0, abs(0.62 + 0.013 + 2 * 0.013 * (idade - 60))),
      morto == 0 & sexo == 'F' & grupo == 139 ~ pas + 0.89 + 0.009 + 2 * 0.009 * (idade - 60) + rnorm(1, 0, abs(0.89 + 0.009 + 2 * 0.009 * (idade - 60)))
    )
    
}

atualiza_pad = function(morto, sexo, grupo, idade, pad){
  
  # Atualizando a pressão arterial diastólica
  pad = dplyr::case_when(
    morto == 0 & sexo == 'M' & grupo == 119 ~ pad - 0.21 - 0.005 - 2 * 0.005 * (idade - 60) + rnorm(1, 0, abs(-0.21 - 0.005 - 2 * 0.005 * (idade - 60))),
    morto == 0 & sexo == 'M' & grupo == 139 ~ pad - 0.26 - 0.01 - 2 * 0.01 * (idade - 60) + rnorm(1, 0, abs(-0.26 - 0.01 - 2 * 0.01 * (idade - 60))),
    morto == 0 & sexo == 'F' & grupo == 119 ~ pad - 0.18 - 0.008 - 2 * 0.008 * (idade - 60) + rnorm(1, 0, abs(-0.18 - 0.008 - 2 * 0.008 * (idade - 60))),
    morto == 0 & sexo == 'F' & grupo == 139 ~ pad - 0.10 - 0.012 - 2 * 0.012 * (idade - 60) + rnorm(1, 0, abs(- 0.10 - 0.012 - 2 * 0.012 * (idade - 60)))
    )
  
  pad = ifelse(pad < 45, 45, pad)
  
}

atualiza_risco = function(pop){
  
  pop$risco = dplyr::case_when(
    pop$morto == 0 & pop$has == 0 & pop$imc > imc_min ~ 1,
    pop$imc < imc_min & pop$tdi_imc > 0 & pop$tdi_imc <= tempo_max_imc ~ 0,
    TRUE ~ 0
  )
  
  pop$tdi_imc = dplyr::case_when(
    pop$imc < imc_min & pop$tdi_imc > 0 & pop$tdi_imc <= tempo_max_imc ~ pop$tdi_imc + 1,
    TRUE ~ 0
  )
  
 return(pop)
   
}

sorteia_interv = function(pop){
  
  n_risco = sum(pop$risco)
  n_sorteio = round(n_risco * meta, 0)
  
  sorteados = sample(pop$id[pop$risco == 1], n_sorteio)
  
  pop$sorteado = ifelse(pop$id %in% sorteados, 1, 0)

  return(pop)
  
}

atualiza_inter = function(pop, ...){
  
  if(pop['sorteado'] == 1){
    
    # Primeiro decide se faz AF
    pop[ativ_fisica] = rbinom(1, 1, ifelse(pop$ativ_fisica == 1, pop$prob_af, 1.15 * pop$prob_af))
    
    # Decidindo se faz dieta
    pop$dieta = rbinom(1, 1, ifelse(pop$dieta == 1, pop$prob_dieta, 1.15 * pop$prob_dieta))
    
    aftodoano = rbinom(1,1,0.5)
    dtodoano = rbinom(1,1,0.5)
    
    if(pop$ativ_fisica == 1 & pop$dieta == 1){
      
      red = rnorm(1, 4.2, 0.4)
      red2 = rnorm(1, 4.2, 0.4) * aftodoano * dtodoano
      pop$ured = red + red2
      pop$imc = pop$imc - red - red2
      
    } else
      if(pop$ativ_fisica == 1 & pop$dieta == 0){
        
        red = rnorm(1, 0.8, 0.1)
        red2 = rnorm(1, 0.8, 0.1) * aftodoano 
        pop$ured = red + red2
        pop$imc = pop$imc - red - red2
        
      } else 
        if(pop$ativ_fisica == 0 & pop$dieta == 1){
          
          red = rnorm(1, 4, 0.4)
          red2 = rnorm(1, 4, 0.4) * dtodoano
          pop$ured = red + red2
          pop$imc = pop$imc - red - red2
          
        } else {
        
          if(pop$tdi > 0 & pop$tdi <= wear_off){
            
            pop$imc = pop$imc - pop$ured / pop$tdi
            pop$tdi = pop$tdi + 1
            
          }
          
          if(pop$tdi > wear_off){
            
            pop$tdi = 0
            
          }
          
        }
    
    if(pop$ativ_fisica == 1 | pop$dieta == 1){
      
      pop$cont = pop$cont + 1
      
      if(pop$imc < imc_min & pop$tdi_imc == 0){
        
        pop$tdi_imc = 1
        
      }
      
      pop$tdi = 1
      
      if(pop$tdi == 1){
        
        pop$imc = pop$imc
        
      }
      
      
    }
    
    
  }
  
  return(pop)
  
}

apply(pop_iter, 1, atualiza_inter)
