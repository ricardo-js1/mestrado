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




atualiza_pas = function( sexo, grupo, idade, pas){
  
  # Atualizando a pressão arterial sistólica
  pas = dplyr::case_when(
    sexo == 'M' & grupo == 119 ~ pas + 0.47 + 0.014 + 2 * 0.014 * (idade - 60) + rnorm(1, 0, abs(0.47 + 0.014 + 2 * 0.014 * (idade - 60))),
    sexo == 'M' & grupo == 139 ~ pas + 0.61 + 0.006 + 2 * 0.006 * (idade - 60) + rnorm(1, 0, abs(0.61 + 0.006 + 2 * 0.006 * (idade - 60))),
    sexo == 'F' & grupo == 119 ~ pas + 0.62 + 0.013 + 2 * 0.013 * (idade - 60) + rnorm(1, 0, abs(0.62 + 0.013 + 2 * 0.013 * (idade - 60))),
    sexo == 'F' & grupo == 139 ~ pas + 0.89 + 0.009 + 2 * 0.009 * (idade - 60) + rnorm(1, 0, abs(0.89 + 0.009 + 2 * 0.009 * (idade - 60)))
  )
  
  return(pas)
  
}

atualiza_pad = function(sexo, grupo, idade, pad){
  
  # Atualizando a pressão arterial diastólica
  pad = dplyr::case_when(
    sexo == 'M' & grupo == 119 ~ pad + 0.21 + 0.005 + 2 * 0.005 * (idade - 60) + rnorm(1, 0, abs(+0.21 + 0.005 + 2 * 0.005 * (idade - 60))),
    sexo == 'M' & grupo == 139 ~ pad + 0.26 + 0.01 + 2 * 0.01 * (idade - 60) + rnorm(1, 0, abs(+0.26 + 0.01 + 2 * 0.01 * (idade - 60))),
    sexo == 'F' & grupo == 119 ~ pad + 0.18 + 0.008 + 2 * 0.008 * (idade - 60) + rnorm(1, 0, abs(+0.18 + 0.008 + 2 * 0.008 * (idade - 60))),
    sexo == 'F' & grupo == 139 ~ pad + 0.10 + 0.012 + 2 * 0.012 * (idade - 60) + rnorm(1, 0, abs(+ 0.10 + 0.012 + 2 * 0.012 * (idade - 60)))
    )
  
  pad = ifelse(pad < 45, 45, pad)
  
}

atualiza_imc = function(sexo, idade, imc){
  
  # Atualizando o imc
  delta_imc = dplyr::case_when(
    sexo == 'M' ~ 0.0312 - 0.00671 + 0.0000429 + 2 * 0.00671 * (idade - 18) + 3 * 0.0000429 * (idade - 18)^2,
    sexo == "F" ~ 0.0186 - 0.00033 - 0.0000269 - 2 * 0.00033 * (idade - 18) - 3 * 0.0000269 * (idade - 18)^2
  )
  
  imc = imc + delta_imc + rnorm(1, 0, abs(delta_imc))
  
  return(imc)
  
}


atualiza_morto = function(idade_agente, sexo_agente){
  
  prob = sobrevida %>% filter(sexo == sexo_agente & idade == idade_agente)
  
  rbinom(1, 1, prob$prob)
  
}

atualiza_morto = function(idade_agente, sexo_agente){
  
  probs <- data.frame(idade = idade_agente, sexo = sexo_agente) %>%
    left_join(sobrevida, by = c("idade", "sexo"))
  
  rbinom(nrow(probs), 1, probs$prob)
  
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
# 
# atualiza_inter = function(pop, ...){
#   
#   n_pop = nrow(pop)
#   
#   if(pop['sorteado'] %in% 1){
#     
#     if(pop["ativ_fisica"] %in% 1){prob_af = pop['prob_af']} else {prob_af = 1.1 * pop["prob_af"]}
#     
#     # Primeiro decide se faz AF
#     pop['ativ_fisica'] = rbinom(n_pop, 1, prob_af)
#     
#     # Decidindo se faz dieta
#     pop$dieta = rbinom(1, 1, ifelse(pop$dieta == 1, pop$prob_dieta, 1.15 * pop$prob_dieta))
#     
#     aftodoano = rbinom(1,1,0.5)
#     dtodoano = rbinom(1,1,0.5)
#     
#     if(pop$ativ_fisica == 1 & pop$dieta == 1){
#       
#       red = rnorm(1, 4.2, 0.4)
#       red2 = rnorm(1, 4.2, 0.4) * aftodoano * dtodoano
#       pop$ured = red + red2
#       pop$imc = pop$imc - red - red2
#       
#     } else
#       if(pop$ativ_fisica == 1 & pop$dieta == 0){
#         
#         red = rnorm(1, 0.8, 0.1)
#         red2 = rnorm(1, 0.8, 0.1) * aftodoano 
#         pop$ured = red + red2
#         pop$imc = pop$imc - red - red2
#         
#       } else 
#         if(pop$ativ_fisica == 0 & pop$dieta == 1){
#           
#           red = rnorm(1, 4, 0.4)
#           red2 = rnorm(1, 4, 0.4) * dtodoano
#           pop$ured = red + red2
#           pop$imc = pop$imc - red - red2
#           
#         } else {
#         
#           if(pop$tdi > 0 & pop$tdi <= wear_off){
#             
#             pop$imc = pop$imc - pop$ured / pop$tdi
#             pop$tdi = pop$tdi + 1
#             
#           }
#           
#           if(pop$tdi > wear_off){
#             
#             pop$tdi = 0
#             
#           }
#           
#         }
#     
#     if(pop$ativ_fisica == 1 | pop$dieta == 1){
#       
#       pop$cont = pop$cont + 1
#       
#       if(pop$imc < imc_min & pop$tdi_imc == 0){
#         
#         pop$tdi_imc = 1
#         
#       }
#       
#       pop$tdi = 1
#       
#       if(pop$tdi == 1){
#         
#         pop$imc = pop$imc
#         
#       }
#       
#       
#     }
#     
#     
#   }
#   
#   return(pop)
#   
# }

atualiza_has = function(idade, sexo, fumante, hist_fam, imc, pas, pad) {
  
  sexo_bin  = ifelse(sexo == 2, 1, 0)
  hist_fam_bin = ifelse(hist_fam == 0, 0, 1)

  coef = 22.94954 - 0.15641 * idade - 0.20293 * sexo_bin - 0.19073 * fumante -
    0.16612 * hist_fam_bin - 0.03388 * imc - 0.05933 * pas - 0.128468 * pad +
    0.001624 * pad * idade
  
  prob = 1 - exp(-exp((log(1) - coef) / 0.87692))
  
  rbinom(length(prob), 1, prob)
  
}


atualiza_risco = function(imc, tdiimc, tempo, imc_min){
  ifelse(imc >= imc_min, 1,
         ifelse(imc < imc_min & tdiimc > 0 & tdiimc <= tempo, 1, 0))
}

atualiza_tdiimc = function(imc, tdiimc, tempo, imc_min){
  ifelse(imc < imc_min & tdiimc > 0 & tdiimc <= tempo, tdiimc + 1, 0)
}

sorteia_agentes = function(id, risco, pop_alvo){
  
  agentes_sob_risco = id[risco == 1]
  sorteados = sample(agentes_sob_risco, pop_alvo * sum(risco))
  
  return(sorteados)
  
}

pop_teste$risco = atualiza_risco(pop_teste$imc, pop_teste$tdiimc, tempo = tempo, imc_min = 27)
pop_teste$tdiimc = atualiza_tdiimc(pop_teste$imc, pop_teste$tdiimc, tempo = tempo, imc_min = 27)

sorteados = sorteia_agentes(pop_teste$id, pop_teste$risco, pop_alvo = 1)

atualiza_interv = function(pop, sorteados, imc_min, t0,
                           t_ini = 0, t_fim = 0, wear_off = 4){
  
  if(pop$id %in% sorteados & (t0 >= t_ini) & (t0 <= t_fim)){
    
    # Decidindo se faz atividade física
    if(pop$atividade_fisica == 0){
     
      # se não fazia 
      pop$atividade_fisica = rbinom(1, ,1, pop$prob_af)
      
    } else {
      
      # se já fazia, tem o dobro de probabilidade de fazer
      pop$atividade_fisica = rbinom(1, 1, 2 * prob_af)
      
    }
    
    # Mesma coisa para dieta
    if(pop$dieta == 0){
      
      # se não fazia 
      pop$dieta = rbinom(1, ,1, pop$prob_dieta)
      
    } else {
      
      # se já fazia, tem o dobro de probabilidade de fazer
      pop$dieta = rbinom(1, 1, 2 * prob_dieta)
      
    }
    
    # Sorteia se a dieta e o AF vão ser feitos o ano todo ou só metade
    aftodoano = rbinom(1, 1, 0.5)
    dtodoano = rbinom(1, 1, 0.5)
      
    
    # atualização do IMC
    if(pop$atividade_fisica == 1 & pop$dieta == 1){
      
      red = rnorm(1, 4.2, 0.4)
      red2 = rnorm(1, 4.2, 0.4) * aftodoano * dtodoano
      
      pop$imc = pop$imc - red - red2
      pop$ured = red + red2
      
    } else 
      if(pop$atividade_fisica == 1 & pop$dieta == 0){
        
        red = rnorm(1, 0.8, 0.1)
        red2 = rnorm(1, 0.8, 0.1) * aftodoano
        
        pop$imc = pop$imc - red - red2
        pop$ured = red + red2
        
      } else 
        if(pop$atividade_fisica == 0 & pop$dieta == 1){
          
          red = rnorm(1, 4, 0.4)
          red2 = rnorm(1, 4, 0.4) * dtodoano
          
          pop$imc = pop$imc - red - red2
          pop$ured = red + red2
          
        } else
          if(pop$tdi > 0 & pop$tdi <= wear_off){
            
            # defasagem para quem fazia exercício e parou
            pop$imc = pop$imc0 - pop$ured/pop$tdi
            pop$tdi = pop$tdi + 1
            
          }
    
          if(pop$tdi > wear_off){
            
            pop$tdi = 0
            
          }
    
  }
  
  if(pop$atividade_fisica == 1 | pop$dieta == 1){
    
    pop$cont = pop$cont + 1
    
    if(pop$imc < imc_min & pop$tdiimc == 0){
      
      pop$tdiimic = 1
      
    }
    
    pop$tdi = 1
    
    if(pop$tdi == 1){
      
      pop$imc0 = pop$imc
      
    }
    
  }
  
  return(pop)
  
}

aa = atualiza_interv(pop, sorteados, imc_min = 27, t0 = 1,
                t_ini = 1, t_fim = 100, wear_off = 4)

summary(aa$imc)
summary(pop$imc)
