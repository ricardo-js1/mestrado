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