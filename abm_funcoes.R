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