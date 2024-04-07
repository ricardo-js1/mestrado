# Script para rodar o ABM

# Definindo uma seed
set.seed(42)

# Importando o script que gera a população
source("gera_pop.R")

# Gerando uma população de tamanho n
pop = gera_pop(10000)

# Função para rodar a simulação
abm = function(pop = pop[[1]], W = pop[[2]],
               anos = 100, min_pop = 0.1, idademax = 85,
               interv_i = 16, interv_fim = 60,
               prob_af = 0.1, prob_dieta = 0.1,
               imc_min = 25, meta = 0.3, tempo_max_imc = 3,
               wear_off = 4){
  
  # O primeiro passo é adicionar as variáveis da simulação à pop virtual
  pop = cbind(pop, iter = 0, morto = 0, risco = 0, selecao = 0, ativ_fisica = 0,
              dieta = 0, tdi = 0, ured = 0, tdi_imc = 0, cont = 0)
  
  # Removendo quem já foi gerado tendo hipertensão
  pop = pop[!pop$has == 9,]
  
  # Calculando o tamanho da população
  n = nrow(pop)
  
  # Parâmetros internos da simulação
  t = 1 # iteração
  pop_prop = 1 # proporção da população que permanece na simulação 
  has = 0 # hipertensão
  inc0 = 0 # incidência has
  n0 = 0 # tamanho da pop na iteração
  
  while((t <= anos) & (pop_prop >= min_pop)){
    
    # Tamanho da pop sob risco no começo da iteração
    n0 = sum(pop_sim$morto == 0 & pop_sim$has == 0)
    
    
    
  }
  
  
  
}
