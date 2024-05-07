# Script para rodar o ABM

# Definindo uma seed
set.seed(42)

# Importando o script que gera a população
source("gera_pop.R")

# Importando as funções auxiliares
source("abm_funcoes.R")

# Importando tabela de parâmetros
sobrevida = read.csv2("sobrevida.csv") |> 
  dplyr::select(-total) |> 
  tidyr::pivot_longer(!idade, names_to = 'sexo', values_to = 'prob')

# Gerando uma população de tamanho n
pop = gera_pop(10000)

# Função para rodar a simulação
abm = function(pop = pop[[1]],
               W = pop[[2]],
               anos = 100, min_pop = 0.1, idademax = 85,
               interv_i = 16, interv_fim = 60,
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
    
    if(t == 1){  
      
      pop_iter = pop[pop$iter == 0,] 
      
      } else {
      
      pop_iter = pop[pop$iter == t,]  
        
    }
    
    # Tamanho da pop sob risco no começo da iteração
    n0 = sum(pop$morto == 0 & pop$has == 0)
    
    # O primeiro passo é atualizar a idade e quem morreu
    pop_iter = atualiza_idade(pop_iter, sobrevida, idademax = 100)
    
    # Atualizando o IMC
    pop_iter = atualiza_imc(pop_iter)
    
    # Atualizando o nível pressórico
    
    # PAS
    pop_iter$pas = pop_iter |>
      dplyr::select(morto, sexo, grupo, idade, pas) |>
      pmap_dbl(atualiza_pas)
    
    # PAD
    pop_iter$pad = pop_iter |>
      dplyr::select(morto, sexo, grupo, idade, pad) |>
      pmap_dbl(atualiza_pad)
    
    # Atualizando a população sob risco
    pop_iter = atualiza_risco(pop_iter)  
  
    # Em seguida, é feito o sorteio para as intervenções
    pop_iter = sorteia_interv(pop_iter)
    
    
  }
  
}
