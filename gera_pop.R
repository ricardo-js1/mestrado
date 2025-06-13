# Script para gerar a população simulada
gera_pop = function(n_agentes){
  
  # Gerando o id dos agentes
  id = 1:n_agentes
  
  # Gerando o sexo e a faixa etária
  sexo_idade = sample(c("F_FE1", "F_FE2", "F_FE3", "F_FE4", "F_FE5", "F_FE6",
                     "M_FE1", "M_FE2", "M_FE3", "M_FE4", "M_FE5", "M_FE6"),
                   n_agentes, replace = T,
                   prob = c(0.054,0.111,0.112,0.113,0.088,0.03,
                            0.047,0.117,0.113,0.107,0.082,0.026))
  
  sexo = substring(sexo_idade, 1, 1)
  idade_cat = substring(sexo_idade, 3, 5)
  
  # Gerando a idade numérica
  f_idade = function(idade_cat){
    dplyr::case_when(
    idade_cat == "FE1" ~ runif(1, 18, 24) |> floor(),
    idade_cat == "FE2" ~ runif(1, 25, 34) |> floor(),
    idade_cat == "FE3" ~ runif(1, 35, 44) |> floor(),
    idade_cat == "FE4" ~ runif(1, 45, 54) |> floor(),
    idade_cat == "FE5" ~ runif(1, 55, 64) |> floor(),
    idade_cat == "FE6" ~ runif(1, 65, 69) |> floor()
    )
    }
  
  idade = purrr::map_dbl(idade_cat, f_idade)
  
  # Calculando os pesos amostrais
  peso = n_agentes/table(sexo, idade_cat)
  
  # Gerando o histórico familiar
  hist_fam = sample(c(0, 1, 2), n_agentes, replace = T, c(0.057, 0.34, 0.603))
  
  # Gerando o IMC por idade e sexo
  # A parte que usa o peso estava comentada no código original
  f_imc = function(sexo_idade){
    dplyr::case_when(
      sexo_idade == "F_FE1" ~ c('IMC1', 'IMC2', 'IMC3')[which(rmultinom(1, 1, c(0.042684237,0.009838661,0.002935571)) == 1)],
      sexo_idade == "F_FE2" ~ c('IMC1', 'IMC2', 'IMC3')[which(rmultinom(1, 1, c(0.073310934,0.031578673,0.01199731)) == 1)],
      sexo_idade == "F_FE3" ~ c('IMC1', 'IMC2', 'IMC3')[which(rmultinom(1, 1, c(0.069456412,0.033180011,0.011557417)) == 1)],
      sexo_idade == "F_FE4" ~ c('IMC1', 'IMC2', 'IMC3')[which(rmultinom(1, 1, c(0.06321826,0.028554213,0.018096166)) == 1)],
      sexo_idade == "F_FE5" ~ c('IMC1', 'IMC2', 'IMC3')[which(rmultinom(1, 1, c(0.041827408,0.028838916,0.011561522)) == 1)],
      sexo_idade == "F_FE6" ~ c('IMC1', 'IMC2', 'IMC3')[which(rmultinom(1, 1, c(0.015781971,0.009269922,0.002959004)) == 1)],
      sexo_idade == "M_FE1" ~ c('IMC1', 'IMC2', 'IMC3')[which(rmultinom(1, 1, c(0.026310992,0.01484585,0.004550715)) == 1)],
      sexo_idade == "M_FE2" ~ c('IMC1', 'IMC2', 'IMC3')[which(rmultinom(1, 1, c(0.045189596,0.047650005,0.018598202)) == 1)],
      sexo_idade == "M_FE3" ~ c('IMC1', 'IMC2', 'IMC3')[which(rmultinom(1, 1, c(0.042813629,0.050066312,0.017916281)) == 1)],
      sexo_idade == "M_FE4" ~ c('IMC1', 'IMC2', 'IMC3')[which(rmultinom(1, 1, c(0.03896837,0.043086307,0.028052634)) == 1)],
      sexo_idade == "M_FE5" ~ c('IMC1', 'IMC2', 'IMC3')[which(rmultinom(1, 1, c(0.025782834,0.043515905,0.017922644)) == 1)],
      sexo_idade == "M_FE6" ~ c('IMC1', 'IMC2', 'IMC3')[which(rmultinom(1, 1, c(0.009728165,0.013987663,0.004587041)) == 1)]
    )
  }
  
  imc_cat = purrr::map_chr(sexo_idade, f_imc)
  
  # Gerando o IMC numérico
  f_imc_num = function(imc_cat){
    dplyr::case_when(
      imc_cat == 'IMC1' ~ runif(1, 18, 25),
      imc_cat == 'IMC2' ~ runif(1, 25, 30),
      imc_cat == 'IMC3' ~ runif(1, 30, 40)
    )
  }
  
  imc = purrr::map_dbl(imc_cat, f_imc_num)
  
  # Gerando os fumantes
  f_fumante = function(sexo_idade, probs, pesos = NULL){
    
    dplyr::case_when(
      sexo_idade == "F_FE1" ~ ifelse(!is.null(pesos), rbinom(1, 1, pesos["F_FE1"] * 0.014), rbinom(1, 1, 0.014)),
      sexo_idade == "F_FE2" ~ ifelse(!is.null(pesos), rbinom(1, 1, pesos["F_FE2"] * 0.032), rbinom(1, 1, 0.032)),
      sexo_idade == "F_FE3" ~ ifelse(!is.null(pesos), rbinom(1, 1, pesos["F_FE3"] * 0.028), rbinom(1, 1, 0.028)),
      sexo_idade == "F_FE4" ~ ifelse(!is.null(pesos), rbinom(1, 1, pesos["F_FE4"] * 0.033), rbinom(1, 1, 0.033)),
      sexo_idade == "F_FE5" ~ ifelse(!is.null(pesos), rbinom(1, 1, pesos["F_FE5"] * 0.019), rbinom(1, 1, 0.019)),
      sexo_idade == "F_FE6" ~ ifelse(!is.null(pesos), rbinom(1, 1, pesos["F_FE6"] * 0.003), rbinom(1, 1, 0.003)),
      sexo_idade == "M_FE1" ~ ifelse(!is.null(pesos), rbinom(1, 1, pesos["M_FE1"] * 0.013), rbinom(1, 1, 0.013)),
      sexo_idade == "M_FE2" ~ ifelse(!is.null(pesos), rbinom(1, 1, pesos["M_FE2"] * 0.037), rbinom(1, 1, 0.037)),
      sexo_idade == "M_FE3" ~ ifelse(!is.null(pesos), rbinom(1, 1, pesos["M_FE3"] * 0.038), rbinom(1, 1, 0.038)),
      sexo_idade == "M_FE4" ~ ifelse(!is.null(pesos), rbinom(1, 1, pesos["M_FE4"] * 0.034), rbinom(1, 1, 0.034)),
      sexo_idade == "M_FE5" ~ ifelse(!is.null(pesos), rbinom(1, 1, pesos["M_FE5"] * 0.021), rbinom(1, 1, 0.021)),
      sexo_idade == "M_FE6" ~ ifelse(!is.null(pesos), rbinom(1, 1, pesos["M_FE6"] * 0.004), rbinom(1, 1, 0.004)),
    )
    
  }
  
  fumante = purrr::map_dbl(sexo_idade, f_fumante)
  
  # Gerando o nível pressórico
  f_np = function(sexo_idade){
    
    dplyr::case_when(
      sexo_idade == "F_FE1" ~ MASS::mvrnorm(1, c(111, 70), matrix(c(11.6^2, 0.75*11.6*8.3, 0.75*11.6*8.3, 8.3^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "F_FE2" ~ MASS::mvrnorm(1, c(112, 73), matrix(c(12.1^2, 0.75*11.6*9.4, 0.75*11.6*9.4, 9.4^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "F_FE3" ~ MASS::mvrnorm(1, c(119, 77), matrix(c(16.4^2, 0.75*16.4*10.7, 0.75*16.4*10.7, 10.7^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "F_FE4" ~ MASS::mvrnorm(1, c(127, 81), matrix(c(18.7^2, 0.75*18.7*11.1, 0.75*18.7*11.1, 11.1^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "F_FE5" ~ MASS::mvrnorm(1, c(137, 82), matrix(c(20.4^2, 0.75*20.4*11.3, 0.75*20.4*11.3, 11.3^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "F_FE6" ~ MASS::mvrnorm(1, c(145, 81), matrix(c(23.5^2, 0.75*23.5*11.7, 0.75*23.5*11.7, 11.7^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "M_FE1" ~ MASS::mvrnorm(1, c(124, 70), matrix(c(13.7^2, 0.75*13.7*8.3, 0.75*13.7*8.3, 8.3^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "M_FE2" ~ MASS::mvrnorm(1, c(125, 73), matrix(c(13.7^2, 0.75*13.7*9.4, 0.75*13.7*9.4, 9.4^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "M_FE3" ~ MASS::mvrnorm(1, c(126, 77), matrix(c(14.3^2, 0.75*14.3*10.7, 0.75*14.3*10.7, 10.7^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "M_FE4" ~ MASS::mvrnorm(1, c(131, 81), matrix(c(18.1^2, 0.75*18.1*11.1, 0.75*18.1*11.1, 11.1^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "M_FE5" ~ MASS::mvrnorm(1, c(137, 82), matrix(c(18.9^2, 0.75*18.9*11.3, 0.75*18.9*11.3, 11.3^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "M_FE6" ~ MASS::mvrnorm(1, c(142, 81), matrix(c(22^2, 0.75*22*11.7, 0.75*22*11.7, 11.7^2), 2, 2, byrow = T)) |> paste0(collapse = ';')
    )
  
  }
  
  np = purrr::map_chr(sexo_idade, f_np)
  
  # Separando a pas e a pad e convertendo em numérico
  pas = unlist(strsplit(np, ';'))[seq(1, n_agentes*2, by = 2)] |> as.numeric()
  pad = unlist(strsplit(np, ';'))[seq(2, n_agentes*2, by = 2)] |> as.numeric()
  
  # Gerando os hipertensos
  # Código 9 para quem é hipertenso em t0
  has = ifelse(pad >= 140 | pad >= 90, 9, 0) 
  
  # Gerando a estrutura de rede social entre os agentes
  
  # Inicializando a matriz de vizinhança
  # rede_social = matrix(0, nrow = n_agentes, ncol = n_agentes)
  
  # Preenchendo a matriz de rede social
  # for(i in 1:n_agentes){
  #   
  #   # A cada iteração, o agente i é removido do sorteio, assim ele não
  #   # pode fazer parte de sua própria rede social
  #   agentes = sample(c(1:n_agentes)[-i], size = 2, replace = F)
  #   
  #   # Os pares i,j e j,i devem ser iguais a 1 para estabelecer o contato na rede
  #   rede_social[i, agentes] = 1
  #   rede_social[agentes, i] = 1
  #   
  # }
  
  # Número de contatos na rede social
  # rede = rowSums(rede_social)
  
  # Atribuindo o grupo
  grupo = ifelse(pas < 120, 119, 139)
  
  # Gerando as probabilidades de intervenções
  
  f_probs = function(sexo_idade){
    
    dplyr::case_when(
      sexo_idade == "F_FE1" ~ MASS::mvrnorm(1, c(0.4, 0.4), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "F_FE2" ~ MASS::mvrnorm(1, c(0.55, 0.55), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "F_FE3" ~ MASS::mvrnorm(1, c(0.5, 0.5), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "F_FE4" ~ MASS::mvrnorm(1, c(0.45, 0.45), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "F_FE5" ~ MASS::mvrnorm(1, c(0.4, 0.4), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "F_FE6" ~ MASS::mvrnorm(1, c(0.35, 0.35), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "M_FE1" ~ MASS::mvrnorm(1, c(0.4, 0.4), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "M_FE2" ~ MASS::mvrnorm(1, c(0.55, 0.55), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "M_FE3" ~ MASS::mvrnorm(1, c(0.5, 0.5), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "M_FE4" ~ MASS::mvrnorm(1, c(0.45, 0.45), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "M_FE5" ~ MASS::mvrnorm(1, c(0.4, 0.4), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
      sexo_idade == "M_FE6" ~ MASS::mvrnorm(1, c(0.35, 0.35), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';')
    )
    
  }
  
  probs = purrr::map_chr(sexo_idade, f_probs)
  
  # Separando a pas e a pad e convertendo em numérico
  prob_dieta = unlist(strsplit(probs, ';'))[seq(1, n_agentes*2, by = 2)] |> as.numeric()
  prob_dieta = ifelse(prob_dieta > 1, 0.85, prob_dieta)
  prob_dieta = ifelse(prob_dieta < 0, 0.15, prob_dieta)
  
  prob_af = unlist(strsplit(probs, ';'))[seq(2, n_agentes*2, by = 2)] |> as.numeric()
  prob_af = ifelse(prob_af > 1, 0.85, prob_af)
  prob_af = ifelse(prob_af < 0, 0.15, prob_af)
  
  pop_sim = data.frame(id, sexo, idade, hist_fam, imc, fumante, pas, pad, has, grupo, prob_af, prob_dieta)
  
  # Exportando uma lista com a população simulada e a matriz da rede social
  return(pop_sim)
  
}

# Testando

pop_teste = gera_pop(n_agentes = 1000)

