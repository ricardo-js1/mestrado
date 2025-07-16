library(igraph)

rede_aleatoria <- function(n_agentes, k_vizinhos = 10) {
  # Rede aleatória
  prob_contato = k_vizinhos / (n_agentes - 1)
  g = sample_gnp(n_agentes, prob_contato)
  
  # Convertendo para matriz de vizinhança W
  adj_matrix = as_adjacency_matrix(g, sparse = FALSE)
  return(adj_matrix)
}

rede_watts_strogatz <- function(n_agentes, k_vizinhos = 10, p_rewire = 0.25) {
  # Rede de Watts-Strogatz (small-world)
  # n_agentes: número de nós
  # k_vizinhos: cada nó conecta com k vizinhos mais próximos (deve ser par)
  # p_rewire: probabilidade de reconexão das arestas
  
  g = sample_smallworld(dim = 1, size = n_agentes, nei = k_vizinhos/2, p = p_rewire)
  
  # Convertendo para matriz de vizinhança W
  adj_matrix = as_adjacency_matrix(g, sparse = FALSE)
  return(adj_matrix)
}

rede_scale_free <- function(n_agentes, m = 5, power = 1, directed = FALSE) {
  # Rede scale-free usando modelo Barabási-Albert
  # m: número de arestas a adicionar a cada novo nó
  # power: poder da preferential attachment (1 = linear)
  
  g = sample_pa(n_agentes, m = m, power = power, directed = directed)
  
  # Convertendo para matriz de vizinhança W
  adj_matrix = as_adjacency_matrix(g, sparse = FALSE)
  return(adj_matrix)
}


rede_homophilic <- function(n_agentes, sex_vector, age_vector, imc_vector, k_vizinhos = 10, homophily_strength = 0.8, 
                            sex_weight = 0.33, age_weight = 0.33, imc_weight = 0.33) {
  # Rede homofílica baseada em atributos dos agentes (sexo, idade e IMC)
  # n_agentes: número de agentes
  # sex_vector: vetor com o sexo dos agentes
  # age_vector: vetor com a idade dos agentes
  # imc_vector: vetor com o IMC dos agentes
  # k_vizinhos: número médio de conexões por agente
  # homophily_strength: força da homofilia (0 = aleatório, 1 = apenas conecta similares)
  # sex_weight: peso da similaridade por sexo (0-1)
  # age_weight: peso da similaridade por idade (0-1)
  # imc_weight: peso da similaridade por IMC (0-1)
  
  # Verificações de entrada
  if (length(sex_vector) != n_agentes) {
    stop("Comprimento de sex_vector deve ser igual a n_agentes")
  }
  
  if (length(age_vector) != n_agentes) {
    stop("Comprimento de age_vector deve ser igual a n_agentes")
  }
  
  if (length(imc_vector) != n_agentes) {
    stop("Comprimento de imc_vector deve ser igual a n_agentes")
  }
  
  # Verificar se age_vector é numérico
  if (!is.numeric(age_vector)) {
    stop("age_vector deve ser numérico")
  }
  
  # Verificar se imc_vector é numérico
  if (!is.numeric(imc_vector)) {
    stop("imc_vector deve ser numérico")
  }
  
  if (any(is.na(age_vector))) {
    stop("age_vector não pode conter valores NA")
  }
  
  if (any(is.na(imc_vector))) {
    stop("imc_vector não pode conter valores NA")
  }
  
  if (any(is.na(sex_vector))) {
    stop("sex_vector não pode conter valores NA")
  }
  
  # Normalizar os pesos
  total_weight <- sex_weight + age_weight + imc_weight
  if (total_weight > 0) {
    sex_weight <- sex_weight / total_weight
    age_weight <- age_weight / total_weight
    imc_weight <- imc_weight / total_weight
  } else {
    sex_weight <- 0.33
    age_weight <- 0.33
    imc_weight <- 0.33
  }
  
  # Inicializar matriz de adjacência
  adj_matrix <- matrix(0, nrow = n_agentes, ncol = n_agentes)
  
  # Calcular matriz de similaridade entre todos os pares de agentes
  similarity_matrix <- matrix(0, nrow = n_agentes, ncol = n_agentes)
  
  # Normalizar idade para escala 0-1
  age_min <- min(age_vector, na.rm = TRUE)
  age_max <- max(age_vector, na.rm = TRUE)
  age_range <- age_max - age_min
  if (age_range == 0) age_range <- 1  # Evitar divisão por zero
  
  # Normalizar IMC para escala 0-1
  imc_min <- min(imc_vector, na.rm = TRUE)
  imc_max <- max(imc_vector, na.rm = TRUE)
  imc_range <- imc_max - imc_min
  if (imc_range == 0) imc_range <- 1  # Evitar divisão por zero
  
  for (i in 1:n_agentes) {
    for (j in 1:n_agentes) {
      if (i != j) {
        # Similaridade por sexo (1 se igual, 0 se diferente)
        sex_similarity <- as.numeric(sex_vector[i] == sex_vector[j])
        
        # Similaridade por idade (1 - diferença normalizada)
        age_diff <- abs(age_vector[i] - age_vector[j])
        age_similarity <- 1 - (age_diff / age_range)
        
        # Similaridade por IMC (1 - diferença normalizada)
        imc_diff <- abs(imc_vector[i] - imc_vector[j])
        imc_similarity <- 1 - (imc_diff / imc_range)
        
        # Combinar similaridades com pesos
        similarity_matrix[i,j] <- sex_weight * sex_similarity + 
          age_weight * age_similarity + 
          imc_weight * imc_similarity
      }
    }
  }
  
  # Calcular probabilidade de conexão baseada na homofilia
  prob_matrix <- matrix(0, nrow = n_agentes, ncol = n_agentes)
  base_prob <- k_vizinhos / (n_agentes - 1)  # probabilidade base (aleatória)
  
  for (i in 1:n_agentes) {
    for (j in 1:n_agentes) {
      if (i != j) {
        # Combinar probabilidade base com homofilia
        homophilic_prob <- similarity_matrix[i,j]
        prob_matrix[i,j] <- (1 - homophily_strength) * base_prob + 
          homophily_strength * homophilic_prob * base_prob * 2
        
        # Garantir que probabilidade não exceda 1
        prob_matrix[i,j] <- min(prob_matrix[i,j], 1)
      }
    }
  }
  
  # Criar conexões baseadas nas probabilidades
  for (i in 1:n_agentes) {
    for (j in (i+1):n_agentes) {
      if (j <= n_agentes) {
        if (runif(1) < prob_matrix[i,j]) {
          adj_matrix[i,j] <- 1
          adj_matrix[j,i] <- 1  # rede não-direcionada
        }
      }
    }
  }
  
  return(adj_matrix)
}


