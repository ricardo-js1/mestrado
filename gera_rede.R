library(igraph)

rede_aleatoria <- function(n_agentes, k_vizinhos = 6) {
  # Rede aleatória
  prob_contato = k_vizinhos / (n_agentes - 1)
  g = sample_gnp(n_agentes, prob_contato)
  
  # Convertendo para matriz de vizinhança W
  adj_matrix = as_adjacency_matrix(g, sparse = FALSE)
  return(adj_matrix)
}

rede_watts_strogatz <- function(n_agentes, k_vizinhos = 6, p_rewire = 0.1) {
  # Rede de Watts-Strogatz (small-world)
  # n_agentes: número de nós
  # k_vizinhos: cada nó conecta com k vizinhos mais próximos (deve ser par)
  # p_rewire: probabilidade de reconexão das arestas
  
  g = sample_smallworld(dim = 1, size = n_agentes, nei = k_vizinhos/2, p = p_rewire)
  
  # Convertendo para matriz de vizinhança W
  adj_matrix = as_adjacency_matrix(g, sparse = FALSE)
  return(adj_matrix)
}
