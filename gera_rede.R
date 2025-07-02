library(igraph)

rede_aleatoria <- function(n_agentes, k_vizinhos = 6) {
  # Rede aleatória
  prob_contato = k_vizinhos / (n_agentes - 1)
  g = sample_gnp(n_agentes, prob_contato)
  
  # Convertendo para matriz de vizinhança W
  adj_matrix = as_adjacency_matrix(g, sparse = FALSE)
  return(adj_matrix)
}




