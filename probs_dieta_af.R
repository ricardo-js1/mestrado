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

idade_cat == "FE1" ~ runif(1, 18, 24) |> floor(),
idade_cat == "FE2" ~ runif(1, 25, 34) |> floor(),
idade_cat == "FE3" ~ runif(1, 35, 44) |> floor(),
idade_cat == "FE4" ~ runif(1, 45, 54) |> floor(),
idade_cat == "FE5" ~ runif(1, 55, 64) |> floor(),
idade_cat == "FE6" ~ runif(1, 65, 69) |> floor()

scores = data.frame(
  sexo_idade = c("F_FE1", "F_FE2", "F_FE3", "F_FE4", "F_FE5", "F_FE6",
                 "M_FE1", "M_FE2", "M_FE3", "M_FE4", "M_FE5", "M_FE6"),
  prob_af = c(mean(c(249.6, 263.5, 234.4)),
           mean(c(249.6, 263.5, 234.4))* 0.95,
           mean(c(286.7, 222.2, 224.8)),
           mean(c(286.7, 222.2, 224.8))* 0.95,
           mean(c(155.7, 158.8, 145.4)),
           mean(c(155.7, 158.8, 145.4))* 0.95,
           mean(c(411.7, 337.9, 338.7)) * 1,
           mean(c(411.7, 337.9, 338.7)) * 0.95,
           mean(c(319.2, 293, 317)) * 1.0,
           mean(c(319.2, 293, 317)) * 0.95,
           mean(c(211.6, 182.4, 171.3)) * 1,
           mean(c(211.6, 182.4, 171.3)) * 0.95)
)

scores$prob_scale_af = scores$prob_af %>% scales::rescale(to = c(0.15, 0.4))

scores_d = data.frame(
  sexo_idade = c("F_FE1", "F_FE2", "F_FE3", "F_FE4", "F_FE5", "F_FE6",
                 "M_FE1", "M_FE2", "M_FE3", "M_FE4", "M_FE5", "M_FE6"),
  prob_d = c(60.4 * 0.98, 60.4, 60.7, 60.7 * 1.025, 64.9* 0.98, 64.9,
           57.5* 0.98, 57.5, 61.0* 0.98, 61.0, 66.3* 0.98, 66.3)
)

scores_d$prob_scale_d = scores_d$prob_d %>% scales::rescale(to = c(0.15, 0.4))

left_join(scores, scores_d)

f_probs = function(sexo_idade){
  
  dplyr::case_when(
    sexo_idade == "F_FE1" ~ MASS::mvrnorm(1, c(0.2692038, 0.2214070), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
    sexo_idade == "F_FE2" ~ MASS::mvrnorm(1, c(0.2548596, 0.2517588), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
    sexo_idade == "F_FE3" ~ MASS::mvrnorm(1, c(0.2639075, 0.2592965), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
    sexo_idade == "F_FE4" ~ MASS::mvrnorm(1, c(0.2498281, 0.2974246), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
    sexo_idade == "F_FE5" ~ MASS::mvrnorm(1, c(0.1588253, 0.3322111), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
    sexo_idade == "F_FE6" ~ MASS::mvrnorm(1, c(0.1500000, 0.3648241), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
    sexo_idade == "M_FE1" ~ MASS::mvrnorm(1, c(0.4000000, 0.1500000), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
    sexo_idade == "M_FE2" ~ MASS::mvrnorm(1, c(0.3791160, 0.1788945), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
    sexo_idade == "M_FE3" ~ MASS::mvrnorm(1, c(0.3389387, 0.2361809), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
    sexo_idade == "M_FE4" ~ MASS::mvrnorm(1, c(0.3211078, 0.2668342), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
    sexo_idade == "M_FE5" ~ MASS::mvrnorm(1, c(0.1992769, 0.3666834), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';'),
    sexo_idade == "M_FE6" ~ MASS::mvrnorm(1, c(0.1884291, 0.4000000), matrix(c(0.05, 0.75*0.05*0.05, 0.75*0.05*0.05, 0.05^2), 2, 2, byrow = T)) |> paste0(collapse = ';')
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
