library(tidycensus)
library(tidyverse)

# Obtendo os microdados do censo americano
# referência: https://walker-data.com/census-r/introduction-to-census-microdata.html
# página do censo sobre framingham: https://data.census.gov/profile?q=Framingham,+MA&g=160XX00US2524960

# Conferindo as variáveis disponíveis para extrair os microdados
View(pums_variables)

# O código PUMA de Framingham é 00605
# ref: https://www2.census.gov/geo/maps/DC2020/PUMA/st25_ma/Catalog_PUMAmaps_st25.pdf

# Extraindo os microdados com as variáveis de sexo, idade e renda
microdados <- get_pums(
  variables = c("SEX", "AGEP", "REGION", "FINCP", "PUMA"),
  state = "MA", # Massachussets
  survey = "acs1",
  year = 2022,
  recode = T
)

# Filtrando para ter apenas a área que inclui Framingham
microdados = microdados |> 
  filter(PUMA == "00605")

# Verificando as distribuições do sexo, idade e renda
table(microdados$SEX_label)
hist(microdados$AGEP)
hist(microdados$FINCP)

# Removendo as linhas com renda negativa e os abaixo de 18 anos
microdados = microdados |> 
  filter(FINCP >= 0 & AGEP >= 18)

# Criando as categorias de faixa etária utilizadas na simulação
microdados = microdados |> 
  mutate(faixa_etaria = cut(AGEP,
                            breaks = c(-Inf, 24, 34, 44, 54, 64, Inf),
                            labels = c('18 a 24 anos', '25 a 34 anos',
                                       '35 a 44 anos', '45 a 54 anos',
                                       '55 a 64 anos', 'Acima de 65 anos')))

# Criando categorias para a renda
microdados = microdados |> 
  mutate(renda_cat = cut(FINCP, breaks = c(-Inf, 78000, 123300, 205000, Inf),
                         labels = c('Até 78000', '78000 a 123300', '123300 a 205000', 'Acima de 205000'))
  )

# Contando a distribuição de renda, faixa etária e sexo usando os pesos da amostra
microdados |> 
  group_by(SEX_label, faixa_etaria, renda_cat) |> 
  count(wt = PWGTP) |> 
  ungroup() |> 
  mutate(prop = n/sum(n)) 


microdados |> 
  group_by(renda_cat) |> 
  count(wt = PWGTP) |> 
  ungroup() |> 
  mutate(prop = n/sum(n)) 

