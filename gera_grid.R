library(tidyverse)
library(MASS)

# https://www.jarad.me/teaching/2017/11/08/spatial-data

# Criando o grid
grid = expand.grid(x = 1:25, y = 1:25)
n = nrow(grid)

# Explanatory variables and coefficients
x1 <- rnorm(n) %>% round(2)
x2 <- rnorm(n) %>% round(2)

# Spatial field
distance <- as.matrix(dist(grid))

omega <- MASS::mvrnorm(n     = 1, 
                       mu    = rep(0.5, n), 
                       Sigma = 0.5 * exp(-0.1 * distance))

eta <- x1 + x2 + omega

d <- grid %>% 
  mutate(Y_normal = rnorm(n, eta, sd = 0.1) %>% round(2),
         Y_pois   = rpois(n, exp(eta)),
         trials   = 10,
         Y_binom  = rbinom(n = n, size = trials, prob = 1/(1+exp(-eta))),
         x1       = x1,
         x2       = x2)

ggplot(d %>% mutate(omega=omega), 
       aes(x=x, y=y)) +
  geom_raster(aes(fill = omega)) +
  theme_bw()
