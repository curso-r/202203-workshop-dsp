library(tidyverse)
library(easystats)
library(performance)

# install.packages("easystats")
# install.packages("performance")

fish <- readr::read_csv("07-regressao/dados/Fish.csv") |> 
  filter(Weight != 0) |> 
  mutate(
    Height2 = Height + rnorm(n())
  )

regressao_super_simples <- lm(
  Weight ~ Height, data = fish
)

hist(fish$Weight)

summary(regressao_super_simples)

check_model(regressao_super_simples)

fish_pred <- fish |> 
  mutate(
    Weight_predito = regressao_super_simples$fitted.values,
    residuos = regressao_super_simples$residuals
  )

fish_pred |> 
  ggplot(aes(x = Weight_predito, y = residuos, color = Species)) + 
  geom_point() +
  theme_bw() #+ 
  #geom_smooth()

regressao_melhor <- lm(
  Weight ~ Height + Species, data = fish
)

regressao_com_interacao <- lm(
  Weight ~ Height*Species, data = fish
)

fish_pred2 <- fish_pred |> 
  mutate(
    Weight_predito2 = regressao_com_interacao$fitted.values 
  ) 

fish_pred2 |> 
  ggplot(aes(x = Height, y = Weight, color = Species)) +
  geom_line(data = fish_pred2, aes(x = Height, y = Weight_predito2, group = Species, color = Species)) + 
  geom_point()

summary(regressao_melhor)

check_model(regressao_com_interacao)

summary(regressao_com_interacao)

fish_sqrt <- fish_pred |> 
  mutate(
    Weight_sqrt = sqrt(Weight)
  ) 

regressao_sqrt <- lm(
  Weight_sqrt ~ Height*Species, data = fish_sqrt
)

check_model(regressao_sqrt)

modelo_gama <- glm(Weight_sqrt ~ Height*Species,
    data = fish_sqrt, family = Gamma)

modelo_gama_2 <- glm(Weight ~ Height*Species,
                   data = fish_sqrt, family = Gamma)

modelo_gama_3 <- glm(Weight ~ Height+Species,
                     data = fish_sqrt, family = Gamma)

check_model(modelo_gama)

check_model(modelo_gama_2)

check_model(modelo_gama_3)

compare_models(
  modelo_gama,
  modelo_gama_2,
  modelo_gama_3)

modelo_quasi <- glm(Weight ~ Height*Species,
                     data = fish_sqrt, family = Gamma)

check_model(modelo_quasi)

# Regressão logística

set.seed(13072022)

dados_churn <- readr::read_csv("07-regressao/dados/WA_Fn-UseC_-Telco-Customer-Churn.csv") |> 
  mutate(
    Churnas = as.numeric(factor(Churn))-1
  ) |> 
  select(-customerID) |> 
  dplyr::sample_n(1000)

modelo_ingenuo <- glm(Churnas ~ .,
                      data = dados_churn,
                      family = 'binomial')

summary(modelo_ingenuo)

check_model(modelo_ingenuo)



