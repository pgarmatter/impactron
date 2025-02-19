
soma.glm.nb <- glm.nb(qt_total_usuarios ~ ano_mes, 
                      data = espondilite.soma %>% filter(ano_mes <= eventos_incorporacao[4]))

soma.glm <- glm(qt_total_usuarios ~ ano_mes, 
                data = espondilite.soma %>% filter(ano_mes <= eventos_incorporacao[4]), 
                family = negative.binomial(theta = 1)) # como selecionar o theta?

soma.glm.nb.cast <- new_data(espondilite.soma %>% filter(ano_mes <= eventos_incorporacao[4]), n = 60)
soma.glm.nb.cast$qt_total_usuarios <- predict.glm(soma.glm.nb, newdata = soma.glm.cast, type="response")

soma.glm.cast <- new_data(espondilite.soma %>% filter(ano_mes <= eventos_incorporacao[4]), n = 60)
soma.glm.cast$qt_total_usuarios <- predict.glm(soma.glm, newdata = soma.glm.cast, type="response")

autoplot(soma.glm.nb.cast) +
  geom_line(data = espondilite.soma, aes(x = ano_mes, y = qt_total_usuarios))

autoplot(soma.glm.cast) +
  geom_line(data = espondilite.soma, aes(x = ano_mes, y = qt_total_usuarios))

## avaliando residuos *da predição* por RAIZ DE ERRO MEDIO QUADRADO (RMSE)
soma.futuro.60 <- c((espondilite.soma %>% filter(ano_mes > eventos_incorporacao[4]))$qt_total_usuarios, rep(NA, 20))

#12
soma.glm.nb.resid <- soma.glm.nb.cast$qt_total_usuarios - c(soma.futuro.12$qt_total_usuarios, rep(NA, 60-12))
RMSE(soma.glm.nb.resid)

soma.glm.resid <- soma.glm.cast$qt_total_usuarios - c(soma.futuro.12$qt_total_usuarios, rep(NA, 60-12))
RMSE(soma.glm.resid)

#24
soma.glm.nb.resid <- soma.glm.nb.cast$qt_total_usuarios - c(soma.futuro.24$qt_total_usuarios, rep(NA, 60-24))
RMSE(soma.glm.nb.resid)

soma.glm.resid <- soma.glm.cast$qt_total_usuarios - c(soma.futuro.24$qt_total_usuarios, rep(NA, 60-24))
RMSE(soma.glm.resid)

# 60
soma.glm.nb.resid <- soma.glm.nb.cast$qt_total_usuarios - c(soma.futuro.60$qt_total_usuarios, rep(NA, 20))
RMSE(soma.glm.nb.resid)

soma.glm.resid <- soma.glm.cast$qt_total_usuarios - c(soma.futuro.60$qt_total_usuarios, rep(NA, 20))
RMSE(soma.glm.resid)

#----------------------- ANALISE DEMANDA AFERIDA ---------------------

# todos os modelos
soma.cast %>% 
  inner_join(espondilite.soma.model %>% 
    pivot_longer(cols = c(arima_110, arima_errors, arima_trend, stepwise), names_to = ".model", values_to = "modelo") %>% 
    mutate(desc_modelo = format(modelo)),
    by = c(".model")) %>% 
  autoplot(., espondilite.soma) + 
  # geom_vline(xintercept = as.Date(eventos_incorporacao), color = "blue") +
  geom_vline(xintercept = as.Date(eventos_incorporacao[4]+c(12,24,60)), color = "red") + 
  facet_wrap(~ desc_modelo) +
  guides(.model = "none", color = "none", fill = "none") + 
  theme(legend.position = "bottom") + 
  labs(y = "Quantidade total de pacientes",
       x = "Data de dispensação",
       title = "Número de pacientes de espondilite ancilosante no SUS", 
       subtitle = "tratados com medicamentos do componente especializado de assistencia farmacêutica 1A")

# scores
aics <- glance(espondilite.soma.model) %>% select(.model, AIC)

espondilite.soma.model %>% 
  forecast(new_data = soma.futuro.60) %>% 
  accuracy(soma.futuro.60,
           list(RMSE = RMSE,
                winkler = winkler_score), level = 95) %>% 
  arrange(winkler)

# somente o escolhido
soma.cast %>% 
  filter(.model == "arima_110") %>% 
  mutate(qt_total_usuarios = qt_total_usuarios / 1000,
         .mean = .mean / 1000) %>% 
  autoplot(., espondilite.soma %>% mutate(qt_total_usuarios = qt_total_usuarios / 1000)) + 
  geom_vline(xintercept = as.Date(eventos_incorporacao[4]+c(12,24,60)), color = "red") + 
  theme(legend.position = c(.1,.8)) + 
  labs(y = "Quantidade estimada de total de pacientes\n(em milhares)",
       x = "Data de dispensação",
       title = "Número de pacientes de EA no SUS", 
       subtitle = "tratados com medicamentos do ceaf 1A")

  

autoplot(soma.cast, espondilite.soma) + 
  # geom_vline(xintercept = as.Date(eventos_incorporacao), color = "blue") +
  geom_vline(xintercept = as.Date(eventos_incorporacao[4]+c(12,24,60)), color = "red") + 
  facet_wrap(~ .model)








#----------------------- ANALISE PARA MEDICAMENTOS -------------------

glance(espondilite.model)

# baseando o qt_usuarios_totais em melhor predição da demanda total
soma.cast <- espondilite.soma.model %>% 
  select(arima_110) %>% 
  forecast(h = 60) %>% 
  as_tibble() %>% 
  select(ano_mes, .mean) %>% 
  rename(qt_total_usuarios = .mean)

espondilite.futuro.12 <- espondilite.all %>% 
  filter(ano_mes >= eventos_incorporacao[4], ano_mes <= eventos_incorporacao[4] + 12) %>% 
  select(-qt_total_usuarios) %>% 
  inner_join(soma.cast, by = "ano_mes")

espondilite.futuro.24 <- espondilite.all %>% 
  filter(ano_mes >= eventos_incorporacao[4], ano_mes <= eventos_incorporacao[4] + 24) %>% 
  select(-qt_total_usuarios) %>% 
  inner_join(soma.cast, by = "ano_mes")

espondilite.futuro.60 <- espondilite.all %>% 
  filter(ano_mes >= eventos_incorporacao[4], ano_mes <= eventos_incorporacao[4] + 60) %>% 
  select(-qt_total_usuarios) %>% 
  inner_join(soma.cast, by = "ano_mes")



espondilite.model %>% 
  filter(no_identificador != "CERT") %>% 
  forecast(new_data = espondilite.futuro.12) %>% 
  accuracy(espondilite.futuro.12, 
           list(RMSE = RMSE,
                winkler = winkler_score,
                qs = quantile_score), level = 95) %>% 
  arrange(no_identificador, via, qs)

espondilite.model %>% 
  filter(no_identificador != "CERT") %>% 
  forecast(new_data = espondilite.futuro.24) %>% 
  accuracy(espondilite.futuro.12, 
           list(RMSE = RMSE,
                winkler = winkler_score,
                qs = quantile_score), level = 95) %>% 
  arrange(no_identificador, via, qs)

espondilite.model %>% 
  filter(no_identificador != "CERT") %>% 
  forecast(new_data = espondilite.futuro.60) %>% 
  accuracy(espondilite.futuro.60, 
           list(RMSE = RMSE,
                winkler = winkler_score,
                qs = quantile_score), level = 95) %>% 
  arrange(no_identificador, via, qs)


# ADAL para o poster
adal.cast %>% 
  filter(.model == "arima110") %>% 
  autoplot(., adal.all) +
  geom_vline(xintercept = as.Date(eventos_incorporacao[4]+c(12,24,60)), color = "red") +
  theme(legend.position = "none")

adal.cast %>% 
  filter(.model == "arima110") %>% 
  mutate(qt_usuarios = qt_usuarios / 1000,
       .mean = .mean / 1000) %>% 
  autoplot(., adal.all %>% mutate(qt_usuarios = qt_usuarios / 1000)) + 
  geom_vline(xintercept = as.Date(eventos_incorporacao[4]+c(12,24,60)), color = "red") + 
  theme(legend.position = c(.1,.8)) + 
  labs(y = "Quantidade estimada de pacientes\n(em milhares)",
       x = "Data de dispensação",
       title = "Número de pacientes de EA no SUS", 
       subtitle = "tratados com adalimumabe.")


# ETAN para o poster
etan.cast %>% 
  filter(.model == "stepwise") %>% 
  mutate(qt_usuarios = qt_usuarios / 1000,
         .mean = .mean / 1000) %>% 
  autoplot(., etan.all %>% mutate(qt_usuarios = qt_usuarios / 1000)) + 
  geom_vline(xintercept = as.Date(eventos_incorporacao[4]+c(12,24,60)), color = "red") + 
  theme(legend.position = "bottom") + 
  labs(y = "Quantidade estimada de pacientes\n(em milhares)",
       x = "Data de dispensação",
       title = "Número de pacientes de espondilite ancilosante no SUS", 
       subtitle = "tratados com etanercepte")



