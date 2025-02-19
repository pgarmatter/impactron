require(tidyverse)
require(fpp3)
require(distributional)

#retorna um lm da série diferenciada da tendencia da coluna <response> de <data>. 
# <columns> é usada para definir a formula
mltrend <- function(data, response, group, columns, ts=F){
  
  decomposition <- data %>% 
    model(MLT = STL(!!rlang::ensym(response) ~ trend() + season(0))) %>% 
    components() %>%
    group_by(!!! group) %>%
    mutate(d1 = difference(trend)) %>% 
    select(-.model)
  
  data = data %>% left_join(decomposition)
  
  if(ts){
    model = data %>% model(TSLM = TSLM(update.formula(d1 ~ ., columns)))
  }else{
    model = lm(update.formula(d1 ~ ., columns), data = data)
    model = step(model) #seleciona variáveis usando AIC
  }
  
  # dispersão
  m.disp = decomposition %>% model(disp = NAIVE(remainder))
  
  resp = list(model = model,
              m.disp = m.disp,
              strat = if(ts){"TSLM"}else{"LM"})

  return(resp)
}

# model deve ter model, m.disp e stats (gerado pela mltrend)
# newdata idealmente contem d1 e trend, ou seja, contém os dados de decomposição da série
# se newdata não for decomposta, deve pelo menos ter chave
predict.mltrend <- function(model, newdata, response, key){
  
  ts = model$strat == "TSLM"
  m = model$model
  m.d = model$m.disp
  
  if(ts){
    predito <- m %>% forecast(new_data = newdata)
  }else{
    predito <- predict(m, newdata = newdata)
  }
  
  if("trend" %in% colnames(newdata)){
    trend <- newdata
  }else{
    trend <- newdata %>% 
      model(MLT = STL(!!rlang::ensym(response) ~ trend())) %>% 
      components() %>%
      group_by(key) %>%
      mutate(d1 = difference(trend)) %>% 
      right_join(newdata) %>% 
      select(-.model)
  }
  
  remainder <- m.d %>% 
    forecast(new_data = newdata %>% filter(ano_mes > max(m$model$ano_mes))) %>% 
    rename(p.remainder = remainder)

  # trend <- trend %>%
  #   bind_cols(p.d1 = predito) %>%
  #   group_by(key) %>%
  #   mutate(p.trend = cumsum(p.d1)) %>%
  #   ungroup() %>%
  #   mutate(
  #     !!enquo(response) := dist_normal(p.trend, sd(remainder)),
  #     .mean = p.trend,
  #     .model = paste0("mltrend(", enquo(response), ")")[2]
  #   ) %>%
  #   relocate(.mean, .after = !!enquo(response)) %>%
  #   select(key, index(trend),!!enquo(response), .mean) %>%
  #   as_fable(response = toString(ensym(response)),
  #            distribution = !!enquo(response))
  
  trend <- trend %>% 
    bind_cols(p.d1 = predito) %>%
    group_by(key) %>%
    mutate(p.trend = cumsum(p.d1)) %>%
    ungroup() %>%
    inner_join(remainder) %>%
    mutate(
      !!enquo(response) := p.remainder + p.trend,
      remainder = p.remainder,
      .mean = p.trend + .mean,
      .model = paste0("mltrend(", enquo(response), ")")[2]
    ) %>%
    relocate(.mean, .after = !!enquo(response)) %>%
    select(key, .model, index(trend),!!enquo(response), .mean, remainder) %>%
    as_fable(response = toString(ensym(response)),
             distribution = !!enquo(response))

  return(trend)
}

# CORPO antigo da função predict.mltrend
# AQUI TA O PROBLEMA?
# como fazer para predizer somente o que pode, sem corromper a predição do trend?
# remainder <- m.d %>% 
#   forecast(new_data = newdata %>% filter(ano_mes > max(m$model$ano_mes))) 
#   # forecast(new_data = newdata) 
#   # refit(new_data = newdata) %>% 
#   # augment() %>% 
#   # rename(p.remainder = remainder)
# 
# # browser()
# trend <- trend %>%
#   bind_cols(p.d1 = predito) %>%
#   group_by(key) %>%
#   mutate(p.trend = cumsum(p.d1)) %>%
#   ungroup() %>%
#   inner_join(remainder) %>%
#   mutate(!!enquo(response) := p.trend + remainder, 
#          .mean = p.trend + .mean,
#          .model = paste0("mltrend(", enquo(response),")")[2]) %>% 
#   relocate(.mean, .after = !!enquo(response)) %>% 
#   select(key, .model, index(trend), !!enquo(response), .mean) %>% 
#   as_fable(response = enquo(response),
#            distribution = !!enquo(response))

#TODO fazer método que pegue o modelo e os dados, como acima, 
# e retorne a série prevista final (compilados os números preditos do ml em 
# tendência e unidos às predições de sazonalidade etc)

# data = meds.treino
# group = vars(no_identificador, via)
# columns = . ~ qt_concorrentes +
#   (via +
#    linha_tto_periferica +
#    somente_adulto +
#    no_forma_organizacao +
#    doses_inducao +
#    doses_mensais) * (qt_concorrentes) +
#   (no_identificador * (ano_mes + qt_total_usuarios))
# 
# 
# decomposition <- data %>%
#   model(MLT = STL(qt_usuarios ~ trend(12))) %>%
#   components() %>%
#   group_by(!!! group) %>%
#   mutate(d1 = difference(trend))
# 
# data = data %>% left_join(decomposition)
# 
# # model = lm(update.formula(d1 ~ ., columns), data = data)
# 
# model = data %>% model(TSLM(update.formula(d1 ~ ., columns)))
# 
# summary(model)
# 
# model = step(model)
# 
# meds.cast <- meds %>% bind_cols(predito = predict(model, newdata = meds)) %>% left_join(decomposition)
# 
# 
# rm(data, group, columns, decomposition, model)



# teste com decomposition_model
# 

