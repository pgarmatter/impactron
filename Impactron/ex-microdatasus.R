# remotes::install_github("rfsaldanha/microdatasus")
library(microdatasus)
library(tidyverse)
library(fpp3)

# dados <- fetch_datasus(year_start = 2022, month_start = 6, month_end = 6, year_end = 2022, information_system = "SIA-PA", uf="PR")
# dados <- process_sia(dados)

# PA_PROC_ID = CO_SIGTAP
# PA_CMP = NU_COMPETENCIA

ano.i=2018
ano.f=2022
mes.i=1
mes.f=6

anos <- tibble(ano_mes = seq(make_yearmonth(year = ano.i, month = mes.i), 
                             make_yearmonth(year = ano.f, month = mes.f), 1))

soma_ <- cross_join(anos, tibble(qt_total_usuarios = NA)) %>% 
  tsibble(index = ano_mes)

meds_ <- cross_join(anos, tibble(key = unique((tratamentos %>% mutate(key=paste0(no_identificador, '_', via)))$key),
                                 qt_usuarios = NA)) %>% 
  tsibble(index = ano_mes, key = key)

system.time({
  # primeiro ano_mes sem dados
  i = (soma_ %>% filter(is.na(qt_total_usuarios)) %>% head(1) %>% select(ano_mes))$ano_mes
  
  dados <- fetch_datasus(year_start = year(i), year_end = year(i), 
                month_start = month(i), month_end = month(i), 
                information_system = "SIA-PA", timeout = 600) %>% 
    filter(substr(PA_PROC_ID,1,2) == '06',
           PA_CIDPRI %in% c("L400", "L401", "L404", "L408")) %>% 
    select(PA_CODUNI, PA_CMP, PA_PROC_ID, PA_AUTORIZ) %>% 
    rename(co_cnes = PA_CODUNI, ano_mes = PA_CMP, nu_apac = PA_AUTORIZ, co_sigtap = PA_PROC_ID)
  
  apac <- fetch_datasus(year_start = year(i), year_end = year(i), 
                        month_start = month(i), month_end = month(i), 
                        information_system = "SIA-AD") %>% 
    select(AP_AUTORIZ, AP_CNSPCN, AP_CODUNI) %>% 
    rename(nu_apac = AP_AUTORIZ, nu_cnspcn = AP_CNSPCN, co_cnes = AP_CODUNI)
  
  dados <- dados %>% 
    mutate(co_sigtap = as.integer(co_sigtap)) %>% 
    inner_join(apac, by = join_by(nu_apac == nu_apac, co_cnes == co_cnes)) %>% 
    inner_join(tratamentos, by=join_by(co_sigtap)) %>% 
    mutate(ano_mes = yearmonth(ano_mes, '%Y%m'), 
           key = paste0(no_identificador, '_', via))
  
  dados.s <- dados %>% 
    as_tibble %>% 
    group_by(ano_mes) %>% 
    summarise(qt = n_distinct(nu_cnspcn)) %>% 
    tsibble(index = ano_mes)
  
  dados.m <- dados %>% 
    as_tibble %>% 
    group_by(ano_mes, key) %>% 
    summarise(qt = n_distinct(nu_cnspcn)) %>% 
    tsibble(index = ano_mes, key = key)
  
  soma_ <- soma_ %>% left_join(dados.s) %>% 
    mutate(qt_total_usuarios = qt) %>% 
    select(-qt)
  
  meds_ <- meds_ %>% left_join(dados.m) %>% 
    mutate(qt_usuarios = qt) %>% select(-qt)
  
  save(dados, soma_, meds_, file="dados.novos.RData")
})
# 48.66

system.time({
  apac <- fetch_datasus(year_start = ano.i, year_end = ano.f,
                        month_start = mes.i, month_end = mes.f, 
                        information_system = "SIA-AD")
})

aux <- dados %>% inner_join(sigtap, by=join_by(PA_PROC_ID == COD))