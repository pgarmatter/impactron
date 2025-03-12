library(microdatasus)
library(tidyverse)

select <- dplyr::select

ufs <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")

pcdts <- read.csv2('pcdts.csv')

fetch_uf <- function(uf, ano_inicio, ano_fim = ano_inicio, mes_inicio = 1, mes_fim = mes_inicio) {
  d.pa <- fetch_datasus(
    year_start = ano_inicio,
    month_start = mes_inicio,
    month_end = mes_fim,
    year_end = ano_fim,
    information_system = "SIA-PA",
    uf = uf
  ) %>%
    filter(PA_CIDPRI != "0000", substr(PA_PROC_ID, 1, 2) == "06")
  
  d.apac <- fetch_datasus(
    year_start = ano_inicio,
    month_start = mes_inicio,
    month_end = mes_fim,
    year_end = ano_fim,
    information_system = "SIA-AM",
    uf = uf
  )
  
  dados <- d.pa %>% 
    filter(as.numeric(PA_AUTORIZ) > 0) %>% 
    inner_join(d.apac, join_by(PA_AUTORIZ == AP_AUTORIZ, 
                               PA_CODUNI == AP_CODUNI), 
               multiple = "all", 
               relationship = "many-to-many") %>% 
    mutate(ano_mes = yearmonth(paste(PA_CMP), format="%Y%m"),
           uf = paste(uf)) %>% 
    rename(nu_cnspcn = AP_CNSPCN, 
           co_sigtap = PA_PROC_ID,
           co_cid_principal = PA_CIDPRI,
           co_cid_secundario = PA_CIDSEC) %>% 
    select(ano_mes, uf, nu_cnspcn, co_sigtap, co_cid_principal, co_cid_secundario)
  
  dados
}

etl <- function(){
  
}

ultima.data <- yearmonth("2022-06", format="%Y-%M")

sus.check <- tibble()
cids.check <- tibble()

if(file.exists("sus.check")){
  load("sus.check")
  ultima.data <- max(c((sus.check %>% 
    filter(baixado == T) %>% 
    summarise(data = max(ano_mes)))$data, ultima.data))
}

sus.check <- sus.check %>% 
  bind_rows(tibble(ano_mes = yearmonth(seq(as.Date(ultima.data)+1, as.Date(today()), by = "1 month"))) %>% 
  mutate(ano = year(ano_mes), mes = month(ano_mes)) %>% 
  cross_join(tibble(uf = ufs)) %>% 
  mutate(baixado = F))


if(file.exists("cids.check")){
  load("cids.check")
}

cids.check <- cids.check %>% 
  bind_rows(sus.check %>% 
              filter(baixado == F) %>% 
              select(-baixado) %>% 
              cross_join(tibble(cid = pcdts$cid[-1])) %>% 
              mutate(processado = F)) %>% unique()

d = data.frame()
# d <- fetch_uf('all', ano_inicio = 2022)
# tic()
# for(uf in ufs){
#   d <- d %>% bind_rows(fetch_uf(uf, ano_inicio = 2022))
# }
# toc()



