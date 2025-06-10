# remotes::install_github("rfsaldanha/microdatasus")
library(microdatasus)
library(tidyverse)

# dados <- fetch_datasus(year_start = 2022, month_start = 6, month_end = 6, year_end = 2022, information_system = "SIA-PA", uf="PR")
# dados <- process_sia(dados)

# PA_PROC_ID = CO_SIGTAP
# PA_CMP = NU_COMPETENCIA

system.time({
  dados <- fetch_datasus(year_start = 2022, 
                month_start = 6, 
                month_end = 6, 
                year_end = 2022, 
                information_system = "SIA-PA", 
                uf="PR") %>% 
    filter(substr(PA_PROC_ID,1,2) == '06',
           PA_CIDPRI %in% c("L400", "L401", "L404", "L408"))
})
# 48.66

system.time({
  apac <- fetch_datasus(year_start = 2022, 
                                   month_start = 6, 
                                   month_end = 6, 
                                   year_end = 2022, 
                                   information_system = "SIA-AD", 
                                   uf="PR")
})

aux <- dados %>% inner_join(sigtap, by=join_by(PA_PROC_ID == COD))