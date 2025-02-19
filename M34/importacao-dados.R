setwd("E:/GEATS/Estimador-AIO-SUS/M34")
select <- dplyr::select

CID = c("M340", "M341", "M348")

db <- 'sabeis'
host_db <- 'localhost'
db_port <- '5432'
db_user <- 'postgres'
db_password <- 'pgres28'

con <-
  dbConnect(
    RPostgres::Postgres(),
    dbname = db,
    host = host_db,
    port = db_port,
    user = db_user,
    password = db_password,
    options="-c search_path=public"
  )

# dbListTables(con)
tb_medicamentos <- tbl(con, "tb_medicamento")


# SIGTAPS
# !!!!! cada PCDT pode ter que ser filtrado de forma diferente e seu no_identificador pode mudar, 
# pois as siglas não separam unicamente os medicamentos !!!!!
tb_sigtap <- tbl(con, "td_medicamento_sigtap") %>%
  as.data.frame() %>%
  separate(
    co_sigtap,
    sep = ";",
    into = paste0('co_sigtap', 1:5) # ajustado para os 5 sigtaps de adal40
  ) %>%        # separa os sigtaps de uma coluna para 3
  pivot_longer(
    cols = starts_with("co_sigtap"),
    values_drop_na = T,
    values_to = "co_sigtap"
  ) %>%  # separa sigtaps diferentes em linhas diferentes
  mutate(no_forma_organizacao = as.factor(trimws(no_forma_organizacao)),
         co_sigtap = as.integer(trimws(co_sigtap))) %>%
  filter(substr(co_sigtap, 1,1) == "6") %>%
  select(co_sigtap, sg_procedimento, no_forma_organizacao, no_procedimento) %>%
  mutate(no_identificador = paste0(gsub('([A-Z]+)\\d+.*', '\\1', sg_procedimento), substring(co_sigtap, 1, 5))) 

# Forma organização
tb_organizacao <- as.data.frame(
  read.csv(
    "../sigtap-forma-organizacao",
    sep = ",",
    header = F,
    col.names = c("codigo", "org"),
    encoding = "UTF-8"
  )
) %>%
  transmute(codigo_organizacao = codigo, org = str_trim(tolower(org)))

# linhas de tratamento
tratamentos <- as.data.frame(read.csv(
  'linhas_tto.csv',
  sep = ";",
  header = T,
  encoding = "UTF-8"
),) %>%
  inner_join(tb_sigtap)

# base não compilada
base <- tb_medicamentos %>%
  filter(co_cidpri %in% CID) %>% 
  as_tibble() %>% 
  mutate(ano_mes = yearmonth(paste(nu_competencia), format="%Y%m")) %>% 
  rename(co_sigtap = co_procedimento) %>%
  inner_join(tratamentos) %>%
  inner_join(tb_sigtap) 

# concorrentes, considerando linhas anteriores como concorrencia
concorrentes <- base %>% 
  select(ano_mes, no_identificador, via, linha_tto) %>% 
  unique() %>% 
  mutate(numero = 1) %>% 
  pivot_wider(names_from = linha_tto,
              values_from = numero,
              names_prefix = 'linha_tto_') %>%
  group_by(ano_mes) %>%
  summarise(
    qt_conc_1 = sum(linha_tto_1, na.rm = T),
    qt_conc_2 = qt_conc_1 + sum(linha_tto_2, na.rm = T),
  ) %>%
  pivot_longer(
    cols = starts_with('qt_conc'),
    names_prefix = 'qt_conc_',
    names_to = 'linha_tto',
    values_to = 'qt_concorrentes',
    names_transform =list(linha_tto = as.integer)
  ) 

#soma
soma <- base %>% 
  group_by(ano_mes) %>% 
  summarise(qt_total_usuarios = n_distinct(nu_cnspcn)) %>% 
  filter(ano_mes < max(ano_mes)) %>% 
  tsibble(index = ano_mes)

# medicamentos
meds <- base %>%
  group_by(
    ano_mes,
    no_identificador,
    no_forma_organizacao,
    via,
    linha_tto
  ) %>%
  summarise(qt_usuarios = n_distinct(nu_cnspcn)) %>%
  arrange(ano_mes) %>%
  ungroup() %>%
  inner_join(concorrentes) %>%
  inner_join(soma) %>%
  select(
    ano_mes,
    no_identificador,
    qt_usuarios,
    qt_total_usuarios,
    no_forma_organizacao,
    via,
    linha_tto,
    qt_concorrentes
  ) %>%
  mutate(no_forma_organizacao = droplevels(no_forma_organizacao)) %>%
  filter(ano_mes < max(ano_mes)) %>% # filtra ultimo mes (potencial subdimensionamento)
  tsibble(index = ano_mes, key = c(no_identificador, via))

pcen <- meds %>% 
  inner_join(soma) %>% 
  mutate(pct_usuarios = qt_usuarios / qt_total_usuarios * 100)

save.image(file = "M34.RData") 
