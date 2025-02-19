setwd("E:/GEATS/Estimador-AIO-SUS/G610")
select <- dplyr::select

CID = c('G610')

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
    into = c("co_sigtap1", "co_sigtap2", "co_sigtap3")
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

# só uma droga, dispensa informações de concorrencia

meds <- base %>% 
  group_by(ano_mes, no_identificador, no_forma_organizacao) %>% 
  summarise(qt_usuarios = n_distinct(nu_cnspcn)) %>% 
  select(
    ano_mes,
    no_identificador,
    qt_usuarios,
    no_forma_organizacao,
    # linha_tto,
    # ordem_tto,
    # doses_inducao,
    # periodo_doses,
    # qt_concorrentes
  ) %>% 
  mutate(no_forma_organizacao = droplevels(no_forma_organizacao)) %>%
  ungroup() %>% 
  filter(ano_mes < max(ano_mes)) %>% # filtra ultimo mes (potencial subdimensionamento)
  tsibble(index = ano_mes, key = no_identificador)

soma <- base %>% 
  group_by(ano_mes) %>% 
  summarise(qt_total_usuarios = n_distinct(nu_cnspcn)) %>% 
  filter(ano_mes < max(ano_mes)) %>% 
  tsibble(index = ano_mes)

pcen <- meds %>% 
  inner_join(soma) %>% 
  mutate(pct_usuarios = qt_usuarios / qt_total_usuarios * 100)

save.image(file = "G610.RData") 
