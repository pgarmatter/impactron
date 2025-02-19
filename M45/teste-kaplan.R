colnames(espondilite)
colnames(pct_usuarios)

# historico_paciente <- tb_medicamentos %>%
#   filter(co_cidpri == 'M45', nu_cnspcn > 0) %>%
#   as.data.frame() %>%
#   rename(co_sigtap = co_procedimento) %>%
#   inner_join(tb_sigtap) %>%
#   mutate(nu_cnspcn = as.character(nu_cnspcn)) %>%
#   group_by(nu_cnspcn, no_identificador) %>%
#   summarise(dt_inicio = min(ym(nu_competencia)),
#             dt_fim = max(ym(nu_competencia))) %>%
#   arrange(nu_cnspcn, dt_inicio)

historico_paciente <- tb_medicamentos %>%
  filter(co_cidpri == 'M45', nu_cnspcn > 0) %>%
  as.data.frame() %>%
  rename(co_sigtap = co_procedimento) %>%
  inner_join(tb_sigtap) %>%
  mutate(nu_cnspcn = as.character(nu_cnspcn),
         data = ym(nu_competencia)) %>%
  select(nu_cnspcn, no_identificador, data) %>%
  arrange(nu_cnspcn, data)

historico_paciente %>%
  filter(nu_cnspcn == "100025000028465") %>%
  ggplot(aes(x=data, fill=no_identificador)) +
  geom_dotplot(binwidth = 60)

historico_paciente %>%
  filter(nu_cnspcn == "100098100088436") %>%
  ggplot(aes(x=data, fill=no_identificador)) +
  geom_dotplot(binwidth = 30)

historico_paciente %>%
  filter(nu_cnspcn == "100209900229452") %>%
  ggplot(aes(x=data, fill=no_identificador)) +
  geom_dotplot(binwidth = 30)

cinc_pcns <- sample(unique(historico_paciente$nu_cnspcn), 50)

cem_pcns <- sample(unique(historico_paciente$nu_cnspcn), 100)
cem_pcns2 <- sample(unique(historico_paciente$nu_cnspcn), 100)
cem_pcns3 <- sample(unique(historico_paciente$nu_cnspcn), 100)

jpeg(filename = "Rplot%03d.jpeg", width = 4000, height = 4000, res = 300)
historico_paciente %>%
  filter(nu_cnspcn %in% cem_pcns) %>%
  inner_join(tratamentos) %>%
  ggplot(aes(x=data, y=nu_cnspcn, color=no_identificador, shape=no_identificador)) +
  geom_line(alpha=0.7, linewidth=1, position=ggstance::position_dodgev(height=0.2)) +
  geom_point() +
  scale_x_date(breaks=seq(ym(201001), ym(202206), by="1 year"), date_minor_breaks="month") +
  scale_shape_manual(values=1:nlevels(as.factor(historico_paciente$no_identificador))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

historico_paciente %>%
  filter(nu_cnspcn %in% cem_pcns2) %>%
  inner_join(tratamentos) %>%
  ggplot(aes(x=data, y=nu_cnspcn, color=no_identificador, shape=no_identificador)) +
  geom_line(alpha=0.7, linewidth=1, position=ggstance::position_dodgev(height=0.2)) +
  geom_point() +
  scale_x_date(breaks=seq(ym(201001), ym(202206), by="1 year"), date_minor_breaks="month") +
  scale_shape_manual(values=1:nlevels(as.factor(historico_paciente$no_identificador))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

historico_paciente %>%
  filter(nu_cnspcn %in% cem_pcns3) %>%
  inner_join(tratamentos) %>%
  ggplot(aes(x=data, y=nu_cnspcn, color=no_identificador, shape=no_identificador)) +
  geom_line(alpha=0.7, linewidth=1, position=ggstance::position_dodgev(height=0.2)) +
  geom_point() +
  scale_x_date(breaks=seq(ym(201001), ym(202206), by="1 year"), date_minor_breaks="month") +
  scale_shape_manual(values=1:nlevels(as.factor(historico_paciente$no_identificador))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

historico_paciente %>%
  group_by(nu_cnspcn) %>%
  summarise(qt_medicamentos = n_distinct(no_identificador)) %>%
  group_by(qt_medicamentos) %>%
  summarise(qt_pacientes = n())

#verificando se um cnspcn sempre corresponde à mesma pessoa: Parece que sim, há mudanças de endereço, mas otras info permanecem
pcns_check <- tb_medicamentos %>%
  # filter(as.character(nu_cnspcn) == "102440400620441") %>%
  filter(as.character(nu_cnspcn) %in% cem_pcns) %>%
  as.data.frame() %>%
  rename(co_sigtap = co_procedimento) %>%
  group_by(as.character(nu_cnspcn), sg_sexo, nu_idade, str_to_upper(sg_uf), co_ibge_municipio_residencia, co_cep_residencia) %>%
  summarise(count = n()) %>%
  arrange()

