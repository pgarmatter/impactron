with(e.ea, {
  newdata = 
    meds %>% append_row(meds, n = 24) %>% 
    fill(everything()) %>% 
    filter(ano_mes > max(meds$ano_mes))

  meds.models %>%
    select(-D) %>%
    refit(meds) %>%
    forecast(new_data = newdata) %>%
    autoplot(meds) +
    facet_wrap(~key +.model, scales = "free")

# STL
  meds %>%
    filter(key == meds$key[1]) %>%
    model(meds.models$D[[1]]$model) %>% forecast(newdata) %>%
    autoplot(meds %>% filter(key == meds$key[1]))

  meds %>%
    model(meds.models$D[[1]]$model) %>%
    forecast(newdata) %>%
    autoplot(meds %>% filter(key == meds$key[1]))

})


with(e.ea, {
  models %>%
    pivot_longer(everything(),
                 names_to = '.model',
                 values_to = 'modelo') %>% 
    split(.$.model) %>%
    map(
      ~ plotOutput(outputId = paste0("modelos.graficos.dados$", .$.model))
    )
}) 

with(e.ea, { 
  models %>%
    forecast(h = 12) %>%
    split(.$.model) %>%
    map(
      ~ autoplot(., soma) +
        scale_x_yearmonth(date_breaks = '2 years', date_labels = '%Y') +
        geom_vline(xintercept = as.Date(max(soma.treino$ano_mes) + c(12, 24)), colour = "red") +
        theme(legend.position = "none") +
        labs(title = .$.model,
             caption = "linhas verticais representam 12 e 24 meses de predição.",
             y = "número de pacientes")
    )
  
  
})
