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

