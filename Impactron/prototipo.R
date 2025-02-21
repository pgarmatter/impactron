require(shiny)
require(bslib)
require(htmltools)
require(DT)
require(xtable)

# list.of.packages <- c("remotes")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# 
# if(!("microdatasus" %in% installed.packages()[,"Package"])){
#   remotes::install_github("rfsaldanha/microdatasus")
# }
# 
# library(microdatasus)

library(kableExtra)

library(fpp3)
library(tidyverse)
library(DBI)
library(GGally)
library(ggdist)
library(ggfortify)
library(MASS)
library(splines)
library(patchwork)
library(distributional)

select <- dplyr::select

pcdts <- read.csv2('pcdts.csv')
cid <- ''
series <- c()
envs <- list()

getNomeSerie <- function(id, via){
  paste(id, via)
}

getkey <- function(nome_serie){
  # nome_serie
  envs[[cid]]$nome_serie = nome_serie
  with(envs[[cid]], {
    (tratamentos %>% filter (no_serie == nome_serie))$key[1]
  })
}

modelName <- function(nome){
  gsub('<|>','',nome) %>% 
    gsub('w/', 'com', .) %>% 
    sub('mean', 'média', .) %>% 
    sub('TSLM', 'Modelo linear com componentes de séries temporais', .) %>% 
    sub('LM com (.*) errors', 'LM com erros \\1', .) %>% 
    sub('LM', 'Modelo linear', .) %>% 
    sub('NULL model', '-', .) %>% 
    sub('ETS(A,A,N)', 'Suavização exponencial de Holt', ., fixed = T) %>% 
    sub('ETS(M,A,N)', 'Suavização exponencial de Holt com erros multiplicativos', ., fixed = T) %>% 
    sub('STL decomposition model', 'Modelo de decomposição em trend + remainder', .)
}

valida <- function(nome, serie){
  if(nome == '' || !(nome %in% serie)){
    NULL
  }else{
    nome
  }
}

ui <- page_sidebar(
  title = "IMPACTRON",
  sidebar = sidebar(
    selectInput(
      inputId = "cid",
      label = "PCDT",
      choices = pcdts$nome,
      selected = '',
      selectize = T
    ),
    selectInput(
      inputId = "droga",
      label = "Série",
      choices = "",
      selected = NULL
    ),
    width = '300px'
  ),
  navset_card_tab(
    title = textOutput("pcdt"),
    nav_panel(
      title = "Exploratória",
      h1("Análise de quantidade de pacientes"),
      tableOutput('stats'),
      layout_column_wrap(
        width = 400,
        heigth = 400,
        card(card_header('Série temporal'), card_body(plotOutput("ts"))),
        card(card_header('Periodograma acumulado'), card_body(plotOutput("cpgram"))),
        card(card_header('FAC'), card_body(plotOutput("acf"))),
        card(card_header('FACP'), card_body(plotOutput("pacf")))
      ),
      tableOutput("tratamentos")
    ),
    nav_panel(
      title = "Modelos",
      tableOutput("modelos"),
      htmlOutput("accuracy"),
      card(card_header('Gráficos'), 
           card_body(plotOutput("modelos.graficos")),
           min_height = 500)
      # card(card_header('Resíduos'), 
      #      card_body(plotOutput("modelos.graficos.residuos")),
      #      min_height = 500)
    ),
    nav_panel(
      title = "Estimativas",
      layout_columns(
        col_widths = c(5, 5, 2),
        fill = F,
        selectInput(
          inputId = "s.model",
          label = "Modelo",
          choices = "",
          selectize = T
        ),
        numericInput(
          inputId = "meses",
          label = "Meses",
          value = 12,
          min = 1,
          max = 60
        ),
        actionButton(
          inputId = 'forecast', 
          label = "Estimar",
          icon = icon("chart-line"),
          style = "height: 100%"
        )
      ),
      layout_column_wrap(
        width = '600px',
        heights_equal = 'row',
        fillable = F,
        card(min_height = 500, full_screen = T, card_header(textOutput("forecast.graficos.title")), card_body(uiOutput(outputId ="forecast.graficos"))),
        card(min_height = 500, full_screen = T, card_header(textOutput("forecast.tabela.title")), card_body(tableOutput("forecast.tabela")))
      )
    )
  )
)

# ui <- page_navbar(
#   navset_card_tab(
#    ...
#   )
# )

server <- function(input, output, session) {
  
  envs <<- list()
  
  output$pcdt = renderText(paste((pcdts %>% filter(nome == input$cid))$nome, collapse = ' - '))

  series <- reactive({
    cid <<- (pcdts %>% filter(nome == input$cid))$cid
    if(!(cid %in% names(envs))){
      envs[[cid]] <<- new.env()
      print(paste(c('Criado env', cid, names(envs)), sep = " - "))
      load(paste0('../', cid, '/', cid, '.RData'), envir = envs[[cid]])
      with(envs[[cid]], {
        if(!('via' %in% colnames(tratamentos))){
          tratamentos <- tratamentos %>% mutate(via = '')
        }
        tratamentos <- tratamentos %>%
          mutate(no_legivel = str_to_sentence(sub(
            "(\\w) ?\\d.*", "\\1", no_procedimento, ignore.case = T
          )),
          no_serie = paste(no_legivel, via, sep = " - "),
          key = paste(no_identificador, via, sep = '_'))
        if(!('key' %in% colnames(meds))){
          meds <- meds %>% mutate(key = paste0(no_identificador, '_'))
        }
      })
    }
    
    if(!is.null(envs[[cid]])){
      with(envs[[cid]],{
        if(length(unique(tratamentos$key)) > 1){
          c("Demanda total", (tratamentos %>% distinct(no_serie))$no_serie)
        }else{
          c("Demanda total")
        }
      })
    }else{
      c("Selecione o PCDT")
    }
  }) %>% 
    bindEvent(valida(input$cid, pcdts$nome), ignoreInit=T, ignoreNULL=T)

  observe({
    updateSelectInput(session, "droga", label = "Séries", choices = series())
  })
  
  data <- reactive({
    with(envs[[cid]], {
      if(input$droga == series()[1]){
        soma %>% mutate(qt_usuarios = qt_total_usuarios)
      }else{
        meds %>% filter(key == getkey(input$droga))
      }
    })
  }) %>% bindEvent(valida(input$droga, series()), ignoreInit = T, ignoreNULL=T)

  #### EXPLORATORIA
  output$stats = renderTable({
    # ver como fazer com kable (html)
    data() %>%
    as_tibble %>%
    summarise(mínimo = min(qt_usuarios),
              máximo = max(qt_usuarios),
              média = mean(qt_usuarios),
              mediana = median(qt_usuarios),
              variância = var(qt_usuarios)) %>%
    bind_cols(tibble(LjungBox_12 = ljung_box(data()$qt_usuarios, lag = 12)["lb_pvalue"],
                     LjungBox_24 = ljung_box(data()$qt_usuarios, lag = 24)["lb_pvalue"]))
  })
  
  output$ts = renderPlot({
    data() %>% autoplot(qt_usuarios) + scale_x_yearmonth(date_breaks = '1 year', date_labels='%Y')
  })
    
  output$acf = renderPlot({
    data() %>% ACF(qt_usuarios) %>% autoplot()
  })
  
  output$pacf = renderPlot({
    data() %>% PACF(qt_usuarios) %>% autoplot()
  })
  
  output$cpgram = renderPlot({
    ggcpgram(data()$qt_usuarios)
  })
  
  #### MODELOS
  
  t.data <- reactive({
    with(envs[[cid]], {
      if(input$droga == series()[1]){
        soma.treino %>% mutate(qt_usuarios = qt_total_usuarios)
      }else{
        meds.treino %>% filter(key == getkey(input$droga))
      }
    })
  }) %>% bindEvent(valida(input$droga, series()), ignoreInit = F, ignoreNULL = T)
  
  m.data <- reactive({
    with(envs[[cid]], {
      if(input$droga == series()[1] || !exists('meds.models')){
        m <- models
      }else{
        m <- meds.models %>% 
          filter(key == getkey(input$droga))  
          # select(!starts_with('key'))
      }
      m
    })
  }) %>% bindEvent(valida(input$droga, series()), ignoreInit = F, ignoreNULL = T)
  
  # m.nomes <- reactive({
  #   m.data() %>% pivot_longer(cols = !starts_with('key'),
  #                      names_to = 'nome',
  #                      values_to = 'model_spec_mdl') %>%
  #     mutate(modelo = modelName(format(model_spec_mdl))) %>% 
  #     column_to_rownames(var="modelo")
  # })
  
  m.nomes <- reactive({
    m.data() %>% pivot_longer(
      cols = !starts_with('key'),
      names_to = 'nome',
      values_to = 'model_spec_mdl'
    ) %>%
      mutate(modelo = modelName(format(model_spec_mdl)))
  })
  
  f.acc <- reactive({
    with(envs[[cid]], {
      if(input$droga == series()[1] || !exists('meds.models')){
        sm.f.acc
      }else{
        mm.f.acc %>% filter(key == getkey(input$droga))
      }
    })
  }) %>% bindEvent(valida(input$droga, series()), ignoreInit = F)
  
  output$modelos = renderTable({
    # print('atualizando tabela m.nomes')
    m.nomes() %>% 
      select(!starts_with('key')) %>% 
      as_tibble() %>% 
      select(-model_spec_mdl) 
  })
  
  output$accuracy = renderText({
    boldmap = f.acc() %>% 
      mutate(across(starts_with(c("AIC", "REQM", "wink")), ~ (.x == min(.x, na.rm = T)) %in% T)) 
    
    f.acc() %>% 
      select(-starts_with('key')) %>% 
      knitr::kable(align = 'c', format = "html") %>% #, caption = "Análise de acurácia dos modelos"
      # knitr::kable(align = 'c', format = 'latex') %>%
      column_spec(2, bold = boldmap$AIC) %>% 
      column_spec(3, bold = boldmap$REQM.0) %>% 
      column_spec(4, bold = boldmap$REQM.12) %>% 
      column_spec(5, bold = boldmap$REQM.24) %>%
      column_spec(6, bold = boldmap$winkler.12) %>% 
      column_spec(7, bold = boldmap$winkler.24) 
  })
  
  output$modelos.graficos = renderPlot({
    newdata = data() %>% 
      filter(ano_mes > max(t.data()$ano_mes), ano_mes <= max(t.data()$ano_mes) + 24)
    if(input$droga == series()[1] || !exists('meds.models')){
      newdata = newdata %>% select(-qt_usuarios)
    }
 
    m.data() %>%
      forecast(new_data = newdata) %>%
      filter(!is.na(.mean)) %>%
      autoplot(data()) +
      facet_wrap( ~ .model, scales = "free") +
      scale_x_yearmonth(date_breaks = '2 years', date_labels='%Y') +
      geom_vline(xintercept = as.Date(max(t.data()$ano_mes) + c(12, 24)), colour = "red") +
      theme(legend.position="none") +
      labs(title = paste("Modelos ajustados para ", input$droga),
         caption = "linhas verticais representam 12 e 24 meses de predição.",
         y = "número de pacientes")
  })
  
  output$modelos.graficos.residuos = renderPlot({
    # m.nomes() %>% autoplot()
  })

  #### ESTIMATIVAS
  observe({
    updateSelectInput(session, "s.model", "Modelos", choices = m.nomes()$modelo)
  })
  
  output$forecast.graficos = renderUI({
    if (is.null(input$s.model) || input$s.model == "") {
      return(h3('Selecione um modelo e um período (meses) para estimar.'))
    }
    
    return(plotOutput(outputId = "forecast.graficos.plot")) #, width = 500, height = 400
  }) %>%
    bindEvent(input$forecast)
  
  forecast.data = reactive({
    meses = min(max(input$meses, 0), 60)
    
    # chavear entre modelos caso necessário (STL, nb)
    modelo = (m.nomes() %>%
        filter(modelo == input$s.model))$model_spec_mdl[[1]]
        
    response = modelo$response[[1]]
    
    newdata = data() %>%
      append_row(data(), n = meses) %>%
      fill(everything()) %>%
      filter(ano_mes > max(data()$ano_mes))
    
    switch(modelo$model$model,
      'dcmp_mdl'={
        modelo = data() %>% 
          model(modelo$model)
      },
      {
        modelo = modelo %>% 
          refit(data())
      }
    )

    modelo %>% 
      forecast(new_data = newdata) %>% 
      select(any_of(response), .mean)
  }) %>%
    bindEvent(input$forecast)
  
  output$forecast.graficos.title = renderText({
    if(input$droga != ''){
      paste("Gráfico de pacientes preditos para", input$droga)
    }else{
      "Gráfico de pacientes preditos"
    }
  }) %>% bindEvent(input$forecast, ignoreNULL = F)
  
  output$forecast.tabela.title = renderText({
    if(input$droga != ''){
      paste("Números preditos de pacientes para", input$droga)
    }else{
      "Números preditos de pacientes"
    }
  }) %>% bindEvent(input$forecast, ignoreNULL = F)
  
  output$forecast.graficos.plot = renderPlot({
    forecast.data()%>%
      autoplot(data() %>% select(starts_with('qt_'))) +
      scale_x_yearmonth(date_breaks = '1 years', date_labels='%Y') +
      geom_vline(xintercept = as.Date(max(data()$ano_mes) + seq(12, min(max(input$meses, 0), 60), 12)), colour = "red") +
      theme(legend.position="none") +
      labs(caption = "linhas verticais representam intervalos de 12 meses de predição.",
           y = "número de pacientes")
  }) %>% 
    bindEvent(input$forecast)
  
  output$forecast.tabela = renderTable({
    forecast.data() %>%
      as_tibble() %>%
      rename(qt_usuarios = starts_with('qt_')) %>%
      mutate(
        ano_mes = format(ano_mes),
        qt95 = hilo(qt_usuarios, 95),
        inf95 = qt95$lower,
        sup95 = qt95$upper,
        qt80 = hilo(qt_usuarios, 80),
        inf80 = qt80$lower,
        sup80 = qt80$upper
      ) %>%
      select(-starts_with('qt')) %>%
      rename(media = .mean) %>%
      relocate(ano_mes, media, inf80, sup80, inf95, sup95)
  }) %>% 
    bindEvent(input$forecast)
}

shinyApp(ui, server)
