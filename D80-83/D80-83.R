---
  title: "D80X, D83X - Imunodeficiência Primária Com Predominância De Defeitos De Anticorpos"
author: "Pietro Garmatter"
date: "`r Sys.Date()`"
output: html_document
---
  
  ```{r}
setwd("E:/GEATS/Estimador-AIO-SUS/D59")
library(fpp3)
library(tidyverse)
library(RPostgres)
library(DBI)
library(ggfortify)
library(MASS)
# library(GGally)
# library(ggdist)
# library(splines)
# library(patchwork)

select <- dplyr::select


load("D59.RData")

soma <- soma %>% 
  as_tsibble() %>% 
  mutate(d1 = difference(qt_total_usuarios))

```

# Exploratória

```{r, grafico}
autoplot(soma, qt_total_usuarios)
```
```{r}
mean(soma$qt_total_usuarios)
var(soma$qt_total_usuarios)
```

Superdispersão, não da para usar Poisson normal. Quasi-poisson? Não da para avaliar AIC no quasi-poisson, mas ainda conseguiria avaliar RMSE nos resíduos para comparar aos outros modelos.

```{r}
unique(tratamentos$no_procedimento)
```

Somente IMUNOGLOBULINA HUMANA é usada no tratamento, diversas doses.

```{r}
gg_tsdisplay(soma, y = qt_total_usuarios ,plot_type = "partial")

gg_tsdisplay(soma, y = d1 ,plot_type = "partial")
```

Estacionária, com picos de variância no inicio e fim da série, tende a zero na ACF (Estacionaridade). ACF e PACF sugerem um processo ARMA(1,5).

```{r}
ljung_box(soma$qt_total_usuarios, lag = 12) # rejeita H0
ljung_box(soma$qt_total_usuarios, lag = 24) # rejeita H0

ljung_box(difference(soma$qt_total_usuarios), lag = 12)
ljung_box(difference(soma$qt_total_usuarios), lag = 24) # rejeita H0

ljung_box(difference(soma$qt_total_usuarios, differences = 2), lag = 12)
ljung_box(difference(soma$qt_total_usuarios, differences = 2), lag = 24)
```

No teste de LB, rejeita a H0 para lag = 1 ano.

```{r}
ggcpgram(soma$qt_total_usuarios)
```

ACF, LB e CPGRAM indicam não ser ruído branco.

# Modelagem

Como só tem uma droga incorporada, podemos ajustar um modelo linear simples com o ano_mes como variável preditora. Devido à superdispersão, não da para usar poisson no glm, mas possivelmente quasi-poisson ou binomial negativa.
Base separada temporalmente até maio de 2020, para ter 2 anos completos de dados para a validação.

```{r}
soma.treino <- soma %>% filter(ano_mes < yearmonth("2020/06"))

models <- soma.treino %>% 
  model(arima115 = ARIMA(qt_total_usuarios ~ 1 + pdq(1,1,5) + PDQ(0,0,0)),
        arima = ARIMA(qt_total_usuarios),
        AAN = ETS(qt_total_usuarios ~ error("A") + trend("A") + season("N")),
        MAN = ETS(qt_total_usuarios ~ error("M") + trend("A") + season("N")),
        lm = TSLM(qt_total_usuarios ~ ano_mes))

quasipoisson = glm(qt_total_usuarios ~ ano_mes, soma.treino, family = quasipoisson())

binomialneg = glm.nb(qt_total_usuarios ~ ano_mes, soma.treino)

glance(models) %>% 
  select(.model, AIC) %>% 
  bind_rows(data.frame(.model = c('quasipoisson', 'binom.neg'), AIC = c(NA, AIC(binomialneg)))) %>% 
  arrange(AIC) 
```

Em questão de AIC, o modelo linear simples foi o melhor para a base de treino até maio de 2020.

# Análise de resíduos

falta avaliar os intervalos de predição de quasip e nbinom.

```{r}
cast <- models %>% 
  forecast(new_data = soma %>% filter(ano_mes >= yearmonth("2020/06")))


cast %>% 
  accuracy(soma,
           list(REQM = RMSE,
                winkler = winkler_score), level = 95)
```

Melhor winkler score entre os modelos foi para AAN (erro e tendencia aditivos, sem sazonalidade - método linear de Holt), mas teve o pior REQM. O melhor REQM foi o ARIMA proposto com base na análise da FAC e FACP (ARIMA(1,1,5)(0,0,0)).
Comparando os dois modelos, arima115 ficou em terceiro para escore e primeiro para REQM, enquanto o Holt ficou em primeiro quanto ao escore, e último quanto ao REQM. No caso da predição qual importa mais?
  
  ```{r}

cast %>% 
  autoplot(soma) +
  facet_wrap(~ .model)
```
Olhando os gráficos, o ARIMA115 parece muito com o naive, enquanto o Holt consegue capturar uma certa tendencia crescente e abarcar a maior incerteza no seu intervalo de confiança. Particularmente eu preferiria o Holt, mas o escore seria o suficiente para justificar a escolha?
  Faria sentido analisar outros intervalos para o treinamento? Predições de 12 e 24 meses são as maiores que podem ser validadas para bases de treino até 2020/06


```{r}
save.image('D59.RData')

```
