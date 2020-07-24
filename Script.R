library(readr); library (dplyr); library(ggplot2); library(tidyverse); library(utils)

df <- read_delim("banco_de_dados/csv_sistemas.csv",
";", escape_double = FALSE, col_types = cols(DATA_NOTIFICACAO = col_date(format = "%m/%d/%Y")), trim_ws = TRUE)

View(df)

##Perguntas

#1. Qual o perfil da curva de crescimento (número de casos acumulados)na população acima dos 60 anos?


#2. Existe alguma associação entre comorbidades e a evolução do quadro clínico?
  
#3. Existe alguma diferenciação entre os sexos no que se relaciona à evolução do quadro clínico?

  ## Grafico 3.1 - Contagem total por sexo faceting por Evoluçao
df %>%
  select(SEXO,EVOLUCAO,CLASSIFICACAO_CASO) %>%
  filter(SEXO %in% c('FEMININO', 'MASCULINO')) %>% 
  count(EVOLUCAO,SEXO) %>%
  ggplot(aes(x=SEXO, y=n)) + 
  geom_col() + facet_wrap(~EVOLUCAO, scales = "free_y") + ggsave(filename = "grafico3.1.png")

  ## Grafico 3.2 - Contagem acumulada por sexo faceting por Evolução 

df %>%
  select(SEXO,EVOLUCAO,DATA_NOTIFICACAO) %>%
  filter(SEXO %in% c('FEMININO', 'MASCULINO')) %>% 
  count(DATA_NOTIFICACAO,SEXO,EVOLUCAO) %>% 
  mutate(n_acumulado=cumsum(n)) %>%
  rename(confirmados=n_acumulado) %>%
  drop_na(DATA_NOTIFICACAO) %>%
  ggplot(aes(x=DATA_NOTIFICACAO, y=confirmados, color=SEXO)) + 
  geom_line() + facet_wrap(~EVOLUCAO, scales = "free_y") + ggsave(filename = "grafico3.2.png")
  
  ## Gráfico 3.3 - Total casos confirmados por Sexo. 

df %>%
  select(SEXO,CLASSIFICACAO_CASO) %>%
  filter(SEXO %in% c('FEMININO', 'MASCULINO')) %>% 
  count(SEXO) %>%
  ggplot(aes(x=SEXO, y=n)) + 
  geom_col() + ggsave(filename = "grafico3.3.png")

###Conclusão dos gráficos: Por sexo não há diferença. Mas há maior número de casos confirmados do sexo masculino?
  

#4. Municípios no Norte e Nordeste possuem piores evoluções dos quadros clínicos em comparação com Belo Horizonte e suas regionais?

