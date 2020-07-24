library(readr); library (dplyr); library(ggplot2); library(tidyverse); library(utils)

install.packages('ggThemeAssist')
library(ggThemeAssist)
library(scales)
df <- read_delim("banco_de_dados/csv_sistemas.csv",
";", escape_double = FALSE, col_types = cols(DATA_NOTIFICACAO = col_date(format = "%d/%m/%Y")), trim_ws = TRUE)


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
  geom_col() + facet_wrap(~EVOLUCAO, scales = "free_y") + labs(title = "Números de Casos Confirmados por Gênero",
                                                               subtitle = "Segundo a evolução",
                                                               x = "Gênero",
                                                               y = "Casos Confirmados") + ggsave(filename = "grafico3.1.png")

  ## Grafico 3.2 - Contagem acumulada por sexo faceting por Evolução 

                     

df %>%
  select(SEXO,EVOLUCAO,DATA_NOTIFICACAO) %>%
  filter(SEXO %in% c('FEMININO', 'MASCULINO')) %>%
  mutate(SEXO=factor(SEXO)) %>%
  count(DATA_NOTIFICACAO,EVOLUCAO,SEXO) %>%
  group_by(EVOLUCAO,SEXO) %>%
  mutate(n_acumulado=cumsum(n)) %>%
  rename(confirmados=n_acumulado) %>%
  ggplot(aes(x=DATA_NOTIFICACAO, y=confirmados, color=SEXO)) + 
  geom_line() + facet_wrap(~EVOLUCAO, scales = "free_y") +  labs(title = "Acumulado de Casos Confirmados por Gênero",
                                                                subtitle = "Segundo a evolução",
                                                                x = "Gênero",
                                                                y = "Casos Confirmados", color="Gênero")  + ggsave(filename = "grafico3.2.png")
  


## Gráfico 3.3 - Total casos confirmados por Sexo. 

df %>%
  select(SEXO,CLASSIFICACAO_CASO) %>%
  filter(SEXO %in% c('FEMININO', 'MASCULINO')) %>% 
  count(SEXO) %>%
  ggplot(aes(x=SEXO, y=n)) + 
  geom_col() +  labs(title = "Total Casos Confirmados por Gênero",
                                                                x= "Gênero",                                                               
                                                                y = "Casos Confirmados") + ggsave(filename = "grafico3.3.png")

###Conclusão dos gráficos 3.1, 3.2 e 3.3: Por sexo não há diferença. Mas há maior número de casos confirmados do sexo masculino.
  
## Grafico 3.4 - Contagem total por faixa etária faceting por Evoluçao
df %>%
  select(FAIXA_ETARIA,EVOLUCAO,CLASSIFICACAO_CASO) %>%
  count(EVOLUCAO,FAIXA_ETARIA) %>%
  ggplot(aes(x=FAIXA_ETARIA, y=n)) + 
  geom_col() + facet_wrap(~EVOLUCAO, scales = "free_y") +   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  labs(title = "Total Casos Confirmados por Faixa etária", subtitle = "Segundo a evolução", x="Faixa Etária",                                                                                                                           x= "Gênero",                                                                x = "Gênero",
                                                                                                                             y = "Casos Confirmados") + ggsave(filename = "grafico3.4.png")

## Grafico 3.5 - Contagem acumulado por Faixa Etaria faceting por Evolução 


df %>%
  count(DATA_NOTIFICACAO,FAIXA_ETARIA,EVOLUCAO) %>%
  group_by(FAIXA_ETARIA,EVOLUCAO) %>% 
  mutate(n_acumulado=cumsum(n)) %>%
  rename(confirmados=n_acumulado) %>%
  drop_na(DATA_NOTIFICACAO) %>%
  ggplot(aes(x=DATA_NOTIFICACAO, y=confirmados, color=EVOLUCAO)) + 
  geom_line() + facet_wrap(~FAIXA_ETARIA, scales = "free_y") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Acumulado de Casos Confirmados por Faixa Etária",
                                                                                                                          subtitle = "Segundo a evolução",
                                                                                                                          x = "Data de notificação",
                                                                                                                          y = "Casos Confirmados", fill="Faixa Etária")  + ggsave(filename = "grafico3.5.png")

## Gráfico 3.6 - Total casos confirmados por Faixa etária. 

df %>%
  select(FAIXA_ETARIA,CLASSIFICACAO_CASO) %>%
  count(FAIXA_ETARIA) %>%
  ggplot(aes(x=FAIXA_ETARIA, y=n)) + 
  geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  labs(title = "Total Casos Confirmados por Faixa Etária",
                                                                                x= "Faixa Etária",                                                               
                                                                                y = "Casos Confirmados") + ggsave(filename = "grafico3.6.png")

###Conclusão dos gráficos 3.1, 3.2 e 3.3: Por faixa etária há diferença.


#4. Municípios no Norte e Nordeste possuem piores evoluções dos quadros clínicos em comparação com Centro?

## Grafico 4.1 - Contagem total por região faceting por Evoluçao
df %>%
  filter(MACRO %in% c('CENTRO', 'JEQUITINHONHA', 'NORDESTE','NOROESTE','NORTE')) %>%
  select(MACRO,EVOLUCAO,CLASSIFICACAO_CASO) %>%
  count(EVOLUCAO,MACRO) %>%
  ggplot(aes(x=MACRO, y=n)) + 
  geom_col() + facet_wrap(~EVOLUCAO, scales = "free_y") +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Números de Casos Confirmados por Região",
                                                               subtitle = "Segundo a evolução",
                                                               x = "Região",
                                                               y = "Casos Confirmados") + ggsave(filename = "grafico4.1.png")

# Grafico 4.2 - Contagem acumulado por Região faceting por Evolução 


df %>%
  filter(MACRO %in% c('CENTRO', 'JEQUITINHONHA', 'NORDESTE','NOROESTE','NORTE')) %>%
  count(DATA_NOTIFICACAO,MACRO,EVOLUCAO) %>%
  group_by(MACRO,EVOLUCAO) %>% 
  mutate(n_acumulado=cumsum(n)) %>%
  rename(confirmados=n_acumulado) %>%
  drop_na(DATA_NOTIFICACAO) %>%
  ggplot(aes(x=DATA_NOTIFICACAO, y=confirmados, color=EVOLUCAO)) + 
  geom_line() + facet_wrap(~MACRO, scales = "free_y") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Acumulado de Casos Confirmados por Região",
                                                                                                                                 subtitle = "Segundo a evolução",

                                                                                                                                                                                                                                                         x = "Data de notificação",
                                                                                                                                 y = "Casos Confirmados", fill="Região")  + ggsave(filename = "grafico4.2.png")
## Gráfico 4.3 - Total casos confirmados por REGIÃO. 

df %>%
  filter(MACRO %in% c('CENTRO', 'JEQUITINHONHA', 'NORDESTE','NOROESTE','NORTE')) %>%
  select(MACRO,CLASSIFICACAO_CASO) %>%
  count(MACRO) %>%
  ggplot(aes(x=MACRO, y=n)) + 
  geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  labs(title = "Total Casos Confirmados por Região",
                                                                                x= "Região",                                                               
                                                                                y = "Casos Confirmados") + ggsave(filename = "grafico4.3.png")


