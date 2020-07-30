library(readr); library (dplyr); library(ggplot2); library(tidyverse); library(utils)


ana_df <- read_delim("banco_de_dados/csv_sistemas.csv", ";", escape_double = FALSE, col_types = cols(DATA_NOTIFICACAO = col_date(format = "%d/%m/%Y")), trim_ws = TRUE)

#1. Qual o perfil da curva de crescimento (número de casos acumulados)na população com idade a partir de 60 anos?

idosos <- filter(ana_df, FAIXA_ETARIA == c( "60 A 69 ANOS", "70 A 79 ANOS", "80 A 89 ANOS","90 OU MAIS"))

idosos$URS <- NULL
idosos$MICRO <- NULL
idosos$MACRO <- NULL
idosos$MUNICIPIO_RESIDENCIA <- NULL
idosos$CODIGO <- NULL
idosos$COMORBIDADE <- NULL
idosos$EVOLUCAO <- NULL
idosos$INTERNACAO <- NULL
idosos$UTI <- NULL
idosos$RACA <- NULL
idosos$DATA_ATUALIZACAO <- NULL
idosos$ORIGEM_DA_INFORMACAO <- NULL

idosos$DATA_NOTIFICACAO <- as.Date(idosos$DATA_NOTIFICACAO, format="%d/%m/%Y")

contagem <- idosos %>%
  mutate(controle = case_when(ID > 0 ~ 1)) %>%
  transform(controle = as.numeric(controle))

agregar <- aggregate(x = contagem["controle"], FUN = sum, by = list(Group.date = contagem$DATA_NOTIFICACAO)) %>% 
  mutate(total = cumsum(agregar$controle))

agregar %>% 
  ggplot(aes(x= Group.date, y= total)) +
  geom_line()+
  labs(title= "Acumulado de casos confirmados para maiores de 60 anos",x= "Data", y = "Total acumulado de casos")

