library(readr)
csv_sistemas <- read_delim("http://coronavirus.saude.mg.gov.br/images/csv-microdados/csv_sistemas.csv",
";", escape_double = FALSE, col_types = cols(DATA_NOTIFICACAO = col_date(format = "%m/%d/%Y")),
trim_ws = TRUE)
View(csv_sistemas)
df<-csv_sistemas
View(df)
