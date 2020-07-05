# Trabalho em grupo (35 pts)

> Criação de um relatório dinâmico contendo análise exploratória de uma base de dados

A entrega do trabalho são os commits registrados no repositório `hw99-analise-viz-dados-grupo-<id>` na organização `analise-viz-dados-1sem-2020`.

O código fonte do relatório dinâmico deve utilizar o Rmarkdown (`.Rmd`), mas qualquer output (eg. `github_document`, `html_document`, etc) é válido. 

A pontuação será distribuída com base nas seguintes etapas de um [fluxo de análise de dados](https://analise-viz-dados-master.github.io/slides/02_analise-dados#2):

* Escolha e importação (10 pts)

    Nessa etapa o grupo deverá escolher uma base de dados de interesse, que esteja publicamente disponível, [articular perguntas exploratórias acerca dos dados](https://youtu.be/EakK9SO1ySQ?t=910), e importá-los no R. Exemplos dos pontos que serão avaliados:
    
    - As perguntas foram escritas de forma explícita?
    - Os dados foram importados de forma correta (eg. codificação, tipos das colunas, etc)?

* Arrumação e Exploração (15 pts)
    
    Nessa etapa deve ser realizada a manipulação e limpeza dos dados de acordo com as visualizações pretendidas. Exemplos dos pontos que serão avaliados:

    - Os dados foram limpos (eg. grafia de palavras)? 
    - Valores ausentes foram investigados? 
    - Foram apresentadas visualizações pertinentes as perguntas articuladas?

* Comunicação (10 pts)
    
    Nessa etapa os alunos devem comunicar os principais aspectos da análise. Exemplos dos pontos que serão avaliados:

    - O texto apresenta coesão? 
    - As principais interpretações foram feitas? 
    - As limitações da análise foram apontadas?
    - As dificuldades encontradas foram registradas?

Um sumário sugestivo para o relatório é

* Introdução (Contexto, objetivo e perguntas)
* Descrição da base de dados
* Análise Exploratória
* Limitações e dificuldades operacionais

## Datas e entregas preliminares

- 19/07 - Escolha e importação.

- 02/08 - Entrega final.

## Fontes de dados

Esse trabalho será mais proveitoso caso sejam escolhidas bases de dados que seja de conhecimento e interesse prévio dos alunos. No entanto, segue abaixo algumas fontes de inspiração:

* Portais de Dados Abertos
    - União (eg. <http://dados.gov.br/>)
    - Estados (eg. <http://www.transparencia.dadosabertos.mg.gov.br/>)
    - Municípios (eg. <https://dados.pbh.gov.br/>)

* [#TidyTuesday](https://github.com/rfordatascience/tidytuesday/blob/master/README.md)

    > A weekly data project aimed at the R ecosystem. As this project was borne out of the R4DS Online Learning Community and the R for Data Science textbook, an emphasis was placed on understanding how to summarize and arrange data to make meaningful charts with ggplot2, tidyr, dplyr, and other tools in the tidyverse ecosystem. However, any code-based methodology is welcome - just please remember to share the code used to generate the results.
