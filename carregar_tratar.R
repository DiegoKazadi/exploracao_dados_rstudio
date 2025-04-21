# Carregando pacotes
library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)

# Lendo o arquivo com separador ponto e vírgula e limpando os nomes das colunas
alunos <- read_delim("E:/Mestrado UFCG/Semestre 2024.2/Dados/Tabelas_0/alunos.csv", delim = ";") %>%
  clean_names()

# Ver nomes das colunas (após limpeza)
names(alunos)

# Visualizando as primeiras linhas
head(alunos)

# Estrutura da tabela
glimpse(alunos)

# Sumário estatístico
summary(alunos)

# Contagem por gênero
alunos %>% count(genero)

# Contagem por tipo de admissão
alunos %>% count(tipo_admissao)

# Situação dos alunos: ativos, ex-alunos e inativos
alunos %>% 
  summarise(
    ATIVOS = sum(alunos_ativos, na.rm = TRUE),
    EX_ALUNOS = sum(ex_alunos, na.rm = TRUE),
    INATIVOS = sum(alunos_inativos, na.rm = TRUE)
  )

# Anos de formatura do ensino médio
alunos %>% 
  count(ano_formatura_ensino_medio) %>% 
  arrange(desc(n))

# Percentual de dados faltantes por coluna
alunos %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "coluna", values_to = "percentual_na") %>%
  arrange(desc(percentual_na))

