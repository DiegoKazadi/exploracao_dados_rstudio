# Carregando pacotes
library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)
library(ggplot2)

# Lendo a tabela e limpando os nomes das colunas
alunos <- read_delim("E:/Mestrado UFCG/Semestre 2024.2/Dados/Tabelas_0/alunos.csv", delim = ";") %>%
  clean_names()

# Visualização rápida dos dados
names(alunos)
head(alunos)
glimpse(alunos)
summary(alunos)

# Análises básicas
alunos %>% count(genero)
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

# GRÁFICOS EXPLORATÓRIOS

# Gráfico de barras por gênero
ggplot(alunos, aes(x = genero)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribuição por Gênero", x = "Gênero", y = "Quantidade de Alunos") +
  theme_minimal()

# Gráfico por tipo de admissão
ggplot(alunos, aes(x = tipo_admissao)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Tipo de Admissão", x = "Tipo", y = "Número de Alunos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico por tipo de ensino médio
ggplot(alunos, aes(x = tipo_ensino_medio)) +
  geom_bar(fill = "purple") +
  labs(title = "Tipo de Ensino Médio", x = "Tipo", y = "Número de Alunos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico por ano de formatura do ensino médio
ggplot(alunos, aes(x = ano_formatura_ensino_medio)) +
  geom_bar(fill = "orange") +
  labs(title = "Ano de Formatura do Ensino Médio", x = "Ano", y = "Número de Alunos") +
  theme_minimal()

# Histograma da idade
ggplot(alunos, aes(x = idade)) +
  geom_histogram(binwidth = 1, fill = "darkred", color = "white") +
  labs(title = "Distribuição de Idade dos Alunos", x = "Idade", y = "Frequência") +
  theme_minimal()

