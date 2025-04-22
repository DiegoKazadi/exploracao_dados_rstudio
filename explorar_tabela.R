# Carregando pacotes
library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)
library(ggplot2)

# Lendo a tabela e limpando os nomes das colunas
alunos <- read_delim("/home/diego/Documentos/Semestre 2024.2/Dados/Tabelas_0/alunos.csv", delim = ";") %>%
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

#####------------------
# Visualizar os alunos Ativos
library(lubridate)

# Verificando estrutura do campo termo_admissao
alunos %>%
  count(termo_admissao) %>%
  arrange(termo_admissao)

# Extraindo o ano do termo_admissao (ex: 2019.2 → 2019)
alunos <- alunos %>%
  mutate(ano_ingresso = as.integer(str_sub(termo_admissao, 1, 4)))

# Filtrando somente alunos ativos e agrupando por ano
ativos_por_ano <- alunos %>%
  filter(alunos_ativos == 1) %>%
  group_by(ano_ingresso) %>%
  summarise(qtd_ativos = n()) %>%
  filter(!is.na(ano_ingresso) & ano_ingresso <= 2024)

# Visualizando os dados
print(ativos_por_ano)

# Gráfico de linha dos estudantes ativos por ano
ggplot(ativos_por_ano, aes(x = ano_ingresso, y = qtd_ativos)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Evolução dos Estudantes Ativos por Ano de Ingresso",
    x = "Ano de Ingresso",
    y = "Quantidade de Alunos Ativos"
  ) +
  theme_minimal()

# Visualizar os Alunos Evandiram

# Extrair o ano do campo 'termo_estado' (onde pode estar a data da evasão)
alunos <- alunos %>%
  mutate(ano_evasao = as.integer(str_sub(termo_estado, 1, 4)))

# Filtrar quem evadiu (ex-alunos ou inativos)
evadidos_por_ano <- alunos %>%
  filter(ex_alunos == 1 | alunos_inativos == 1) %>%
  group_by(ano_evasao) %>%
  summarise(qtd_evadidos = n()) %>%
  filter(!is.na(ano_evasao) & ano_evasao <= 2024)

# Visualizar os dados de evasão por ano
print(evadidos_por_ano)

# Gráfico de linha de evasões ao longo dos anos
ggplot(evadidos_por_ano, aes(x = ano_evasao, y = qtd_evadidos)) +
  geom_line(color = "firebrick", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Evolução da Evasão Estudantil por Ano",
    x = "Ano de Evasão",
    y = "Quantidade de Alunos Evadidos"
  ) +
  theme_minimal()

# Visualizar lunos ativos e Evadidos
library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)
library(ggplot2)

# Carregando os dados
alunos <- read_delim("/home/diego/Documentos/Semestre 2024.2/Dados/Tabelas_0/alunos.csv", delim = ";") %>%
  clean_names()

# === 1. ATIVOS POR ANO DE INGRESSO ===
alunos <- alunos %>%
  mutate(ano_ingresso = as.integer(str_sub(termo_admissao, 1, 4)))

ativos_por_ano <- alunos %>%
  filter(alunos_ativos == 1) %>%
  group_by(ano_ingresso) %>%
  summarise(qtd = n()) %>%
  rename(ano = ano_ingresso) %>%
  mutate(tipo = "Ativos")

# === 2. EVADIDOS POR ANO DE SAÍDA ===
alunos <- alunos %>%
  mutate(ano_evasao = as.integer(str_sub(termo_estado, 1, 4)))

evadidos_por_ano <- alunos %>%
  filter(ex_alunos == 1 | alunos_inativos == 1) %>%
  group_by(ano_evasao) %>%
  summarise(qtd = n()) %>%
  rename(ano = ano_evasao) %>%
  mutate(tipo = "Evadidos")

# === 3. JUNÇÃO DOS DOIS ===
dados_comparativos <- bind_rows(ativos_por_ano, evadidos_por_ano) %>%
  filter(!is.na(ano) & ano <= 2024)

# === 4. GRÁFICO COM LEGENDA ===
ggplot(dados_comparativos, aes(x = ano, y = qtd, color = tipo)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Comparativo de Estudantes Ativos e Evadidos por Ano",
    x = "Ano",
    y = "Quantidade de Alunos",
    color = "Situação"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Ativos" = "steelblue", "Evadidos" = "firebrick"))

