# =====================================================
# Instalação 
# =====================================================
# install.packages("ggplot2")  # Descomente esta linha se ainda não tiver o ggplot2 instalado

# =====================================================
# Carregando pacotes
# =====================================================
library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)
library(ggplot2)

# =====================================================
# Carregando os dados e limpando nomes das colunas
# =====================================================
alunos <- read_delim("E:/Mestrado UFCG/Semestre 2024.2/Dados/Tabelas_0/alunos.csv", delim = ";") %>%
  clean_names()

# =====================================================
# Explorando os dados
# =====================================================

# Verificando os nomes das colunas após limpeza
names(alunos)

# Visualizando as primeiras linhas
head(alunos)

# Estrutura geral da tabela
glimpse(alunos)

# Estatísticas descritivas básicas
summary(alunos)

# =====================================================
# Análises simples + Visualizações
# =====================================================

# ---- Contagem por gênero ----
alunos %>%
  count(genero) %>%
  ggplot(aes(x = genero, y = n, fill = genero)) +
  geom_col() +
  labs(title = "Distribuição por Gênero",
       x = "Gênero",
       y = "Número de Alunos") +
  theme_minimal()

# ---- Contagem por tipo de admissão ----
alunos %>%
  count(tipo_admissao) %>%
  ggplot(aes(x = reorder(tipo_admissao, -n), y = n, fill = tipo_admissao)) +
  geom_col() +
  labs(title = "Distribuição por Tipo de Admissão",
       x = "Tipo de Admissão",
       y = "Número de Alunos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- Situação dos alunos: ativos, ex-alunos e inativos ----
alunos %>%
  summarise(
    ATIVOS = sum(alunos_ativos, na.rm = TRUE),
    EX_ALUNOS = sum(ex_alunos, na.rm = TRUE),
    INATIVOS = sum(alunos_inativos, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "situacao", values_to = "quantidade") %>%
  ggplot(aes(x = situacao, y = quantidade, fill = situacao)) +
  geom_col() +
  labs(title = "Situação dos Alunos",
       x = "Situação",
       y = "Quantidade") +
  theme_minimal()

# ---- Anos de formatura do ensino médio ----
alunos %>%
  count(ano_formatura_ensino_medio) %>%
  filter(!is.na(ano_formatura_ensino_medio)) %>%
  ggplot(aes(x = ano_formatura_ensino_medio, y = n)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Formatura do Ensino Médio por Ano",
       x = "Ano de Formatura",
       y = "Número de Alunos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- Percentual de dados faltantes por coluna ----
alunos %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "coluna", values_to = "percentual_na") %>%
  arrange(desc(percentual_na)) %>%
  ggplot(aes(x = reorder(coluna, percentual_na), y = percentual_na)) +
  geom_col(fill = "tomato") +
  labs(title = "Percentual de Dados Faltantes por Coluna",
       x = "Coluna",
       y = "Percentual de NAs (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- Extraindo e visualizando o ano de ingresso

# Contagem por ano (extraído de termo_admissao)
alunos %>%
  mutate(ano_ingresso = str_sub(termo_admissao, 1, 4)) %>%
  count(ano_ingresso) %>%
  arrange(ano_ingresso)

# Gráfico: Distribuição por ano de ingresso
alunos %>%
  mutate(ano_ingresso = str_sub(termo_admissao, 1, 4)) %>%
  count(ano_ingresso) %>%
  ggplot(aes(x = ano_ingresso, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Distribuição de Alunos por Ano de Ingresso",
       x = "Ano de Ingresso",
       y = "Número de Alunos") +
  theme_minimal()

# =====================================================
# Gráficos de Situação dos Alunos
# =====================================================

# Preparar os dados agregados
situacao_alunos <- alunos %>%
  summarise(
    Ativos = sum(alunos_ativos, na.rm = TRUE),
    Graduados = sum(ex_alunos, na.rm = TRUE),
    Evasao = sum(alunos_inativos, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "situacao", values_to = "quantidade")

# Calcular porcentagens
situacao_alunos <- situacao_alunos %>%
  mutate(
    percentual = round(quantidade / sum(quantidade) * 100, 1),
    label = paste0(percentual, "%")
  )

# Gráfico de Pizza com porcentagens
ggplot(situacao_alunos, aes(x = "", y = quantidade, fill = situacao)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  # Borda branca entre fatias
  coord_polar("y", start = 0) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 4.5) +
  labs(title = "Distribuição de Situação dos Alunos",
       fill = "Situação") +  # Título da legenda
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


# ---- Gráfico de Barras ----
ggplot(situacao_alunos, aes(x = situacao, y = quantidade, fill = situacao)) +
  geom_col() +
  labs(title = "Situação dos Alunos",
       x = "Situação",
       y = "Quantidade") +
  theme_minimal()

# ---- Gráfico de Curva (Linha) ----
# Para fins de exemplo, vamos supor que a evolução ao longo dos anos seja baseada em termo_admissao
# Extraímos o ano de ingresso para acompanhar ao longo do tempo

alunos %>%
  mutate(ano_ingresso = str_sub(termo_admissao, 1, 4)) %>%
  filter(!is.na(ano_ingresso)) %>%
  group_by(ano_ingresso) %>%
  summarise(
    Ativos = sum(alunos_ativos, na.rm = TRUE),
    Graduados = sum(ex_alunos, na.rm = TRUE),
    Evasao = sum(alunos_inativos, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -ano_ingresso, names_to = "situacao", values_to = "quantidade") %>%
  ggplot(aes(x = as.integer(ano_ingresso), y = quantidade, color = situacao)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Evolução de Situação dos Alunos ao Longo dos Anos",
       x = "Ano de Ingresso",
       y = "Quantidade de Alunos",
       color = "Situação") +
  theme_minimal()









# =====================================================
# Fim do script
# =====================================================
