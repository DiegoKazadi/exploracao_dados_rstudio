# =====================================================
# PRÉ-PROCESSAMENTO E VISUALIZAÇÃO INICIAL
# =====================================================

# Carregando pacotes necessários
library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)
library(ggplot2)

# =====================================================
# Leitura do arquivo e limpeza dos nomes das colunas
# =====================================================
alunos <- read_delim("E:/Mestrado UFCG/Semestre 2024.2/Dados/Tabelas_0/alunos.csv", delim = ";") %>%
  clean_names()

# =====================================================
# Visualização e inspeção inicial
# =====================================================
glimpse(alunos)      # Estrutura dos dados
head(alunos)         # Primeiras linhas
summary(alunos)      # Estatísticas descritivas
names(alunos)        # Nomes das colunas

# =====================================================
# Remoção de registros duplicados
# =====================================================
alunos <- alunos %>% distinct()

# =====================================================
# Percentual de valores ausentes por coluna
# =====================================================
faltantes <- alunos %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "coluna", values_to = "percentual_na") %>%
  arrange(desc(percentual_na))

# Gráfico: Porcentagem de dados faltantes por coluna
ggplot(faltantes, aes(x = reorder(coluna, percentual_na), y = percentual_na)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(title = "Porcentagem de Dados Faltantes por Coluna",
       x = "Coluna", y = "% de NA") +
  theme_minimal()

# =====================================================
# Criação da coluna de ano de ingresso (a partir de termo_admissao)
# =====================================================
alunos <- alunos %>%
  mutate(ano_ingresso = as.integer(str_sub(termo_admissao, 1, 4)))

# Filtrar anos plausíveis
alunos <- alunos %>%
  filter(ano_ingresso >= 2000, ano_ingresso <= 2025)

# =====================================================
# Gráfico: Distribuição de alunos por ano de ingresso
# =====================================================
alunos %>%
  count(ano_ingresso) %>%
  ggplot(aes(x = ano_ingresso, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Distribuição de Alunos por Ano de Ingresso",
       x = "Ano", y = "Número de Alunos") +
  theme_minimal()

# =====================================================
# Padronização de categorias textuais
# =====================================================
alunos <- alunos %>%
  mutate(
    genero = str_to_lower(genero),
    tipo_admissao = str_to_lower(tipo_admissao),
    estado_civil_alunos = str_to_lower(estado_civil_alunos)
  )

# =====================================================
# Gráfico: Gênero dos alunos
# =====================================================
alunos %>%
  count(genero) %>%
  ggplot(aes(x = genero, y = n, fill = genero)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Distribuição por Gênero", x = "Gênero", y = "Quantidade") +
  theme_minimal()

# =====================================================
# Gráfico: Tipo de Admissão
# =====================================================
alunos %>%
  count(tipo_admissao) %>%
  ggplot(aes(x = reorder(tipo_admissao, n), y = n, fill = tipo_admissao)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Distribuição por Tipo de Admissão",
       x = "Tipo de Admissão", y = "Quantidade") +
  theme_minimal()


# =====================================================
#  Verificar e tratar tipos de dados
# =====================================================
alunos <- alunos %>%
  mutate(
    data_nascimento = as.Date(data_nascimento, format = "%Y-%m-%d"),
    ano_formatura_ensino_medio = as.integer(ano_formatura_ensino_medio)
  )


# =====================================================
#  Tratar valores inconsistentes ou "sujos"
# =====================================================
alunos <- alunos %>%
  mutate(
    genero = case_when(
      genero %in% c("f", "feminino") ~ "feminino",
      genero %in% c("m", "masculino") ~ "masculino",
      TRUE ~ "outro"
    )
  )

# =====================================================
#   Identificar e tratar outliers
# =====================================================
alunos <- alunos %>%
  mutate(idade = as.integer(format(Sys.Date(), "%Y")) - as.integer(format(data_nascimento, "%Y"))) %>%
  filter(idade >= 15 & idade <= 100)

# =====================================================
#   Preencher ou excluir valores ausentes
# =====================================================
# Exemplo: remover colunas com mais de 80% de NA
limite_na <- 80
colunas_validas <- faltantes %>% filter(percentual_na < limite_na) %>% pull(coluna)
alunos <- alunos %>% select(all_of(colunas_validas))


# =====================================================
#   Preencher ou excluir valores ausentes
# =====================================================
alunos <- alunos %>%
  mutate(
    tempo_desde_ingresso = 2025 - ano_ingresso,
    status_resumido = case_when(
      alunos_ativos == 1 ~ "Ativo",
      ex_alunos == 1 ~ "Concluído",
      alunos_inativos == 1 ~ "Inativo",
      TRUE ~ "Desconhecido"
    )
  )



# =====================================================
# Fim do pré-processamento
# =====================================================
