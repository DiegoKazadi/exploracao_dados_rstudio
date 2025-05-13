# =====================================================
# PRÉ-PROCESSAMENTO DA TABELA 'alunos.csv'
# =====================================================

# 1. Carregando pacotes
# Apagar o diretório de pacotes manualmente (riscos mínimos, mas cuidado se tiver muitos pacotes instalados)
unlink("/home/diego/R/x86_64-pc-linux-gnu-library/4.1", recursive = TRUE)

# Criar novamente a pasta de bibliotecas
dir.create("/home/diego/R/x86_64-pc-linux-gnu-library/4.1", recursive = TRUE)
install.packages(c("janitor", "lubridate", "dplyr", "tidyr", "stringr", "ggplot2"))
install.packages("janitor")
library(janitor)


# 2. Lendo a tabela e limpando os nomes das colunas
alunos <- readr::read_delim("/home/diego/Documentos/Semestre 2024.2/Dados/Tabelas_0/alunos.csv", delim = ";") %>%
  janitor::clean_names()

# 3. Removendo duplicatas
alunos <- alunos %>% distinct()

# 4. Criando coluna de ano de ingresso
alunos <- alunos %>%
  mutate(ano_ingresso = as.integer(str_sub(termo_admissao, 1, 4))) %>%
  filter(ano_ingresso >= 2000, ano_ingresso <= 2025)

# 5. Padronização de categorias textuais
alunos <- alunos %>%
  mutate(
    genero = str_to_lower(genero),
    tipo_admissao = str_to_lower(tipo_admissao),
    estado_civil_alunos = str_to_lower(estado_civil_alunos)
  )

# 6. Tratamento de valores inconsistentes
alunos <- alunos %>%
  mutate(
    genero = case_when(
      genero %in% c("f", "feminino") ~ "feminino",
      genero %in% c("m", "masculino") ~ "masculino",
      TRUE ~ "outro"
    )
  )

# 7. Conversões de tipos
alunos <- alunos %>%
  mutate(
    data_nascimento = as.Date(data_nascimento, format = "%Y-%m-%d"),
    ano_formatura_ensino_medio = as.integer(ano_formatura_ensino_medio)
  )

# 8. Verificando percentual de dados ausentes
faltantes <- alunos %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "coluna", values_to = "percentual_na") %>%
  arrange(desc(percentual_na))

# 9. Removendo colunas com mais de 80% de NA
colunas_validas <- faltantes %>% filter(percentual_na < 80) %>% pull(coluna)
alunos <- alunos %>% select(all_of(colunas_validas))

# 10. Criando colunas úteis
alunos <- alunos %>%
  mutate(
    tempo_desde_ingresso = 2025 - ano_ingresso,
    status_resumido = case_when(
      alunos_ativos == 1 ~ "Ativo",
      ex_alunos == 1 ~ "Concluído",
      alunos_inativos == 1 ~ "Inativo",
      TRUE ~ "Desconhecido"
    ),
    evadiu = ifelse(status_resumido == "Inativo", 1, 0)
  )

# 11. Salvando a tabela pré-processada
write_csv(alunos, "/home/diego/Documentos/Semestre 2024.2/Dados/Tabelas_0/alunos_preprocessado.csv")

# 12. Fim
message("Pré-processamento finalizado e arquivo salvo com sucesso!")
