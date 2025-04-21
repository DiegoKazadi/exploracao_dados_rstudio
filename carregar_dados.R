install.packages("dplyr")
install.packages("readr")
library(dplyr)
library(readr)

# Função para carregar uma tabela
carregar_tabela <- function(caminho) {
  tryCatch({
    df <- read_csv(caminho, show_col_types = FALSE)
    return(df)
  }, error = function(e) {
    message(paste("Erro ao carregar", caminho, ":", e))
    return(NULL)
  })
}

# Função para carregar todas as tabelas de um diretório
carregar_todas_tabelas <- function(diretorio) {
  tabelas <- list()
  if (!dir.exists(diretorio)) {
    message(paste("Diretório não encontrado:", diretorio))
    return(tabelas)
  }
  
  arquivos <- list.files(diretorio, pattern = "\\.csv$", full.names = TRUE)
  if (length(arquivos) == 0) {
    message(paste("Nenhum arquivo CSV encontrado no diretório:", diretorio))
  } else {
    for (arquivo in arquivos) {
      df <- carregar_tabela(arquivo)
      if (!is.null(df)) {
        tabelas[[basename(arquivo)]] <- df
      }
    }
  }
  return(tabelas)
}

# Diretórios das tabelas
diretorio_tabelas <- "C:/Users/Big Data/Documents/Mestrado UFCG/Semestre 2024.1/Dados/Diego"

# Carregar os dados de ambos os diretórios
tabelas <- carregar_todas_tabelas(diretorio_tabelas)

# Verificar se as tabelas foram carregadas
message("Tabelas carregadas do diretório Diego:")
cat(paste(names(tabelas), collapse = "\n"), "\n\n")



# Função para visualizar uma tabela
visualizar_tabela <- function(nome, df) {
  message(paste("Visualização da Tabela:", nome))
  message(paste("Número de Linhas:", nrow(df)))
  message(paste("Número de Colunas:", ncol(df)))
  message("Nomes das Colunas:")
  print(names(df))
  cat(rep("=", 80), "\n")
}

# Visualizar tabelas carregadas do primeiro diretório
message("\nVisualização das tabelas carregadas")
for (nome in names(tabelas)) {
  visualizar_tabela(nome, tabelas[[nome]])
}

