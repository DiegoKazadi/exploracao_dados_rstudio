# =====================================================
# MODELAGEM DE EVASÃO A PARTIR DA TABELA PRÉ-PROCESSADA
# =====================================================

# 1. Carregar pacotes necessários
pacotes <- c("readr", "dplyr", "caret", "randomForest", "class", "pROC", "ggplot2", "e1071")
novos <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(novos) > 0) install.packages(novos)
lapply(pacotes, library, character.only = TRUE)

# 2. Carregar dados pré-processados
alunos <- read_csv("/home/diego/Documentos/Semestre 2024.2/Dados/Tabelas_0/alunos_preprocessado.csv")

# 3. Limpeza de dados: remover colunas pessoais e tratar fatores
alunos <- alunos %>%
  select(-matricula, -nome, -e_mail, -local_nascimento, -id_cidadao) %>%
  mutate(across(where(is.character), as.factor)) %>%
  na.omit()

# Corrigir níveis dos fatores
fatores <- sapply(alunos, is.factor)
alunos[fatores] <- lapply(alunos[fatores], function(x) factor(x))

# 4. Separar dados em treino e teste
set.seed(123)
indice_treino <- createDataPartition(alunos$evadiu, p = 0.7, list = FALSE)
treino <- alunos[indice_treino, ]
teste  <- alunos[-indice_treino, ]

# 5. Remover colunas constantes
variancia_zero <- sapply(treino, function(x) length(unique(x)) == 1)
treino <- treino[, !variancia_zero]
teste  <- teste[, names(treino)]  # manter consistência de colunas

# 6. Garantir níveis dos fatores no teste compatíveis com o treino
for (col in names(treino)) {
  if (is.factor(treino[[col]])) {
    teste[[col]] <- factor(teste[[col]], levels = levels(treino[[col]]))
  }
}

# 7. Separar preditores e variável alvo
x_treino <- treino %>% select(-evadiu)
y_treino <- treino$evadiu

x_teste <- teste %>% select(-evadiu)
y_teste <- teste$evadiu

# 8. Regressão Logística
modelo_log <- glm(evadiu ~ ., data = treino, family = "binomial")
prob_log <- predict(modelo_log, newdata = teste, type = "response")
pred_log <- ifelse(prob_log > 0.5, 1, 0)

# 9. Random Forest
modelo_rf <- randomForest(as.factor(evadiu) ~ ., data = treino, ntree = 100)
pred_rf <- predict(modelo_rf, newdata = x_teste)

# 10. SVM
modelo_svm <- svm(as.factor(evadiu) ~ ., data = treino, probability = TRUE)
prob_svm <- predict(modelo_svm, newdata = x_teste, probability = TRUE)
attr_prob <- attr(prob_svm, "probabilities")[, "1"]
pred_svm <- ifelse(attr_prob > 0.5, 1, 0)

# 11. KNN - precisa de variáveis numéricas
dummies <- dummyVars(" ~ .", data = x_treino)
x_treino_knn <- predict(dummies, newdata = x_treino)
x_teste_knn  <- predict(dummies, newdata = x_teste)

# Converter para data.frame e remover NAs
x_treino_knn <- as.data.frame(x_treino_knn)
x_teste_knn  <- as.data.frame(x_teste_knn)
x_treino_knn <- na.omit(x_treino_knn)
x_teste_knn  <- na.omit(x_teste_knn)

# Ajustar y_treino para manter consistência com linhas válidas
y_treino_knn <- y_treino[as.numeric(rownames(x_treino_knn))]

# Rodar o modelo KNN
knn_pred <- knn(train = x_treino_knn, test = x_teste_knn, cl = y_treino_knn, k = 5)

# 12. Avaliação dos modelos
avaliar_modelo <- function(real, previsto, nome_modelo) {
  cm <- confusionMatrix(as.factor(previsto), as.factor(real), positive = "1")
  auc <- roc(real, as.numeric(previsto))$auc
  cat(paste0("\n### ", nome_modelo, " ###\n"))
  print(cm)
  cat(paste("AUC:", round(auc, 3), "\n"))
}

avaliar_modelo(y_teste, pred_log, "Regressão Logística")
avaliar_modelo(y_teste, pred_rf, "Random Forest")
avaliar_modelo(y_teste, pred_svm, "SVM (Support Vector Machine)")
avaliar_modelo(y_teste[as.numeric(rownames(x_teste_knn))], knn_pred, "KNN")

# 13. Fim
message("✅ Modelagem finalizada!")

# ================================

# Instalar e carregar o mlflow
installed.packages()[, "Package"] |> grep("mlflow", ., value = TRUE)
install.packages("remotes")  # se ainda não tiver
install.packages("reticulate")
remotes::install_github("mlflow/mlflow", subdir = "mlflow/R/mlflow")
remotes::install_github("mlflow/mlflow", subdir = "mlflow/R/mlflow", lib = "~/R/x86_64-pc-linux-gnu-library/4.3")
library(mlflow, lib.loc = "~/R/x86_64-pc-linux-gnu-library/4.3")
library(reticulate)
py_config()


# Iniciar experimento
mlflow_set_experiment("modelo_evasao_ufcg")
mlflow_start_run(run_name = "experimento_completo")

# Logar parâmetros, métricas e modelo
avaliar_modelo <- function(real, previsto, nome_modelo) {
  cm <- confusionMatrix(as.factor(previsto), as.factor(real), positive = "1")
  auc_val <- roc(real, as.numeric(previsto))$auc
  
  cat(paste0("\n### ", nome_modelo, " ###\n"))
  print(cm)
  cat(paste("AUC:", round(auc_val, 3), "\n"))
  
  # Log no MLflow
  mlflow_log_param("modelo", nome_modelo)
  mlflow_log_metric("AUC", round(auc_val, 3))
  mlflow_log_metric("Accuracy", cm$overall["Accuracy"])
  mlflow_log_metric("Sensitivity", cm$byClass["Sensitivity"])
  mlflow_log_metric("Specificity", cm$byClass["Specificity"])
}


# Finalizar o run
mlflow_end_run()

# Rodar o MLflow UI (no terminal, fora do R)


