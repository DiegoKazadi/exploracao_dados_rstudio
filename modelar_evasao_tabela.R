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

# 3. Preparação dos dados
alunos <- alunos %>% 
  select(-matricula, -nome, -e_mail, -local_nascimento, -id_cidadao) %>%  # Remove identificadores pessoais
  mutate_if(is.character, as.factor) %>%
  na.omit()  # Remove linhas com NA

# 4. Separar preditores e alvo
set.seed(123)
indice_treino <- createDataPartition(alunos$evadiu, p = 0.7, list = FALSE)
treino <- alunos[indice_treino, ]
teste  <- alunos[-indice_treino, ]

x_treino <- treino %>% select(-evadiu)
y_treino <- treino$evadiu

x_teste <- teste %>% select(-evadiu)
y_teste <- teste$evadiu

# 5. Regressão Logística
modelo_log <- glm(evadiu ~ ., data = treino, family = "binomial")
prob_log <- predict(modelo_log, newdata = x_teste, type = "response")
pred_log <- ifelse(prob_log > 0.5, 1, 0)

# 6. Random Forest
modelo_rf <- randomForest(as.factor(evadiu) ~ ., data = treino, ntree = 100)
pred_rf <- predict(modelo_rf, newdata = x_teste)

# 7. SVM (Support Vector Machine)
modelo_svm <- svm(as.factor(evadiu) ~ ., data = treino, probability = TRUE)
prob_svm <- predict(modelo_svm, newdata = x_teste, probability = TRUE)
attr_prob <- attr(prob_svm, "probabilities")[, "1"]
pred_svm <- ifelse(attr_prob > 0.5, 1, 0)

# 8. KNN
knn_pred <- knn(train = x_treino, test = x_teste, cl = y_treino, k = 5)

# 9. Avaliação dos modelos
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
avaliar_modelo(y_teste, knn_pred, "KNN")

# 10. Fim
message("✅ Modelagem finalizada!")
