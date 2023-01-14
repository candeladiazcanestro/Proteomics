#####This script is used for performing machine learning (random forest)

args = commandArgs(trailingOnly = TRUE)

library(caret)
library(MLeval)
library(imbalance)
library(Metrics)
library(caret)
library(pROC)
library(plotROC)

baselin_OLINK <- read.csv(args[1], row.names = 1)
grouping_information <- read.csv(args[2], row.names = 1)

baselin_OLINK <- merge(baselin_OLINK, grouping_information, by = 0, all = F)
row.names(baselin_OLINK) <- baselin_OLINK$Row.names
baselin_OLINK$Row.names <- NULL
baselin_OLINK$Responder <- factor(baselin_OLINK$Responder)

OLINK_discovery <- subset(baselin_OLINK, Cohort == "Testing")
OLINK_discovery$Cohort <- NULL
OLINK_validation <- subset(baselin_OLINK, Cohort == "Validation")
OLINK_validation$Cohort <- NULL

p_all <- apply(OLINK_discovery[1:(length(OLINK_discovery)-1)], 2, function(x) wilcox.test(x ~ OLINK_discovery$Responder))
p_all <- as.data.frame(lapply(p_all, function(x) x$p.value))
p_all <- as.data.frame(t(p_all))
colnames(p_all) <- "p_origin"
p_all <- subset(p_all, p_origin < 0.1)

OLINK_training <- OLINK_discovery

OLINK_training$Responder <- factor(OLINK_training$Responder)
OLINK_validation$Responder <- factor(OLINK_validation$Responder)

OLINK_training <- OLINK_training[which(colnames(OLINK_training) %in% c(row.names(p_all),"Responder"))]
OLINK_validation <- OLINK_validation[which(colnames(OLINK_validation) %in% c(row.names(p_all),"Responder"))]

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           classProbs = TRUE,
                           savePredictions = TRUE, 
                           summaryFunction = twoClassSummary,
                           sampling = "rose")

ranger_result <- train(Responder ~ ., data = OLINK_training,
                  method = "ranger",
                  importance = "permutation",
                  trControl = fitControl)

saveRDS(ranger_result, "Model_Random_Forest.rds")

evalm_result <- evalm(ranger_result)

pred_result <- predict(ranger_result, newdata = OLINK_validation, type = "prob")
pred_result = data.frame(pred_result, OLINK_validation$Responder)
pred_result$obs <- pred_result$OLINK_validation.Responder
pred_result$pred <- ifelse(pred_result$no>pred_result$si,"no","si")
evalm_pred_result <- evalm(pred_result)

selectedIndices <- ranger_result$pred$mtry == 2 & ranger_result$pred$splitrule == "extratrees"

discovery_AUC <- ggplot(ranger_result$pred[selectedIndices, ], aes(m=si, d=factor(obs, levels = c("no", "si")))) + 
  geom_roc(n.cuts=0, color = "blue") + 
  coord_equal() +
  theme_bw()+
  #style_roc()+
  labs(x = "1 - Specificity", y = "Sensitivity")+
  geom_abline(intercept = 0, slope = 1,color = "red", linetype = "dashed")
ggsave("discovery_AUC.pdf", discovery_AUC )

validation_AUC <- ggplot(pred_result, aes(m=si, d=factor(OLINK_validation.Responder, levels = c("no", "si")))) + 
  geom_roc(n.cuts=0, color = "blue")  +
  theme_bw()+
  labs(x = "1 - Specificity", y = "Sensitivity")+
  geom_abline(intercept = 0, slope = 1,color = "red",  linetype = "dashed")
ggsave("validation_AUC.pdf", validation_AUC)

rangerImp <- varImp(ranger_result)
OLINK_imp <- rangerImp$importance

p_var <- ggplot(OLINK_imp, aes(x=reorder(rownames(OLINK_imp),Overall), y=Overall)) +
  geom_point( color="black")+#, size=4, alpha=0.6)+
  geom_segment( aes(x=rownames(OLINK_imp), xend=rownames(OLINK_imp), y=0, yend=Overall), 
                color='black') +
  xlab('')+
  ylab('Importance')+
  theme_light() +
  coord_flip()+
  geom_abline(intercept = 0, slope = 0)
p_var 
ggsave("feature_importance.pdf", p_var )
