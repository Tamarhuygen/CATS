#             fold 1      fold 2      fold 3
# 
# rfe :       0.8695652   0.7272727   0.6956522
# knowledge : 0.8695652   0.7727273   0.6086957


best_features <- v_cols
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
subgroups = factor(merged[merged$Subgroup != 'HER2+', 'Subgroup'])
train_featureData <- her2_less[,best_features]
final.fit <- train(train_featureData, subgroups, method = "rf", trControl = fitControl,tuneLength = 10)

val_call <- as.data.frame(t(validation.call[,-1:-4]))

final.fit
predictSubgroups <- predict(final.fit, newdata = val_call[,best_features])
predictSubgroups_df <- data.frame(Sample = rownames(val_call), Subgroup = predictSubgroups)

predictSubgroups_df[-c(as.character(her2_predictions$Sample)),]

her2_predictions <- read.table("predictions_group_5_first.txt", header =T)

subset(predictSubgroups_df, !(predictSubgroups_df$Sample %in% her2_predictions$Sample))


final_results <- rbind(subset(predictSubgroups_df, !(predictSubgroups_df$Sample %in% her2_predictions$Sample)), her2_predictions)

write.table(final_results,file = "final_predictions.txt",quote = TRUE,sep = "\t",row.names = F,col.names = T)

write.table()

