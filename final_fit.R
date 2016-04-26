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

getTree(final.fit)
plot(getTree(final.fit$finalModel))
varImpPlot(final.fit$finalModel)
partialPlot(final.fit$finalModel, train_featureData, x.var=V2185)
?partialPlot

MDSplot(final.fit$finalModel, subgroups)

subgroups = factor(merged[merged$Subgroup != 'HER2+', 'Subgroup'])
all.fit <- train(comb[,v_cols], merged$Subgroup, method = "rf", trControl = fitControl,tuneLength = 10)

pdf("/Users/Harme/Dropbox/b4tm/CATS/variable_importance_dotchart_HER2+.pdf")
varImpPlot(final.fit$finalModel,
           main="Variable Importance without HER2+")
dev.off()
pdf("/Users/Harme/Dropbox/b4tm/CATS/variable_importance_dotchart.pdf")
varImpPlot(all.fit$finalModel,
           main="Variable Importance with HER2+")
dev.off()




?varImpPlot
important_vars <- rownames(varImp(all.fit$finalModel))
classes <- c("HER2+", "HR+", "Triple Neg")

pdf("/Users/Harme/Dropbox/b4tm/CATS/partial_dependences.pdf")
par(mfrow=c(1,2))
for (i in seq_along(important_vars)){
  for (j in seq_along(classes)){
  partialPlot(all.fit$finalModel, comb, x.var=important_vars[i], main = c("Partial Dependence on ", important_vars[i]),
              xlab = classes[j], which.class=classes[j])
}
}
dev.off()
partialPlot(all.fit$finalModel, train_featureData, x.var=V2185,
            main = "Variable Importance in RF without HER2+", which.class="Triple Neg")
partialPlot(all.fit$finalModel, train_featureData, x.var=V2185)
?partialPlot()
plot(getTree(final.fit$finalModel))
getTree(final.fit$finalModel)
comb[,2:ncol(comb)]
feature_sizes <- seq(10,100,5)
ctrl <- rfeControl(functions = rfFuncs,method = 'repeatedcv',repeats = 1,verbose = F)
profile <- rfe(comb[,3:ncol(comb)], merged$Subgroup,sizes = feature_sizes,rfeControl = ctrl)
best_features <- predictors(profile)
best_features2 <- predictors(profile)


all.fit.rfe <- train(comb[,best_features2], merged$Subgroup, method = "rf", trControl = fitControl,tuneLength = 10)
pdf("/Users/Harme/Dropbox/b4tm/CATS/variable_importance_dotchart_both.pdf")
par(mfrow=c(1,2))
varImpPlot(all.fit.rfe$finalModel, main="Variable Importance RFE")
varImpPlot(all.fit$finalModel,
           main="Variable Importance KB")
dev.off()


