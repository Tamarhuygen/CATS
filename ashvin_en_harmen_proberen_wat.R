library(ggplot2)
library(caret)
setwd("/Users/Harme/Dropbox/b4tm/CATS/")

# The function 'Validate' performs training using double cross validation to reliably determine the
# accuracy of a predictor trained using the provided data and corresponding classes.
#
# The function can be called with minimal parameters, data and corresponding classes
# You can also override the defaults (train method, nr inner/outer folds, etc)
#
# The function returns a list with results (accuracy, standard deviation)
# Please be aware that the function can take quite long to complete.  Be patient!
#


# read the samples and associated classes from file
train.call = read.delim("train_call.txt")
train.clinical = read.delim("train_clinical.txt")
validation.call = read.delim("validation_call.txt")

# Merge data and classes
merged = merge(train.clinical, t(train.call[,-1:-4]), by.x="Sample", by.y="row.names")
val = as.data.frame(t(validation.call[,-1:-4]))
her2_less <- merged[merged$Subgroup != 'HER2+',]
classes = factor(merged[merged$Subgroup != 'HER2+', 'Subgroup'])
results = Validate(her2_less[,3:2836], 
                   classes, 
                   outer.fold = 3,
                   inner.fold = 3,
                   method = "rf",
                   filter_method = c("rfe","knowledge"), 
                   knowledge_features = v_cols)


(1*0.32) +  (mean(c(results$MethodAccuracy$`1_knowledge`, results$MethodAccuracy$`2_knowledge`, results$MethodAccuracy$`3_knowledge`)) * 0.68)
(1*0.32) + (mean(c(results$MethodAccuracy$`1_rfe`, results$MethodAccuracy$`2_rfe`, results$MethodAccuracy$`3_rfe`)) * 0.68)

names((her2_less[,3:2836]))
merged[,predictors(lmProfile)]
results$AverageAccuracy
predicted_val <- predict(results$fit,newdata = val)

used_features <- colnames(results$fit$trainingData[-length(results$fit$trainingData)])

predicted_results <- predict(results$fit, newdata = val[,used_features])
cbind(rownames(val),as.character(predicted_results))

knowledge  <- c(2185, 877, 878, 879, 1842, 1578, 304, 1297, 1395, 360, 2201, 1647, 2636, 2368, 671, 2612,2613, 2433, 1415, 154, 2718, 2075, 1229, 1072, 774, 745, 760, 2554, 1646, 2202, 2320, 238, 302)
knowledge.2 <- c()
for(i in knowledge){
  knowledge.2 <- c(knowledge.2, paste("V",as.character(i),sep = ""))
}
blub <- her2_less[,3:2836]
blub[,knowledge.2]
