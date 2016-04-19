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
subgroups = factor(merged[merged$Subgroup != 'HER2+', 'Subgroup'])
results = CrossvalidationScheme( her2_less[,3:2836], 
                                 subgroups = subgroups,
                                 n_outerFolds = 68,
                                 innerFolds = 10,
                                 trainMethod = "rf",
                                 filterMethods = c("rfe","knowledge"), 
                                 knowledgeFeatures = v_cols)


(1*0.32) +  (mean(c(results$MethodAccuracy$`1_knowledge`, results$MethodAccuracy$`2_knowledge`, results$MethodAccuracy$`3_knowledge`)) * 0.68)
(1*0.32) + (mean(c(results$MethodAccuracy$`1_rfe`, results$MethodAccuracy$`2_rfe`, results$MethodAccuracy$`3_rfe`)) * 0.68)
