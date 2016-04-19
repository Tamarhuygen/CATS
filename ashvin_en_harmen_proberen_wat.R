library(ggplot2)
library(caret)
setwd("/Users/Harme/Dropbox/b4tm/CATS/")
Distance = function(data)
{
  v = vector(mode="numeric", length=ncol(data))
  for (c in 4:ncol(data)) {
    v[c] = sum(abs(data[,c]-data[,c-1]))
  }
  return (v)
}

# Find a feature
FindRegion = function(train.call, chromosome, location)
{
  return(train.call[(train.call$Chromosome == chromosome) & (train.call$Start <= location) & (train.call$End >= location),1:5])
}
#Feature SELECTION
FeatureSelection = function(data)
{
  varImp(data, method="rfe")
}

fitControl = trainControl(
  method = "repeatedcv",
  number = 10,
  #            classProbs = TRUE,
  repeats = 3)

fit = train(merged[1:50,3:length(merged)], merged$Subgroup[1:50],
            method = "nb",
            trControl = fitControl,
            tuneLength = 10)
fit

acc <- sum(pt == merged$Subgroup[31:100]) / length(merged$Subgroup[31:100])
acc

pt <- predict.train(fit, merged[31:100,3:length(merged)])
predict.train(fit, testX=merged[31:100,3:length(merged)], testY = merged[31:100,2])
?predict.train
varImp(fit, useModel = T, scale=F)
?varImp
merged[,3:length(merged)]
#
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

results.knowledge = Validate(merged[,3:2836], merged$Subgroup, outer.fold = 3,inner.fold = 3,method = "pam")
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
