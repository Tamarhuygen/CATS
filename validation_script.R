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
Validate = function(data, classes, method = "pam", outer.fold = 3, outer.repeats=1, inner.fold = 10, inner.repeats = 1, verbose = TRUE)
{
  cat(sprintf("Training and validation process using method '%s'", method))
  
  # Make sure we create the same 'random' sets every time we run (reproducable)
  set.seed(1)
  
  # Store predicted accuracy and actual accuracy for all validation runs
  # Return results including average accuracy and standard deviation
  results = list()
  
  results.train.accuracy = vector('numeric', length = outer.repeats * outer.fold)
  results.validate.accuracy = vector('numeric', length = outer.repeats * outer.fold)
  
  for (run in sequence(outer.repeats))
  {
    # Create separate folds for training and validation
    outer.folds = createFolds(classes, k = outer.fold)
    
    for (fold in sequence(outer.fold))
    {
      if (verbose) cat(sprintf("\nRun %d of %d, fold %d of %d\n", run, outer.repeats, fold, outer.fold))
      
      # Select the train and validation folds for this run/fold
      validation.fold = outer.folds[[fold]]
      train.data = data[-validation.fold,]
      train.classes = classes[-validation.fold]
      validation.data = data[validation.fold,]
      validation.classes = classes[validation.fold]      
      
      # Train the model with the train data and classes using
      # selected method
      fitControl = trainControl(
        method = "repeatedcv",
        number = inner.fold,
        #            classProbs = TRUE,
        repeats = inner.repeats)
      
      fit = train(train.data, train.classes,
                  method = method,
                  trControl = fitControl,
                  tuneLength = 10)
      
      # Calculate predicted accuracy
      performance = fit$results[row.names(fit$finalModel$tuneValue),]
      i = (run - 1) * outer.fold + fold
      results.train.accuracy[i] = performance$Accuracy
      predict.classes = predict(fit, newdata = validation.data)
      cm = confusionMatrix(predict.classes, validation.classes)
      if (verbose) print(cm)
      # predict.probs = predict(fit, newdata = validation.data, type = "prob")
      # if (verbose) print(predict.probs)
      
      results.validate.accuracy[i] = cm$overall['Accuracy'] 
      if (i == 1) results$fit = fit
    }
  }
  
  
  results$AverageAccuracy = mean(results.validate.accuracy)
  results$StdDevAccuracy = sd(results.validate.accuracy)
  
  results$TrainAverageAccuracy = mean(results.train.accuracy)
  results$StdDevTrainAccuracy = sd(results.train.accuracy)
  
  return (results)
}



ValidateAll = function(merged, val)
{
  data = merged[,-1:-2]
  classes = merged$Subgroup  
  Validate(data, classes, outer.fold = 10)
}

ValidateWithoutSmallDistance = function(merged, val)
{
  data = merged[merged$Subgroup != 'HER2+',Distance(merged)>=4]
  validation.data = val[,which(Distance(merged)>=4)-2]
  
  cat(dim(data))
  classes = factor(merged[merged$Subgroup != 'HER2+', 'Subgroup'])
  results = Validate(data, classes, outer.fold = nrow(data))
  
  # Predict train samples, excluding HER2+
  results$predict.training.classes = predict(results$fit, newdata = data)
  levels(results$predict.training.classes) <- c(levels(results$predict.training.classes), "HER2+")
  
  # Predict validation samples
  results$predict.classes = predict(results$fit, newdata = validation.data)
  levels(results$predict.classes) <- c(levels(results$predict.classes), "HER2+")
  results$predict.classes[which(val[,2185] == 2)] = "HER2+"
  names(results$predict.classes) = rownames(val)
  
  print(results)
  return(results)
}

ValidateWithoutHER2 = function(merged, val)
{
  data = merged[merged$Subgroup != 'HER2+',-1:-2]
  
  classes = factor(merged[merged$Subgroup != 'HER2+', 'Subgroup'])
  results = Validate(data, classes, outer.fold = nrow(data))
  
  # Predict train samples, excluding HER2+
  results$predict.training.classes = predict(results$fit, newdata = data)
  levels(results$predict.training.classes) <- c(levels(results$predict.training.classes), "HER2+")
  
  # Predict validation samples
  results$predict.classes = predict(results$fit, newdata = val)
  levels(results$predict.classes) <- c(levels(results$predict.classes), "HER2+")
  results$predict.classes[which(val[,2185] == 2)] = "HER2+"
  names(results$predict.classes) = rownames(val)
  
  print(results)
  return(results)
}

ValidateGeneRegions = function(merged, val)
{
  # Genes: 
  # 2185, 877, 878, 879, 1842, 1578  
  data = merged[merged$Subgroup != 'HER2+',c('V2185', 'V877', 'V878', 'V879', 'V1842', 'V1578')]
  validation.data = val[,c(2185, 877, 878, 879, 1842, 1578)]
  
  # Genes: Chromosomes 6,11,14
  # data = merged[merged$Subgroup != 'HER2+',c(716:897,1405:1593,1772:1878)]
  classes = factor(merged[merged$Subgroup != 'HER2+', 'Subgroup'])
  results = Validate(data, classes, outer.fold = nrow(data))
  
  # Predict train samples, excluding HER2+
  results$predict.training.classes = predict(results$fit, newdata = data)
  levels(results$predict.training.classes) <- c(levels(results$predict.training.classes), "HER2+")
  
  # Predict validation samples
  results$predict.classes = predict(results$fit, newdata = validation.data)
  levels(results$predict.classes) <- c(levels(results$predict.classes), "HER2+")
  results$predict.classes[which(val[,2185] == 2)] = "HER2+"
  names(results$predict.classes) = rownames(val)
  
  print(results)
  return(results)
}


ValidateExtendedGeneRegions = function(merged, val)
{
  # Genes: 
  # 2185, 877, 878, 879, 1842, 1578, 304, 1297, 1395, 360, 2201, 1647, 2636, 2368, 671, 2612,2613, 
  # 2433, 1415, 154, 2718, 2075, 1229, 1072, 774, 745, 760, 2554, 1646, 2202 2320, 238, 302
  
  data = merged[merged$Subgroup != 'HER2+',c('V2185', 'V877', 'V878', 'V879', 'V1842', 'V1578', 'V304', 'V1297', 'V1395', 'V360', 'V2201', 'V1647', 'V2636', 'V2368', 'V671', 'V2612','V2613', 'V2433', 'V1415', 'V154', 'V2718', 'V2075', 'V1229', 'V1072', 'V774', 'V745', 'V760', 'V2554', 'V1646', 'V2202', 'V2320', 'V238', 'V302')]
  validation.data = val[,c(2185, 877, 878, 879, 1842, 1578, 304, 1297, 1395, 360, 2201, 1647, 2636, 2368, 671, 2612,2613, 2433, 1415, 154, 2718, 2075, 1229, 1072, 774, 745, 760, 2554, 1646, 2202, 2320, 238, 302)]
  
  classes = factor(merged[merged$Subgroup != 'HER2+', 'Subgroup'])
  results = Validate(data, classes, outer.fold = nrow(data))
  
  # Predict train samples, excluding HER2+
  results$predict.training.classes = predict(results$fit, newdata = data)
  levels(results$predict.training.classes) <- c(levels(results$predict.training.classes), "HER2+")
  
  # Predict validation samples
  results$predict.validation.classes = predict(results$fit, newdata = validation.data)
  levels(results$predict.validation.classes) <- c(levels(results$predict.validation.classes), "HER2+")
  results$predict.validation.classes[which(val[,2185] == 2)] = "HER2+"
  names(results$predict.validation.classes) = rownames(val)
  
  print(results)
  return(results)
}


PermutationTest = function(merged, val)
{
  # Use random classes, should generate 0.5 as accuracy.  And it does!!! :)
  data = merged[merged$Subgroup != 'HER2+',c('V2185', 'V877', 'V878', 'V879', 'V1842', 'V1578', 'V304', 'V1297', 'V1395', 'V360', 'V2201', 'V1647', 'V2636', 'V2368', 'V671', 'V2612','V2613', 'V2433', 'V1415', 'V154', 'V2718', 'V2075', 'V1229', 'V1072', 'V774', 'V745', 'V760', 'V2554', 'V1646', 'V2202', 'V2320', 'V238', 'V302')]
  validation.data = val[,c(2185, 877, 878, 879, 1842, 1578, 304, 1297, 1395, 360, 2201, 1647, 2636, 2368, 671, 2612,2613, 2433, 1415, 154, 2718, 2075, 1229, 1072, 774, 745, 760, 2554, 1646, 2202, 2320, 238, 302)]
  
  classes = factor(merged[merged$Subgroup != 'HER2+', 'Subgroup'])
  for (permutation in 1:100)
  {
    classes = sample(classes)
  }
  
  
  nr.permutations = 200
  permutations.accuracy = numeric(nr.permutations)
  
  for (permutation in 1:nr.permutations)
  {
    classes = sample(classes)
    results = Validate(data, classes, outer.fold = nrow(data))
    permutations.accuracy[permutation] = results$AverageAccuracy
  }
  
  print(permutations.accuracy)
  return(permutations.accuracy)
}

plotHER2plus = function(merged, val)
{
  roc = filterVarImp(merged[,-1:-2],merged$Subgroup)
  # ord = roc[order(roc$HER2, decreasing=TRUE),1]
  qplot(data=roc, x=HER2, binwidth=0.025, fill=I("blue"), alpha=I(.2), col=I("blue"))
  print(mean(roc$HER2))
  print(sd(roc$HER2))  
}



# read the samples and associated classes from file
train.call = read.delim("train_call.txt")
train.clinical = read.delim("train_clinical.txt")
validation.call = read.delim("validation_call.txt")

# Rename classes such that they can be used as variable names
# levels(train.clinical$Subgroup) = c('HER2','HR','Triple.Neg')


# Merge data and classes
merged = merge(train.clinical, t(train.call[,-1:-4]), by.x="Sample", by.y="row.names")
val = t(validation.call[,-1:-4])

results = ValidateWithoutSmallDistance(merged, val)
df = data.frame(results$predict.classes)
colnames(df) = c("Subgroup")
write.table(df, file="predictions.txt", sep="\t")

res2 = PermutationTest(merged, val)
