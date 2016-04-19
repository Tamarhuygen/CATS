Validate = function(data, classes, method = "knn", outer.fold = 3, outer.repeats=1, inner.fold = 10, inner.repeats = 1, verbose = TRUE)
{
  cat(sprintf("Training and validation process using method '%s'", method))
  
  # Make sure we create the same 'random' sets every time we run (reproducable)
  set.seed(1)
  
  # Store predicted accuracy and actual accuracy for all validation runs
  # Return results including average accuracy and standard deviation
  results = list()
  
  results.train.accuracy = vector('numeric', length = outer.repeats * outer.fold)
  results.validate.accuracy = vector('numeric', length = outer.repeats * outer.fold)
  
  for (run in sequence(outer.repeats)){
    # Create separate folds for training and validation
    outer.folds = createFolds(classes, k = outer.fold)
    
    for (fold in sequence(outer.fold)){
      if (verbose) cat(sprintf("\nRun %d of %d, fold %d of %d\n", run, outer.repeats, fold, outer.fold))
      
      # Select the train and validation folds for this run/fold
      validation.fold = outer.folds[[fold]]
      train.data = data[-validation.fold,]
      train.classes = classes[-validation.fold]
      validation.data = data[validation.fold,]
      validation.classes = classes[validation.fold]      
      
#       subsets <- seq(10,100,10)
#       ctrl <- rfeControl(functions = rfFuncs,
#                          method = "repeatedcv",
#                          repeats = 1,
#                          verbose = F)
#       
#       profile <- rfe(train.data, train.classes,
#                        sizes = subsets,
#                        rfeControl = ctrl)
#       best_features <- predictors(profile)
      best_features <- knowledge.2
      # Calculate predicted accuracy
      
      
      
      
      
      # Train the model with the train data and classes using
      # selected method
      fitControl = trainControl(
        method = "repeatedcv",
        number = inner.fold,
        #            classProbs = TRUE,
        repeats = inner.repeats)
      train.data <- train.data[,best_features]
      fit = train(train.data, train.classes,
                  method = method,
                  trControl = fitControl,
                  tuneLength = 10)
      
     
      print("done training")
      performance = fit$results[row.names(fit$finalModel$tuneValue),]
      i = fold
      
      results.train.accuracy[i] = performance$Accuracy
      predict.classes = predict(fit, newdata = validation.data[,best_features])
      cm = confusionMatrix(predict.classes, validation.classes)
      if (verbose) print(cm)
      
      # predict.probs = predict(fit, newdata = validation.data, type = "prob")
      # if (verbose) print(predict.probs)
      
      results.validate.accuracy[i] = cm$overall['Accuracy']
      print('##################')
      print(cm$overall['Accuracy'])
      print(fit)
      print('##################')
      if (fold == 1) results$fit = fit
      if (fold > 1) if (max(fit$results$Accuracy) > max(results$fit$results$Accuracy) ) results$fit = fit

    }
  }
  
  max(results$fit$results$Accuracy)
  
  print("calculating results")
  print(results.validate.accuracy)
  results$AverageAccuracy = mean(results.validate.accuracy)
  results$StdDevAccuracy = sd(results.validate.accuracy)
  
  results$TrainAverageAccuracy = mean(results.train.accuracy)
  results$StdDevTrainAccuracy = sd(results.train.accuracy)
  
  return (results)
}
?createFolds
