CrossvalidationScheme = function(featureData, 
                    subgroups, 
                    trainMethod = "pam", 
                    n_outerFolds = 3, 
                    outerRepeats= 1, 
                    innerFolds = 10, 
                    innerRepeats = 1, 
                    verbose = FALSE, 
                    filterMethods,
                    knowledgeFeatures)
{
  # Variables to store all our results in R style
  validate_accuracy <- vector('numeric', length = outerRepeats * n_outerFolds)
  method_accuracy <- list()
  train_accuracy <- vector('numeric', length = outerRepeats * n_outerFolds)
  results <- list()
  # Let's not forget setting the super 'random' seed.
  set.seed(1337)


  
  for (outerRepeat in sequence(outerRepeats)){  # for1
    # Creating outerFolds
    outerFolds = createFolds(subgroups, k = n_outerFolds)
    
    for (outerFold in sequence(n_outerFolds)){ # for2
      print(paste("STARTING WITH FOLD: ",outerFold))
      for(filterMethod in filterMethods){  # for3
      # Split data according to proper fold 
      validation_outerFold <- outerFolds[[outerFold]]
      train_featureData <- featureData[-validation_outerFold,]
      train_subgroups <- subgroups[-validation_outerFold]
      validation_featureData <- featureData[validation_outerFold,]
      validation_subgroups <- subgroups[validation_outerFold]      
      

      if(filterMethod =="rfe"){
      subsets <- seq(10,100,10)
      ctrl <- rfeControl(functions = rfFuncs,
                         method = "repeatedcv",
                         repeats = 1,
                         verbose = F)
      
      profile <- rfe(train_featureData, train_subgroups,
                       sizes = subsets,
                       rfeControl = ctrl)
      best_features <- predictors(profile)
      }
      if(filterMethod=="knowledge"){
        best_features <- knowledgeFeatures
      }
      
      
      # Calculate predicted accuracy  
      # Train the model with the train featureData and subgroups using
      # selected method
      fitControl <- trainControl(
        method = "repeatedcv",
        number = innerFolds,
        #            classProbs = TRUE,
        repeats = innerRepeats)
      train_featureData <- train_featureData[,best_features]
      
    
      fit <- train(train_featureData, train_subgroups,
                  method = trainMethod,
                  trControl = fitControl,
                  tuneLength = 10)

     
      print("##### FINISHED TRAINING #####")
      performance <- fit$results[row.names(fit$finalModel$tuneValue),]
      
      train_accuracy[outerFold] <- performance$Accuracy
      predictSubgroups <- predict(fit, newdata = validation_featureData[,best_features])
      cm <- confusionMatrix(predictSubgroups, validation_subgroups)
      if (verbose) print(cm)
      
      # predict_probs = predict(fit, newdata = validation_featureData, type = "prob")
      # if (verbose) print(predict_probs)
      
      validate_accuracy[outerFold] = cm$overall['Accuracy']
#       print('##################')
#       print(cm$overall['Accuracy'])
#       print(fit)
#       print('##################')
      if (outerFold == 1) results$fit = fit
      if (outerFold > 1) if (validate_accuracy[outerFold-1] < cm$overall['Accuracy'] ) results$fit <- fit
      
      print('################## RESULTS ##################')
      method_accuracy[paste(outerFold,filterMethod,sep = "_")] <- validate_accuracy[outerFold]
      print(paste(outerFold,filterMethod,validate_accuracy[outerFold],length(best_features)))
      
    }  # for3
    }  # for2
      }  # for1
  
  max(results$fit$results$Accuracy)
  
  print("calculating results")
  print(validate_accuracy)
  results$MethodAccuracy = method_accuracy
  results$AverageAccuracy = mean(validate_accuracy)
  results$StdDevAccuracy = sd(validate_accuracy)
  
  results$TrainAverageAccuracy = mean(train_accuracy)
  results$StdDevTrainAccuracy = sd(train_accuracy)
  
  return (results)
}

