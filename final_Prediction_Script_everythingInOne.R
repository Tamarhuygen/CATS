library(caret)
library(ggplot2)
setwd("/Users/Harme/Dropbox/b4tm/CATS/")
CrossvalidationScheme = function(featureData, subgroups, trainMethod = "pam", n_outerFolds = 3,outerRepeats= 1, innerFolds = 10, innerRepeats = 1, verbose = FALSE, filterMethods,knowledgeFeatures){
  
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
          feature_sizes <- seq(10,100,10)
          ctrl <- rfeControl(functions = rfFuncs,method = 'repeatedcv',repeats = 1,verbose = F)
          profile <- rfe(train_featureData, train_subgroups,sizes = feature_sizes,rfeControl = ctrl)
          best_features <- predictors(profile)
        }
        if(filterMethod=="knowledge"){best_features <- knowledgeFeatures}
        
        fitControl <- trainControl(method = "repeatedcv",number = innerFolds,repeats = innerRepeats)
        train_featureData <- train_featureData[,best_features]
        fit <- train(train_featureData, train_subgroups,method = trainMethod,trControl = fitControl,tuneLength = 10)
        
        
        print("##### FINISHED TRAINING #####")
        performance <- fit$results[row.names(fit$finalModel$tuneValue),]
        train_accuracy[outerFold] <- performance$Accuracy
        predictSubgroups <- predict(fit, newdata = validation_featureData[,best_features])
        cm <- confusionMatrix(predictSubgroups, validation_subgroups)
        if (verbose) print(cm)
        

        validate_accuracy[paste(outerFold,filterMethod,sep = "_")] = cm$overall['Accuracy']
#         if (outerFold == 1) results$fit = fit
#         if (outerFold > 1) if (validate_accuracy[outerFold-1] < cm$overall['Accuracy'] ) results$fit <- fit
#         
        
        method_accuracy[paste(outerFold,filterMethod,sep = "_")] <- validate_accuracy[paste(outerFold,filterMethod,sep = "_")]
        # print(paste(outerFold,filterMethod,validate_accuracy[paste(outerFold,filterMethod,sep = "_")],length(best_features)))
        
      }  # for3
    }  # for2
  }  # for1
  
  max(results$fit$results$Accuracy)
  print("calculating results")
  print(validate_accuracy)
  results$methodAcc = method_accuracy
  results$avgValAcc = mean(validate_accuracy)
  results$sdValAcc = sd(validate_accuracy)
  results$avgTrainAcc = mean(train_accuracy)
  results$sdTrainAcc = sd(train_accuracy)
  
  return (results)
}

# read the samples and associated classes from file
train.call = read.delim("train_call.txt")
train.clinical = read.delim("train_clinical.txt")
validation.call = read.delim("validation_call.txt")

### KNOWLEDGE BASED FEATURES BY TAMAR####
train_call <- read.delim("train_call.txt")
train<- t(train_call[,-1:-4])
train_clinical <- read.delim("train_clinical.txt")
comb <-  merge(train_clinical, train, by.x="Sample", by.y="row.names")


regionfinder <- function(chromosome, start, end){
  result <- c()
  for(i in which(train_call['Chromosome']==chromosome)){
    if (train_call$Start[i]<=start && train_call$End[i]>=start){
      result<-append(result, i)
    }else {
      if(train_call$Start[i]<=end && train_call$End[i]>=end){
        result<-append(result,i)
      }
    }
  }
  return(result)
}
cols <-c()
cols<-append(cols, regionfinder(17,35104766,35138441))
cols<-append(cols, regionfinder(6,152053324,152466099))
cols<-append(cols, regionfinder(14,63763506,63875021))
cols<-append(cols, regionfinder(11,100414313,100506465))
cols<-append(cols, regionfinder(2,60531806,60634272))
cols<-append(cols, regionfinder(10,8136673,8157170))
cols<-append(cols, regionfinder(10,129784913,129814645))
cols<-append(cols, regionfinder(2,191542121,191587181))
cols<-append(cols, regionfinder(17,36933396,36938167))
cols<-append(cols, regionfinder(12,51577238,51585127))
cols<-append(cols, regionfinder(12,51628922,51632951))
cols<-append(cols, regionfinder(1,110078077,110085183))
cols<-append(cols, regionfinder(20,54377852,54400800))
cols<-append(cols, regionfinder(17,73721872,73733310))
cols<-append(cols, regionfinder(5,68498669,68509822))
cols<-append(cols, regionfinder(20,41729179,41778537))
cols<-append(cols, regionfinder(17,35147713,35157070))
cols<-append(cols, regionfinder(18,58941559,59138341))
cols<-append(cols, regionfinder(11,8998511,9069731))
cols<-append(cols, regionfinder(1,110031965,110037890)) 
cols<-append(cols, regionfinder(22,22445036,22456502))
cols<-append(cols, regionfinder(17,7423529,7426152))
cols<-append(cols, regionfinder(9,33244163,33254737))
cols<-append(cols, regionfinder(8,38387813,38445509))
cols<-append(cols, regionfinder(6,43845926,43862202))
cols<-append(cols, regionfinder(6,20510377,20601921))
cols<-append(cols, regionfinder(6,32270599,32299822))
cols<-append(cols, regionfinder(20,5043599,5055268))
cols<-append(cols, regionfinder(12,51194630,51200510))
cols<-append(cols, regionfinder(17,37029218,37034355))
cols<-append(cols, regionfinder(17,59752964,59794509))
cols<-append(cols, regionfinder(1,206116942,206151306))
cols<-append(cols, regionfinder(12,51167225,51173448))
cols<-append(cols, regionfinder(9,138508717,138560135))
cols<-append(cols, regionfinder(1,119911553,120069703))
cols<-append(cols, regionfinder(19,15131445,15172792))
cols<-append(cols, regionfinder(12,25249449,25295121))
cols<-append(cols, regionfinder(7,55054219,55242524))
cols<-append(cols, regionfinder(17,7512445,7531642))
cols<-append(cols, regionfinder(8,128816862,128822853))
cols<-append(cols, regionfinder(20,35407971,35467867))
cols<-append(cols, regionfinder(11,522242,525591))
cols<-append(cols, regionfinder(3,41211405,41256943))
cols<-append(cols, regionfinder(6,20510377,20601921))
cols<-append(cols, regionfinder(1,153499284,153509905))
cols<-append(cols, regionfinder(1,241718158,242080053))
cols<-append(cols, regionfinder(13,113369595,113373975))
cols<-append(cols, regionfinder(22,20443946,20551970))
cols<-append(cols, regionfinder(8,144870417,144876619))
cols<-append(cols, regionfinder(1,151917737,151933087))
cols<-append(cols, regionfinder(1,203537816,203557506))
cols<-append(cols, regionfinder(8,87129718,87151042))
cols<-append(cols, regionfinder(8,141737683,142080514))
cols<-append(cols, regionfinder(5,96524397,96544700))
cols<-append(cols, regionfinder(18,43613464,43711510))
cols<-append(cols, regionfinder(5,70256524,70284595))
cols<-append(cols, regionfinder(3,30622998,30710638))
cols<-append(cols, regionfinder(8,22027877,22045326))
cols<-unique(cols)


v_cols <- paste("V",cols,sep = "")

#################################################################################

# Merge data and classes
merged = merge(train.clinical, t(train.call[,-1:-4]), by.x="Sample", by.y="row.names")
val = as.data.frame(t(validation.call[,-1:-4]))
her2_less <- merged[merged$Subgroup != 'HER2+',]
subgroups = factor(merged[merged$Subgroup != 'HER2+', 'Subgroup'])
results <- CrossvalidationScheme( her2_less[,3:2836], 
                                 subgroups = subgroups,
                                 n_outerFolds = 3,
                                 innerFolds = 10,
                                 innerRepeats = 1,
                                 trainMethod = "rf",
                                 filterMethods = c("rfe","knowledge"), 
                                 knowledgeFeatures = v_cols)

#################### LOOK WHICH METHOD HAD HIGHEST ACCURACY ####################
(1*0.32) +  (mean(c(results$MethodAccuracy$`1_knowledge`, results$MethodAccuracy$`2_knowledge`, results$MethodAccuracy$`3_knowledge`)) * 0.68)
(1*0.32) + (mean(c(results$MethodAccuracy$`1_rfe`, results$MethodAccuracy$`2_rfe`, results$MethodAccuracy$`3_rfe`)) * 0.68)


############ PREDICTING KNOWLEDGE BASES HER2+ ##################
HER2_plus <- as.character(rownames(val)[which(val["V2185"]==2)])
her2_df <- as.data.frame(cbind(HER2_plus,rep("HER2+",length(HER2_plus))))
colnames(her2_df) <- c("Sample","Subgroup")

#################### PRINTING THE LAST PREDICTION Supah Ugly #################### 
best_features <- v_cols
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
subgroups = factor(merged[merged$Subgroup != 'HER2+', 'Subgroup'])
train_featureData <- her2_less[,best_features]
final.fit <- train(train_featureData, subgroups, method = "rf", trControl = fitControl,tuneLength = 10)

val_call <- as.data.frame(t(validation.call[,-1:-4]))
predictSubgroups <- predict(final.fit, newdata = val_call[,best_features])
predictSubgroups_df <- data.frame(Sample = rownames(val_call), Subgroup = predictSubgroups)
final_results <- rbind(subset(predictSubgroups_df, !(predictSubgroups_df$Sample %in% her2_df$Sample)), her2_df)

write.table(final_results,file = "final_predictions.txt",quote = TRUE,sep = "\t",row.names = F,col.names = T)

