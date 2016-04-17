library(caret)
library(entropy)
setwd("C:/Users/Ashvin/B4TM/data/")
set.seed(0591)

#############
# load data #
#############
source("C:/Users/Ashvin/Dropbox/Bioinf_master/jaar_1/periode_5/B4TM/B4TM_process_data.R")


#############################
# feature selection methods #
#############################
calculate_entropy <- function(feature_values){
  feature_freqs <- as.vector(table(factor(feature_values, levels = -1:2)))
  entr <- entropy(feature_freqs)
  return(entr)
}
calculate_ig <- function(feature_values){
  feature_freqs <- (table(feature_values))
  ig <- information.gain()
}
do_feature_selection <- function(fs_test, predictor, class_labels){
  colnames(data)
  feature_list <- NULL
  if(fs_test=="t.test"){
    
    ttest_results <- apply(X = predictor, MARGIN = 2, FUN = t.test)
    feature_list <- sapply(ttest_results, '[[', 'p.value')
    feature_list <- feature_list[which(feature_list < 0.01)]
    feature_list <- names(feature_list)
    #feature_list <- as.vector(which(feature_list[order(feature_list)] > 0.01))+2
    
  }
  if(fs_test=="wilcox.test"){
    wilcoxtest_result <- apply(X = predictor, MARGIN = 2, FUN = wilcox.test)
    feature_list <- sapply(wilcoxtest_result, '[[', 'p.value')
    feature_list <- feature_list[which(feature_list < 0.01)]
    feature_list <- names(feature_list)
    
  }
  if(fs_test=="entropy"){
    entropytest_result <- as.vector(apply(X = predictor, MARGIN = 2, FUN = calculate_entropy))
    feature_list <- as.vector(order(entropytest_result,decreasing = FALSE))
    
  }
  if(fs_test=="pca"){
    pca_analyses <- prcomp(predictor)
    aload <- abs(pca_analyses$rotation)
    aload_contrib_pc1 <- sweep(aload, 2, colSums(aload), "/")[,1]
    feature_list <- names(aload_contrib_pc1)[order(aload_contrib_pc1,decreasing=TRUE)]
  }
  if(fs_test=="random"){
    feature_list <- sample(1:ncol(predictor),replace = FALSE) 
  }
  #binominal
  # return features indices, ordered by importance
  return(feature_list)
}


#####################################
# train model with cross validation #
#####################################
make_classifier_model <- function(predictor, feature_set, class_labels, classifying_method){
  train_control <- trainControl(method="cv", number=10)
  model <- train(class_labels ~.,predictor[,feature_set], trControl = train_control, method = classifying_method)
  return(model)
}

############################
# create data.frame easily #
############################
cbind.all <- function (...) {
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function(x) rbind(x, matrix(, n - nrow(x), ncol(x)))))
}

#################################################
# cross validation to select number of features #
#################################################
feature_selection_methods <- c("t.test","wilcox.test","entropy","pca","random")
classifying_algorithms <- c("knn","nb","rf")
combo_folds <- createFolds(combo[,3:2836], k = 10)
filter_feature_selection()

