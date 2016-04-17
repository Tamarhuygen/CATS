filter_feature_selection <- function(fold_iter=1){  # function 
  for(folds in combo_folds){  # forloop1
    
    print(paste("fold : ",fold_iter))
    data <- combo[as.character(folds)]
    data_classes <- combo[,2]
    feature_list <- c() 
    features_all_methods <- data.frame()
    for(feature_test in feature_selection_methods){  # forloop2
      feature_list <- do_feature_selection(fs_test = feature_test, predictor = data, class_labels = data_classes)
      print(paste("feature length : ",length(feature_list)))
      features_all_methods <- cbind.all(features_all_methods,as.character(feature_list))
      
#       for(ca in classifying_algorithms){  # forloop3
# #         print(ca)
#         max_feature_number <- 100
#         if(length(feature_list) < 100){ max_feature_number <- length(feature_list)}
#         for(number_of_features in seq(from = 10, to = max_feature_number, by = 10)){ # forloop4
#           print(paste("# of features : ",number_of_features))
# #           print(feature_list)          
# #           print(names(feature_list)[1:number_of_features])
#           model <- make_classifier_model(predictor = data, 
#                                            class_labels = data_classes, 
#                                            feature_set = feature_list[1:number_of_features], 
#                                            classifying_method = ca)
#     
#           
#           
#           bestTuneParam <- model$bestTune[1,1]
#           if(ca=="nb"){bestTuneParam <- model$bestTune[1,2]}
#           fold_iter_char <- as.character(fold_iter)
#           acc <- as.character(max(model$results$Accuracy, na.rm = TRUE))
#           fn <- as.character(number_of_features)
#           
#           print(paste(fold_iter_char,"\t",feature_test,"\t",ca,"\t",fn,"\t",acc,"\t",bestTuneParam), file ="results_mod_acc.txt", append=TRUE)
#         
#        }  # forloop4 end 
#        }  # forloop3 end
             
}  # forloop2 end  
for(i in 1:(ncol(features_all_methods)-1)){
  for(l in i:ncol(features_all_methods)){
    
    jaccard_val <- jaccard(features_all_methods[,i],features_all_methods[,l])
    write(paste(fold_iter,"i:",i,"l:",l,jaccard_val), file ="results_fs.txt", append=TRUE)
  }
  print(i)
}
# print(paste("#####"))
# print(features_all_methods)
fold_iter <- fold_iter + 1
  }  # forloop1 end
}  # function end
