library(caret)
setwd("C:/Users/Ashvin/B4TM/data/")

###################
# data processing #
###################
train_call <- read.delim("Train_call.txt", header=TRUE, sep="\t", quote="\"", dec=".",fill=TRUE, comment.char="")
train_clinical <-read.delim("Train_clinical.txt", header=TRUE, sep="\t", quote="\"", dec=".",fill=TRUE, comment.char="")
rownames(train_clinical) <- train_clinical$Sample
train_call_trans <- as.data.frame(t(subset(train_call[,5:104])))
combo <- merge(train_clinical, train_call_trans, by="row.names")
row.names(combo) <- combo$Row.names 
combo$Sample <- NULL




################
# not used yet #
################
# train_combo_index <- createDataPartition(combo$Subgroup, p = 0.7, groups = 3)
# training_combo <- combo[train_combo_index$Resample1,]
# test_combo <- combo[-train_combo_index$Resample1,]
# test_classes <- test_combo$Subgroup 
# test_combo$Subgroup <- NULL
