
###################
# Data processing #
###################
train.call = read.delim("train_call.txt")
train.clinical = read.delim("train_clinical.txt")
validation.call = read.delim("validation_call.txt")
merged = merge(train.clinical, t(train.call[,-1:-4]), by.x="Sample", by.y="row.names")

her2_less <- merged[merged$Subgroup != 'HER2+',]
