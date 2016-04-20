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


selected_features<-comb[cols]
correlation<-cor(selected_features)
for(i in seq(ncol(correlation))){
  j<-c()
  j<-which(correlation[i,]>=0.95 & correlation[i,]<1.0)
  if(length(j)>0){
    print(paste(as.character(i),as.character(j)))
  }
}
v_cols <- paste("V",cols,sep = "")



# co<-append(co, regionfinder(19,15131445,15172792))
# co<-append(co, regionfinder(12,25249449,25295121))
# co<-append(co, regionfinder(7,55054219,55242524))
# co<-append(co, regionfinder(17,7512445,7531642))
# co<-append(co, regionfinder(8,128816862,128822853))
# co<-append(co, regionfinder(20,35407971,35467867))
# co<-append(co, regionfinder(11,522242,525591))
# co<-append(co, regionfinder(3,41211405,41256943))
# co<-append(co, regionfinder(6,20510377,20601921))
# co<-append(co, regionfinder(1,153499284,153509905))
# co<-append(co, regionfinder(1,241718158,242080053))
# co<-append(co, regionfinder(13,113369595,113373975))
# co<-append(co, regionfinder(22,20443946,20551970))
# co<-append(co, regionfinder(8,144870417,144876619))
# co<-append(co, regionfinder(1,151917737,151933087))
# co<-append(co, regionfinder(1,203537816,203557506))
# co<-append(co, regionfinder(8,87129718,87151042))
# co<-append(co, regionfinder(8,141737683,142080514))
# co<-append(co, regionfinder(5,96524397,96544700))
# co<-append(co, regionfinder(18,43613464,43711510))
# co<-append(co, regionfinder(5,70256524,70284595))
# co<-append(co, regionfinder(3,30622998,30710638))
# co<-append(co, regionfinder(8,22027877,22045326))
# print(co)

# v_cols <- paste("V",co,sep = "")
