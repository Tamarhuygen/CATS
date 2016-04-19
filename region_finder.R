Train_call <- read.delim("train_call.txt")
T1 <- t(Train_call[,-1:-4])
Train_clinical <- read.delim("train_clinical.txt")
Combo <-  merge(Train_clinical, T1, by.x="Sample", by.y="row.names")


regionfinder <- function(chromosome, start, end){
  result <- c()
  for(i in which(Train_call['Chromosome']==chromosome)){
    if (Train_call$Start[i]<=start && Train_call$End[i]>=start){
      result<-append(result, i)
    }else {
      if(Train_call$Start[i]<=end && Train_call$End[i]>=end){
        result<-append(result,i)
      }
    }
  }
  return(result)
}
co <-c()
co<-append(co, regionfinder(19,15131445,15172792))
co<-append(co, regionfinder(12,25249449,25295121))
co<-append(co, regionfinder(7,55054219,55242524))
co<-append(co, regionfinder(17,7512445,7531642))
co<-append(co, regionfinder(8,128816862,128822853))
co<-append(co, regionfinder(20,35407971,35467867))
co<-append(co, regionfinder(11,522242,525591))
co<-append(co, regionfinder(3,41211405,41256943))
co<-append(co, regionfinder(6,20510377,20601921))
co<-append(co, regionfinder(1,153499284,153509905))
co<-append(co, regionfinder(1,241718158,242080053))
co<-append(co, regionfinder(13,113369595,113373975))
co<-append(co, regionfinder(22,20443946,20551970))
co<-append(co, regionfinder(8,144870417,144876619))
co<-append(co, regionfinder(1,151917737,151933087))
co<-append(co, regionfinder(1,203537816,203557506))
co<-append(co, regionfinder(8,87129718,87151042))
co<-append(co, regionfinder(8,141737683,142080514))
co<-append(co, regionfinder(5,96524397,96544700))
co<-append(co, regionfinder(18,43613464,43711510))
co<-append(co, regionfinder(5,70256524,70284595))
co<-append(co, regionfinder(3,30622998,30710638))
co<-append(co, regionfinder(8,22027877,22045326))
print(co)

