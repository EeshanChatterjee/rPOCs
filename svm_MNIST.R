
setwd("~/Projects/POCs/")

getImageDF<- function(readFile){
  
  m = lapply(c(1:10000),function(x) matrix(readBin(readFile,integer(), size=1, n=28*28, endian="big"),28,28))
  bigList<-lapply(m,function(x) as.vector(t(x)))
  bigmatrix<-do.call(rbind,bigList)
  bigDF<-as.data.frame(bigmatrix)
  return(bigDF)}

load_label_file <- function(filename) {
  #   f = file(filename,'rb')
  f=filename
  readBin(f,'integer',n=1,size=4,endian='big')
  n = readBin(f,'integer',n=1,size=4,endian='big')
  y = readBin(f,'integer',n=n,size=1,signed=F)
  close(f)
  y
}

to.read = file("t10k-images.idx3-ubyte", "rb")
testImages<-getImageDF(to.read)

to.read = file("t10k-labels.idx1-ubyte", "rb")
testLabels<-load_label_file(to.read)

to.read = file("train-images.idx3-ubyte", "rb")
trainImages<-getImageDF(to.read)

to.read = file("train-labels.idx1-ubyte", "rb")
trainLabels<-load_label_file(to.read)
trainLabels = trainLabels[1:10000]
# -------------------

# SVM for zero vs non-zero

trainLabels[which(!trainLabels==0)]=1

trainMatrix<-cbind(trainImages,trainLabels)

require('kernlab')

svmModel=ksvm(x=as.matrix(trainImages[1:2000,]),y=trainLabels[1:2000],type="C-svc",kernel="vanilladot",kpar="automatic",prob.model=TRUE)
save(svmModel,file = "mnistModel.RData")

predictions = predict(svmModel,as.matrix(testImages[1:1000,]),type="probabilities")
predictedClass = apply(predictions,1,FUN = function(x) which(x == max(x))) - 1



