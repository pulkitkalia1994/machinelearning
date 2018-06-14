machine_learning<-function(){
  
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","train.csv")
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv","test.csv")
  train<-read.csv("train.csv")
  test<-read.csv("test.csv")

  columns<-grep("*accel*",names(train),value = TRUE)
  data<-select(train,columns)
  data<-select(data,-c(var_total_accel_belt,var_accel_arm,var_accel_dumbbell,var_accel_forearm))
  
  classe<-train$classe
  
  
  data<-cbind(data,classe=classe)
  df.m <- melt(data, "classe")
  ggplot(df.m,aes(x=value,y=classe))+geom_point()+facet_wrap(~variable)
  
  train<-""
  
  ##setting number=5 dues to memory constraints
  tr<-trainControl(method = "cv", number = 5)
  cartGrid = expand.grid( .cp = seq(0.00002,0.002,0.00002))
  model<-train(classe~.,data=data,method="rpart",trControl=tr,tuneGrid=cartGrid)
  ##accuracy of 82% at cp=0.0002
  prp(model$finalModel)
  getTrainPerf(model)
  
  
  
  ##randomForest accuracy ~94%
  model2<-randomForest(classe~.,data=data)
  predict(model2,newdata = test)
  ##getTrainPerf(model2)
  
  model3<-train(classe~.,data=data,method="rf",trControl=trainControl(method="cv",number=5))
  ##accuracy of ~95% with cv
  getTrainPerf(model3)
  
  predictionResults<-predict(model3,newdata = test)
  print(predictionResults)

  
}