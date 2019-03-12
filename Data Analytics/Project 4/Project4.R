  setwd("C:/Users/MOIN SHAIKH/Desktop/")
  
  nursery=read.csv("nursery.csv",FALSE)
  nursery=data.frame(nursery)
  typeof(nursery)
  
  # load the package
  library(RWeka)
  library(ROCR)
  library(caret)
  # load data
  data(nursery)
  
  
  
  ##############################Implementing C4.5 algorithm now 
  
  training_size <- floor(0.70 * nrow(nursery))
  
  set.seed(140)
  training_set_indexes <- sample(seq_len(nrow(nursery)), size = training_size)
  
  
  training_set=nursery[training_set_indexes,]
  test_set=nursery[-training_set_indexes,]
  
  #Training the classifier
  fit <- J48(V9~., data=training_set)
  
  
  # summarize the fit
  summary(fit)
  
  # make predictions
  predictions <- predict(fit, test_set[,1:8])
  #predictions
  
  length(predictions)
  
  # summarize accuracy
  table(predictions, test_set$V9)
  
  accuracy=0
  
  for(j in 1:length(predictions))
  {
    if(predictions[j]==test_set$V9[j])
      accuracy=accuracy+1
  }
  
  #accuracy calculated here
  accuracy=accuracy/length(predictions)
  
  result <- confusionMatrix(predictions,test_set$V9)
  
  #Accuracy calculated
  accuracy
  
  
  #######Calculations of precision ,recall and f1_Score for different classes
  
  #Precision for class: not_recom
  p=ifelse(!is.na(result$byClass[1,3]),result$byClass[1,3],0)
  p
  
  #Recall for class: not_recom
  r=ifelse(!is.na(result$byClass[1,1]),result$byClass[1,1],0)
  r
  
  #f1_Score for class: not_recom
  f=ifelse(((r+p)!=0),2*r*p/(r+p),0)
  f
  
  #Precision for class: priority
  p=ifelse(!is.na(result$byClass[2,3]),result$byClass[2,3],0)
  p
  
  #Recall for class: priority
  r=ifelse(!is.na(result$byClass[2,1]),result$byClass[2,1],0)
  r
  
  #f1_Score for class: not_recom
  f=ifelse(((r+p)!=0),2*r*p/(r+p),0)
  f
  
  #Precision for class: recommend
  p=ifelse(!is.na(result$byClass[3,3]),result$byClass[3,3],0)
  p
  
  #Recall for class: recommend
  r=ifelse(!is.na(result$byClass[3,1]),result$byClass[3,1],0)
  r
  
  #f1_Score for class: not_recom
  f=ifelse(((r+p)!=0),2*r*p/(r+p),0)
  f
  
  #Precision for class: spec_prior
  p=ifelse(!is.na(result$byClass[4,3]),result$byClass[4,3],0)
  p
  
  #Recall for class: spec_prior
  r=ifelse(!is.na(result$byClass[4,1]),result$byClass[4,1],0)
  r
  
  #f1_Score for class: not_recom
  f=ifelse(((r+p)!=0),2*r*p/(r+p),0)
  f
  
  #Precision for class: very_recom
  p=ifelse(!is.na(result$byClass[5,3]),result$byClass[5,3],0)
  p
  
  #Recall for class: very_recom
  r=ifelse(!is.na(result$byClass[5,1]),result$byClass[5,1],0)
  r
  
  #f1_Score for class: not_recom
  f=ifelse(((r+p)!=0),2*r*p/(r+p),0)
  f
  
  #Confusion matrix
  table(predictions, test_set$V9)
  
  
  
  
  
  
  
  
  ########################################################
  
  
  #Code for 10 fold cross validation here
  
  #Dividing it in 10 equal parts
  per_fold_size=nrow(nursery)/10
  
  index_train=list()
  new_index_train=list()
  
  #Dividing all the index in 10 equal parts
  for(i in c(1:10)) {
    index_train[[i]]=seq((i-1)*per_fold_size+1,(i)*per_fold_size,1)
  }
  
  # Dividing all the index in sets of 9:1 ratio 
  for(j in c(0:9))
    {
    new_index_train[[j+1]]=c(index_train[[(j)%%10+1]],index_train[[(j+1)%%10+1]],index_train[[(j+2)%%10+1]],index_train[[(j+3)%%10+1]],index_train[[(j+4)%%10+1]],index_train[[(j+5)%%10+1]],index_train[[(j+6)%%10+1]],index_train[[(j+7)%%10+1]],index_train[[(j+8)%%10+1]])
  }
  
  Accuracy=0
  
  training=list()
  
  precision_values=list()
  
  #Encoded calues for class representation 
  
  #not_recom=1
  #priority=2
  #recommend=3
  #spec_prior=4
  #very_recom=5
  
  
  precision_values[[1]]=0
  precision_values[[2]]=0
  precision_values[[3]]=0
  precision_values[[4]]=0
  precision_values[[5]]=0
  
  
  f1_score=list()
  
  f1_score[[1]]=0
  f1_score[[2]]=0
  f1_score[[3]]=0
  f1_score[[4]]=0
  f1_score[[5]]=0
  recall=list()
  
  recall[[1]]=0
  recall[[2]]=0
  recall[[3]]=0
  recall[[4]]=0
  recall[[5]]=0
  
  
  tpr_c4.5=matrix(0,10,5)
  fpr_c4.5=matrix(0,10,5)
  
  #10 fold cross validation starts here
  
  for (i in c(1:10)) {
    
    #taking the training datafrom the index calculated above 
    #training data:test_Data=9:1 ratio 
    
     train_data=nursery[new_index_train[[i]],]
     test_data=nursery[-new_index_train[[i]],]
     
     fit <- J48(V9~., data=train_data)
     
     
     # make predictions
     predictions <- predict(fit, test_data[,1:8])
    
     #print confusion matrix for this iteration 
     print(table(predictions,test_data$V9))
     
     #Calculating accuracy for this iteration 
     accuracy=0
     
     for(j in 1:length(predictions))
     {
       if(predictions[j]==test_data$V9[j])
         accuracy=accuracy+1
     }
     
     
     accuracy=accuracy/length(predictions)
     
     #print(accuracy)
     result <- confusionMatrix(predictions,test_data$V9)
     
     #print result // This result consists of all values precison , recall etc 
     print(result)
     
     precision_values[[1]]=precision_values[[1]]+ifelse(!is.na(result$byClass[1,3]),result$byClass[1,3],0)
     precision_values[[2]]=precision_values[[2]]+ifelse(!is.na(result$byClass[2,3]),result$byClass[2,3],0)
     precision_values[[3]]=precision_values[[3]]+ifelse(!is.na(result$byClass[3,3]),result$byClass[3,3],0)
     precision_values[[4]]=precision_values[[4]]+ifelse(!is.na(result$byClass[4,3]),result$byClass[4,3],0)
     precision_values[[5]]=precision_values[[5]]+ifelse(!is.na(result$byClass[5,3]),result$byClass[5,3],0)
     
     recall[[1]]=recall[[1]]+ifelse(!is.na(result$byClass[1,1]),result$byClass[1,1],0)
     recall[[2]]=recall[[2]]+ifelse(!is.na(result$byClass[2,1]),result$byClass[2,1],0)
     recall[[3]]=recall[[3]]+ifelse(!is.na(result$byClass[3,1]),result$byClass[3,1],0)
     recall[[4]]=recall[[4]]+ifelse(!is.na(result$byClass[4,1]),result$byClass[4,1],0)
     recall[[5]]=recall[[5]]+ifelse(!is.na(result$byClass[5,1]),result$byClass[5,1],0)
     
     
     
     # Calculating tpr for calculating ROC curve later
     
     tpr_cart[i,1]=ifelse(!is.na(result$byClass[1,1]),result$byClass[1,1],0)
     tpr_cart[i,2]=ifelse(!is.na(result$byClass[2,1]),result$byClass[2,1],0)
     tpr_cart[i,3]=ifelse(!is.na(result$byClass[3,1]),result$byClass[3,1],0)
     tpr_cart[i,4]=ifelse(!is.na(result$byClass[4,1]),result$byClass[4,1],0)
     tpr_cart[i,5]=ifelse(!is.na(result$byClass[5,1]),result$byClass[5,1],0)
     
     # calculating fpr by 1-specificity
     
     fpr_cart[i,1]=1-ifelse(!is.na(result$byClass[1,2]),result$byClass[1,2],0)
     fpr_cart[i,2]=1-ifelse(!is.na(result$byClass[2,2]),result$byClass[2,2],0)
     fpr_cart[i,3]=1-ifelse(!is.na(result$byClass[3,2]),result$byClass[3,2],0)
     fpr_cart[i,4]=1-ifelse(!is.na(result$byClass[4,2]),result$byClass[4,2],0)
     fpr_cart[i,5]=1-ifelse(!is.na(result$byClass[5,2]),result$byClass[5,2],0)
     
     p=ifelse(!is.na(result$byClass[1,3]),result$byClass[1,3],0)
     r=ifelse(!is.na(result$byClass[1,1]),result$byClass[1,1],0)
     
     f1_score[[1]]=f1_score[[1]]+ifelse(((r+p)!=0),2*r*p/(r+p),0)
     
     p=ifelse(!is.na(result$byClass[2,3]),result$byClass[2,3],0)
     r=ifelse(!is.na(result$byClass[2,1]),result$byClass[2,1],0)
     
     f1_score[[2]]=f1_score[[2]]+ifelse(((r+p)!=0),2*r*p/(r+p),0)
     
     
     p=ifelse(!is.na(result$byClass[3,3]),result$byClass[3,3],0)
     r=ifelse(!is.na(result$byClass[3,1]),result$byClass[3,1],0)
     
     f1_score[[3]]=f1_score[[3]]+ifelse(((r+p)!=0),2*r*p/(r+p),0)
     
     
     p==precision_values[[4]]+ifelse(!is.na(result$byClass[4,3]),result$byClass[4,3],0)
     r=ifelse(!is.na(result$byClass[4,1]),result$byClass[4,1],0)
     
     f1_score[[4]]=f1_score[[4]]+ifelse(((r+p)!=0),2*r*p/(r+p),0)
     
     
     p=ifelse(!is.na(result$byClass[5,3]),result$byClass[5,3],0)
     r=ifelse(!is.na(result$byClass[5,1]),result$byClass[5,1],0)
     
     f1_score[[5]]=f1_score[[5]]+ifelse(((r+p)!=0),2*r*p/(r+p),0)
     
      Accuracy=Accuracy+accuracy
     
  }
  
  
  #Final accuracy after 10 fold 
  Accuracy=Accuracy/10
  
  f1_score[[1]]=f1_score[[1]]/10
  f1_score[[2]]=f1_score[[2]]/10
  f1_score[[3]]=f1_score[[3]]/10
  f1_score[[4]]=f1_score[[4]]/10
  f1_score[[5]]=f1_score[[5]]/10
  
  
  
  recall[[1]]=recall[[1]]/10
  recall[[2]]=recall[[2]]/10
  recall[[3]]=recall[[3]]/10
  recall[[4]]=recall[[4]]/10
  recall[[5]]=recall[[5]]/10
  
  
  
  precision_values[[1]]=precision_values[[1]]/10
  precision_values[[2]]=precision_values[[2]]/10
  precision_values[[3]]=precision_values[[3]]/10
  precision_values[[4]]=precision_values[[4]]/10
  precision_values[[5]]=precision_values[[5]]/10
  
  #Averaged accuracy for 10 fold cross validation 
  Accuracy
  
  
  # Note : #not_recom=1
           #priority=2
           #recommend=3
           #spec_prior=4
           #very_recom=5
  
  # F1 score for each class after 10 fold cross validation
  f1_score
  
  # Similarly recall for each class after 10 fold cross validation
  recall
  
  # Similarly precision values for each class after 10 fold cross validation
  precision_values
  
  # Confusion matrix for each run in 10 fold cross validation is already printed in the loop 
  
  
  ###############################################################################################
  ###############################################################################################
  ############################Implementing CART algorithm######################################
  
  
  # load the package
  library(rpart)
  
  # Dividing the dataset in the ratio 7:3
  training_size <- floor(0.70 * nrow(nursery))
  
  set.seed(140)
  training_set_indexes <- sample(seq_len(nrow(nursery)), size = training_size)
  
  
  training_set=nursery[training_set_indexes,]
  test_set=nursery[-training_set_indexes,]
  #Training the classifier 
  
  fit <- rpart(V9~., data=training_set)
  
  
  # summarize the fit
  summary(fit)
  
  # make predictions
  predictions <- predict(fit, test_set[,1:8], type="class")
  #predictions
  
  length(predictions)
  
  # summarize accuracy
  table(predictions, test_set$V9)
  
  accuracy=0
  
  for(j in 1:length(predictions))
  {
    if(predictions[j]==test_set$V9[j])
      accuracy=accuracy+1
  }
  
  #accuracy calculated here
  accuracy=accuracy/length(predictions)
  
  result <- confusionMatrix(predictions,test_set$V9)
  
  #Accuracy calculated
  accuracy
  
  
  #######Calculations of precision ,recall and f1_Score for different classes
  
  #Precision for class: not_recom
  p=ifelse(!is.na(result$byClass[1,3]),result$byClass[1,3],0)
  p
  
  #Recall for class: not_recom
  r=ifelse(!is.na(result$byClass[1,1]),result$byClass[1,1],0)
  r
  
  #f1_Score for class: not_recom
  f=ifelse(((r+p)!=0),2*r*p/(r+p),0)
  f
  
  #Precision for class: priority
  p=ifelse(!is.na(result$byClass[2,3]),result$byClass[2,3],0)
  p
  
  #Recall for class: priority
  r=ifelse(!is.na(result$byClass[2,1]),result$byClass[2,1],0)
  r
  
  #f1_Score for class: not_recom
  f=ifelse(((r+p)!=0),2*r*p/(r+p),0)
  f
  
  #Precision for class: recommend
  p=ifelse(!is.na(result$byClass[3,3]),result$byClass[3,3],0)
  p
  
  #Recall for class: recommend
  r=ifelse(!is.na(result$byClass[3,1]),result$byClass[3,1],0)
  r
  
  #f1_Score for class: not_recom
  f=ifelse(((r+p)!=0),2*r*p/(r+p),0)
  f
  
  #Precision for class: spec_prior
  p=ifelse(!is.na(result$byClass[4,3]),result$byClass[4,3],0)
  p
  
  #Recall for class: spec_prior
  r=ifelse(!is.na(result$byClass[4,1]),result$byClass[4,1],0)
  r
  
  #f1_Score for class: not_recom
  f=ifelse(((r+p)!=0),2*r*p/(r+p),0)
  f
  
  #Precision for class: very_recom
  p=ifelse(!is.na(result$byClass[5,3]),result$byClass[5,3],0)
  p
  
  #Recall for class: very_recom
  r=ifelse(!is.na(result$byClass[5,1]),result$byClass[5,1],0)
  r
  
  #f1_Score for class: not_recom
  f=ifelse(((r+p)!=0),2*r*p/(r+p),0)
  f
  
  #Confusion matrix
  table(predictions, test_set$V9)
  

  
  ########################################################
  
  
  #Code for 10 fold cross validation here
  
  per_fold_size=nrow(nursery)/10
  
  index_train=list()
  new_index_train=list()
  for(i in c(1:10)) {
    index_train[[i]]=seq((i-1)*per_fold_size+1,(i)*per_fold_size,1)
  }
  for(j in c(0:9))
  {
    new_index_train[[j+1]]=c(index_train[[(j)%%10+1]],index_train[[(j+1)%%10+1]],index_train[[(j+2)%%10+1]],index_train[[(j+3)%%10+1]],index_train[[(j+4)%%10+1]],index_train[[(j+5)%%10+1]],index_train[[(j+6)%%10+1]],index_train[[(j+7)%%10+1]],index_train[[(j+8)%%10+1]])
  }
  
  Accuracy=0
  
  per_fold_size=nrow(training_set)/10
  training=list()
  
  precision_values=list()
  
  #not_recom=1
  #priority=2
  #recommend=3
  #spec_prior=4
  #very_recom=5
  
  
  precision_values[[1]]=0
  precision_values[[2]]=0
  precision_values[[3]]=0
  precision_values[[4]]=0
  precision_values[[5]]=0
  
  
  f1_score=list()
  
  f1_score[[1]]=0
  f1_score[[2]]=0
  f1_score[[3]]=0
  f1_score[[4]]=0
  f1_score[[5]]=0
  recall=list()
  
  recall[[1]]=0
  recall[[2]]=0
  recall[[3]]=0
  recall[[4]]=0
  recall[[5]]=0
  
  tpr_cart=matrix(0,10,5)
  fpr_cart=matrix(0,10,5)
  
  for (i in c(1:10)) {
    train_data=nursery[new_index_train[[i]],]
    test_data=nursery[-new_index_train[[i]],]
    
    #Training the classifier
    fit <- rpart(V9~., data=train_data)
    
    
    # make predictions
    predictions <- predict(fit, test_data[,1:8], type="class")
    
    #print confusion matrix for this iteration 
    
    print(table(predictions,test_data$V9))
    
    accuracy=0
    
    for(j in 1:length(predictions))
    {
      if(predictions[j]==test_data$V9[j])
        accuracy=accuracy+1
    }
    
    
    accuracy=accuracy/length(predictions)
    
    
    
    #print(accuracy)
    result <- confusionMatrix(predictions,test_data$V9)
    #print result // This result consists of all values precison , recall etc 
    print(result)
    
    precision_values[[1]]=precision_values[[1]]+ifelse(!is.na(result$byClass[1,3]),result$byClass[1,3],0)
    precision_values[[2]]=precision_values[[2]]+ifelse(!is.na(result$byClass[2,3]),result$byClass[2,3],0)
    precision_values[[3]]=precision_values[[3]]+ifelse(!is.na(result$byClass[3,3]),result$byClass[3,3],0)
    precision_values[[4]]=precision_values[[4]]+ifelse(!is.na(result$byClass[4,3]),result$byClass[4,3],0)
    precision_values[[5]]=precision_values[[5]]+ifelse(!is.na(result$byClass[5,3]),result$byClass[5,3],0)
    
    recall[[1]]=recall[[1]]+ifelse(!is.na(result$byClass[1,1]),result$byClass[1,1],0)
    recall[[2]]=recall[[2]]+ifelse(!is.na(result$byClass[2,1]),result$byClass[2,1],0)
    recall[[3]]=recall[[3]]+ifelse(!is.na(result$byClass[3,1]),result$byClass[3,1],0)
    recall[[4]]=recall[[4]]+ifelse(!is.na(result$byClass[4,1]),result$byClass[4,1],0)
    recall[[5]]=recall[[5]]+ifelse(!is.na(result$byClass[5,1]),result$byClass[5,1],0)
    
    
    
    # Calculating tpr for calculating ROC curve later
    
    tpr_cart[i,1]=ifelse(!is.na(result$byClass[1,1]),result$byClass[1,1],0)
    tpr_cart[i,2]=ifelse(!is.na(result$byClass[2,1]),result$byClass[2,1],0)
    tpr_cart[i,3]=ifelse(!is.na(result$byClass[3,1]),result$byClass[3,1],0)
    tpr_cart[i,4]=ifelse(!is.na(result$byClass[4,1]),result$byClass[4,1],0)
    tpr_cart[i,5]=ifelse(!is.na(result$byClass[5,1]),result$byClass[5,1],0)
    
    # calculating fpr by 1-specificity
    
    fpr_cart[i,1]=1-ifelse(!is.na(result$byClass[1,2]),result$byClass[1,2],0)
    fpr_cart[i,2]=1-ifelse(!is.na(result$byClass[2,2]),result$byClass[2,2],0)
    fpr_cart[i,3]=1-ifelse(!is.na(result$byClass[3,2]),result$byClass[3,2],0)
    fpr_cart[i,4]=1-ifelse(!is.na(result$byClass[4,2]),result$byClass[4,2],0)
    fpr_cart[i,5]=1-ifelse(!is.na(result$byClass[5,2]),result$byClass[5,2],0)
    
    p=ifelse(!is.na(result$byClass[1,3]),result$byClass[1,3],0)
    r=ifelse(!is.na(result$byClass[1,1]),result$byClass[1,1],0)
    
    f1_score[[1]]=f1_score[[1]]+ifelse(((r+p)!=0),2*r*p/(r+p),0)
    
    p=ifelse(!is.na(result$byClass[2,3]),result$byClass[2,3],0)
    r=ifelse(!is.na(result$byClass[2,1]),result$byClass[2,1],0)
    
    f1_score[[2]]=f1_score[[2]]+ifelse(((r+p)!=0),2*r*p/(r+p),0)
    
    
    p=ifelse(!is.na(result$byClass[3,3]),result$byClass[3,3],0)
    r=ifelse(!is.na(result$byClass[3,1]),result$byClass[3,1],0)
    
    f1_score[[3]]=f1_score[[3]]+ifelse(((r+p)!=0),2*r*p/(r+p),0)
    
    
    p==precision_values[[4]]+ifelse(!is.na(result$byClass[4,3]),result$byClass[4,3],0)
    r=ifelse(!is.na(result$byClass[4,1]),result$byClass[4,1],0)
    
    f1_score[[4]]=f1_score[[4]]+ifelse(((r+p)!=0),2*r*p/(r+p),0)
    
    
    p=ifelse(!is.na(result$byClass[5,3]),result$byClass[5,3],0)
    r=ifelse(!is.na(result$byClass[5,1]),result$byClass[5,1],0)
    
    f1_score[[5]]=f1_score[[5]]+ifelse(((r+p)!=0),2*r*p/(r+p),0)
    
    Accuracy=Accuracy+accuracy
    
  }
  
  
  #Final accuracy after 10 fold 
  Accuracy=Accuracy/10
  
  f1_score[[1]]=f1_score[[1]]/10
  f1_score[[2]]=f1_score[[2]]/10
  f1_score[[3]]=f1_score[[3]]/10
  f1_score[[4]]=f1_score[[4]]/10
  f1_score[[5]]=f1_score[[5]]/10
  
  
  
  recall[[1]]=recall[[1]]/10
  recall[[2]]=recall[[2]]/10
  recall[[3]]=recall[[3]]/10
  recall[[4]]=recall[[4]]/10
  recall[[5]]=recall[[5]]/10
  
  
  
  precision_values[[1]]=precision_values[[1]]/10
  precision_values[[2]]=precision_values[[2]]/10
  precision_values[[3]]=precision_values[[3]]/10
  precision_values[[4]]=precision_values[[4]]/10
  precision_values[[5]]=precision_values[[5]]/10
  
  #Averaged accuracy for 10 fold cross validation 
  Accuracy
  
  # F1 score for each class after 10 fold cross validation
  # Note : #not_recom=1
  #priority=2
  #recommend=3
  #spec_prior=4
  #very_recom=5
  
  f1_score
  
  # Similarly recall for each class after 10 fold cross validation
  recall
  
  # Similarly precision values for each class after 10 fold cross validation
  precision_values
  
  # Confusion matrix for each run in 10 fold cross validation is already printed in the loop 
  
  
  
  
  
  
  
  
  ###############################################################################################
  ##################################Now generating the ROC curves################################
  
  
  tpr_c4.5
  fpr_c4.5
  tpr_cart
  fpr_cart
  
  #ROC curve for class : not_recom
  
 
  
  plot(fpr_c4.5[,1],tpr_c4.5[,1],ylim=range(c(tpr_c4.5[,1],tpr_cart[,1])),xlim=range(c(fpr_c4.5[,1],fpr_cart[,1])),xlab="fpr",ylab="tpr",main="ROC_curve for class: not_recom",type="p",col="green")
  
  par(new=TRUE)
  
  plot(fpr_cart[,1],tpr_cart[,1],ylim=range(c(tpr_c4.5[,1],tpr_cart[,1])),xlim=range(c(fpr_c4.5[,1],fpr_cart[,1])),xlab="fpr",ylab="tpr",main="ROC_curve for class: not_recom",type="p",col="red")
  
  
  
  
  
  #ROC curve for class : priority 
  
  smoothingSpline = smooth.spline(fpr_c4.5[,2], tpr_c4.5[,2], spar=0.01)
  
  plot(fpr_c4.5[,2],tpr_c4.5[,2],ylim=range(c(tpr_c4.5[,2],tpr_cart[,2])),xlim=range(c(fpr_c4.5[,2],fpr_cart[,2])),xlab="fpr",ylab="tpr",main="ROC_curve for class: priority",type="p",col="green")
  
  lines(smoothingSpline,col="green")
  
  par(new=TRUE)
  
  smoothingSpline = smooth.spline(fpr_cart[,2], tpr_cart[,2], spar=0.01)
  
  plot(fpr_cart[,2],tpr_cart[,2],ylim=range(c(tpr_c4.5[,2],tpr_cart[,2])),xlim=range(c(fpr_c4.5[,2],fpr_cart[,2])),xlab="fpr",ylab="tpr",main="ROC_curve for class: priority",type="p",col="red")
  
  lines(smoothingSpline,col="red")
  
  
  
  
  #ROC curve for class : recommend
  

  
  plot(fpr_c4.5[,3],tpr_c4.5[,3],ylim=range(c(tpr_c4.5[,3],tpr_cart[,3])),xlim=range(c(fpr_c4.5[,3],fpr_cart[,3]+1e-06)),xlab="fpr",ylab="tpr",main="ROC_curve for class: recommend",type="p",col="green")
  
  
  
  par(new=TRUE)
  
  
  
  plot(fpr_cart[,3],tpr_cart[,3],ylim=range(c(tpr_c4.5[,3],tpr_cart[,3])),xlim=range(c(fpr_c4.5[,3],fpr_cart[,3])),xlab="fpr",ylab="tpr",main="ROC_curve for class: recommend",type="p",col="red")
  
  
  
  
  
  #ROC curve for class :spec_prior
  
  smoothingSpline = smooth.spline(fpr_c4.5[,4], tpr_c4.5[,4], spar=0.01)
  
  plot(fpr_c4.5[,4],tpr_c4.5[,4],ylim=range(c(tpr_c4.5[,4],tpr_cart[,4])),xlim=range(c(fpr_c4.5[,4],fpr_cart[,4])),xlab="fpr",ylab="tpr",main="ROC_curve for class: spec_prior",type="p",col="green")
  
  lines(smoothingSpline,col="green")
  
  par(new=TRUE)
  
  smoothingSpline = smooth.spline(fpr_cart[,4], tpr_cart[,4], spar=0.01)
  
  plot(fpr_cart[,4],tpr_cart[,4],ylim=range(c(tpr_c4.5[,4],tpr_cart[,4])),xlim=range(c(fpr_c4.5[,4],fpr_cart[,4])),xlab="fpr",ylab="tpr",main="ROC_curve for class: spec_prior",type="p",col="red")
  
  lines(smoothingSpline,col="red")
  
  
  
  #ROC curve for class :very_recom
  
  
  
  plot(fpr_c4.5[,5],tpr_c4.5[,5],ylim=range(c(tpr_c4.5[,5],tpr_cart[,5])),xlim=range(c(fpr_c4.5[,5],fpr_cart[,5])),xlab="fpr",ylab="tpr",main="ROC_curve for class: very_recom",type="p",col="green")
  
  
  
  par(new=TRUE)
  
  
  
  plot(fpr_cart[,5],tpr_cart[,5],ylim=range(c(tpr_c4.5[,5],tpr_cart[,5])),xlim=range(c(fpr_c4.5[,5],fpr_cart[,5])),xlab="fpr",ylab="tpr",main="ROC_curve for class: very_recom",type="p",col="red")
  
  
  
