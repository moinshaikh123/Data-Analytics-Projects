#Name : Shaikh Moin Dastagir
#Roll no : 16CS30033

setwd("3rdyear/Data Analytics/")                         #First Setting Working Directory
adult <-read.csv("./adult.csv",TRUE,",")                 #Reading the file 
adult_original <-adult                                   # Making a copy of the original file for future use

adult_preprocessed<-adult

for (i in 1:ncol(adult)) {                              # Removing the rows with unknown values
  adult_preprocessed<- adult_preprocessed[!adult_preprocessed[,i]==" ?",]                    
}


write.csv(adult,"Preprocessed.csv",row.names=FALSE)           # Writing the preprocessed file 

                                                       #Making a copy of the preprocessed dataframe 
adult_label_encoded<-adult_preprocessed


#Now Label Encoding

for (i in 1:ncol(adult_label_encoded)) {
  if(!is.numeric(adult_label_encoded[,i]))
  {
    temp_factor=factor(adult_label_encoded[,i])                      # Converting the ith column into factor
    adult_label_encoded[,i]<-match(adult_label_encoded[,i],temp_factor)            # Now using the match function to map the ith column to the temp_factor
  }                                                    # Basically what happens is the match function returns the index at which
                                                       # element occurs in argument 1 in vector in argument 2      
}


write.csv(adult_label_encoded,"Label_encoded.csv",row.names=FALSE)   # Writing the label encoded values in file



adult_onehot_encoded<-adult_label_encoded

#Now doing One hot Encoding
                       # I am here adding a new column for each different value possible in the non numeric columns possible 
for(i in 1:ncol(adult_onehot_encoded))
{
  if(!is.numeric(adult_original[,i]))
  {
    for(unique_value in unique(adult_onehot_encoded[,i]))
      {
        adult_onehot_encoded[paste(colnames(adult_onehot_encoded[i]), unique_value, sep = ".")] <- ifelse(adult_onehot_encoded[,i] == unique_value, 1, 0)
      }
  }
}

write.csv(adult_onehot_encoded,"Onehot_encoded.csv",row.names=FALSE)                 # One hot encoded dataframe stored in file

 
#Sorting the Data with respect to Hours per week
adult_sorted<-adult_preprocessed

adult_sorted<-(adult_sorted[ order(adult_sorted[,13]), ])

write.csv(adult_sorted,"Sorted.csv",row.names=FALSE)           # Writing the Sorted Data


#Now Printing all the Rows with United States
adult_united_states<-adult_preprocessed[adult_preprocessed[,14]==" United-States",]
print(adult_united_states)

write.csv(adult_united_states,"Only_United_States.csv",row.names=FALSE)

#Now Soting the values with male and female separate

adult_male<-adult_preprocessed[adult_preprocessed[,10]==" Male",]
adult_female<-adult_preprocessed[!adult_preprocessed[,10]==" Male",]

write.csv(adult_male,"Adult_male.csv",row.names=FALSE)
write.csv(adult_female,"Adult_female.csv",row.names=FALSE)



