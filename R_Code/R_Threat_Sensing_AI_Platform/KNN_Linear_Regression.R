test_data <- read.csv(file="P:/q2/For_Prototype.csv", header=TRUE, sep=",")
train_data <- read.csv(file="P:/q2/reporting_2000 - 2007 with 2005- 2012 stability.csv", header=TRUE, sep=",")
train_data <- as.data.frame(train_data)
colnames(train_data) <- c('Year', 'Country', 'F1','F2','F3','F4','F5','F6','F7','F8','F9','F10','F11','F12','F13','F14','F15','F16','F17','F18','F19','F20','F21','F22','F23','F24','F25','F26','F27','F28','F29','F30','F31','F32','F33','F34','F35','F35','F36','F37','F38','F39','Stability')
colnames(test_data) <- c('Year', 'Country', 'F1','F2','F3','F4','F5','F6','F7','F8','F9','F10','F11','F12','F13','F14','F15','F16','F17','F18','F19','F20','F21','F22','F23','F24','F25','F26','F27','F28','F29','F30','F31','F32','F33','F34','F35','F35','F36','F37','F38','F39','Stability')

crossvalidation <- function(traindata){
  
  
  
  
  
}

#Min-Max Normalization
#Find Max
findmax <- function(x) {
  return (max(x))
}
#FindMin
findmin <- function(x) {
  return (min(x))
}
#MMN Normalization
normalize <- function(x) {
  #print(class(x))
  max_x= findmax(train_data[,3:42])
  min_x = findmin(train_data[,3:42])
  return ((x - min_x) / (max_x - min_x)) 
}

makeband <- function(stability){
  
  i <- c(1)
  while(i<= length(stability)){
    
    if( stability[i]>=0 && stability[i]<2){
      stability[i]=1
      
    }
    else if( stability[i]>=2 &&  stability[i]<4){
      stability[i]=2
      
    }
    
    else if (stability[i]>=4 &&   stability[i]<6){
      stability[i]=3
      
    }
    else if ( stability[i]>=6 &&    stability[i]<8){
      stability[i]=4
      
    }
    else if ( stability[i]>=8 &&    stability[i]<10){
      stability[i]=5
      
    }
    
    i = i+c(1)
    
  }
  return(stability) 
  
}

#Calling the Normalized data
train_n <- as.data.frame(lapply((train_data[,3:42]), normalize))
test_n <- as.data.frame(lapply((test_data[,3:42]), normalize))
stability = makeband(train_data[,43])
train_data["Stability"] <-stability
#print(train_data["Stability"])
stability_test = makeband(test_data[,43])
test_data["Stability"] <-stability_test
#print(test_data["Stability"])



#KNN 
require(class)
#caret package 
install.packages('caret', dependencies = TRUE)
library(caret)
s <- c(3) 
print('******************************************************')

m1 <- knn( train =train_n, test =test_n, cl= train_data[,43], 3)
print(m1)
accuracy<- table(m1,test_data[,43] )
print(confusionMatrix(accuracy))

print('******************************************************')
cat(paste('For ',test_data[1,2],'          Year ', as.numeric(test_data[1,1])+5,' Expected ',test_data[1,43],' Predicted ',m1[1],"\n"))
cat(paste('For ',test_data[2,2],' Year ', as.numeric(test_data[2,1])+5,' Expected ',test_data[2,43],' Predicted ',m1[2],"\n"))
cat(paste('For ',test_data[3,2],'    Year ', as.numeric(test_data[3,1])+5,' Expected ',test_data[3,43],' Predicted ',m1[3],"\n"))
cat(paste('For ',test_data[4,2],'      Year ', as.numeric(test_data[4,1])+5,' Expected ',test_data[4,43],' Predicted ',m1[4],"\n"))
print('******************************************************')
cat(paste('For ',test_data[5,2],'          Year ', as.numeric(test_data[5,1])+5,' Expected ',test_data[5,43],' Predicted ',m1[5],"\n"))
cat(paste('For ',test_data[6,2],' Year ', as.numeric(test_data[6,1])+5,' Expected ',test_data[6,43],' Predicted ',m1[6],"\n"))
cat(paste('For ',test_data[7,2],'    Year ', as.numeric(test_data[7,1])+5,' Expected ',test_data[7,43],' Predicted ',m1[7],"\n"))
cat(paste('For ',test_data[8,2],'      Year ', as.numeric(test_data[8,1])+5,' Expected ',test_data[8,43],' Predicted ',m1[8],"\n"))
print('******************************************************')
test_data["Stability"] <- m1

#write to CSV
setwd("P:/q2/")
write.csv(test_data, file = "KNN.csv",  row.names = FALSE)


#LINEAR Regression 
train_n["Stability"] <-NA
test_n["Stability"] <-NA
train_n$Stability =  train_data$Stability
mdl = glm(formula = Stability ~ F1+F2+F3+F4+F5+F6+F7+F8+F9+F10+F11+F12+F13+F14+F15+F16+F17+F18+F19+F20+F21+F22+F23+F24+F25+F26+F27+F28+F29+F30+F31+F32+F33+F34+F35+F36+F37+F38+F39,data=train_n , family = binomial)
testPoint1 = data.frame(test_n)
predict(mdl,testPoint1 ,type="response")

print("***********LINEAR REGRESSION********************************")
install.packages('caret', dependencies = TRUE)
library(caret)
accuracy<- table(m1,test_data[,43] )
print(confusionMatrix(accuracy))

print('************************LINEAR REGRESSION******************************')
cat(paste('For ',test_data[1,2],'          Year ', as.numeric(test_data[1,1])+5,' Expected ',test_data[1,43],' Predicted ',m1[1],"\n"))
cat(paste('For ',test_data[2,2],' Year ', as.numeric(test_data[2,1])+5,' Expected ',test_data[2,43],' Predicted ',m1[2],"\n"))
cat(paste('For ',test_data[3,2],'    Year ', as.numeric(test_data[3,1])+5,' Expected ',test_data[3,43],' Predicted ',m1[3],"\n"))
cat(paste('For ',test_data[4,2],'      Year ', as.numeric(test_data[4,1])+5,' Expected ',test_data[4,43],' Predicted ',m1[4],"\n"))
print('******************************************************')
cat(paste('For ',test_data[5,2],'          Year ', as.numeric(test_data[5,1])+5,' Expected ',test_data[5,43],' Predicted ',m1[5],"\n"))
cat(paste('For ',test_data[6,2],' Year ', as.numeric(test_data[6,1])+5,' Expected ',test_data[6,43],' Predicted ',m1[6],"\n"))
cat(paste('For ',test_data[7,2],'    Year ', as.numeric(test_data[7,1])+5,' Expected ',test_data[7,43],' Predicted ',m1[7],"\n"))
cat(paste('For ',test_data[8,2],'      Year ', as.numeric(test_data[8,1])+5,' Expected ',test_data[8,43],' Predicted ',m1[8],"\n"))
print('*************************LINEAR REGRESSION*****************************')

