#Auth Token
#password @ issue
#windows/mac/ubuntu
#system("command.com")

library(tools)

trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

connect <- function() {
  system('rm -rf ~/threat_sensing_starbucks')
  system('pwd')
  system("mkdir ~/threat_sensing_starbucks")
  setwd("~/threat_sensing_starbucks")
  system("cd ~/threat_sensing_starbucks",  intern = T)
  system("git clone  https://github.com/seshadriarti/threat_sesnsing_AI_R.git", intern = T)
}


addTimeStamp<- function(filename, x){
  x = gsub(":", "_", gsub("_", "", x))
  file_name =  substr(basename(filename), 1, nchar(basename(filename)) - 4)
  print(paste(file_name, x , ".", file_ext(filename), sep=""))
  
  return ( paste(file_name, x , ".",file_ext(filename) , sep=""))
  
}

push <- function(copyfrom, isKNN, filename ){
  x <- trim(as.character(Sys.time()))
  if(isKNN){
    system(paste(paste("cp" ,copyfrom ),copyto_knn),  intern = T)
    system(paste("mv ",paste(copyto_knn,filename, sep=""), paste(copyto_knn,addTimeStamp(filename, gsub(" ","_",x)), sep=""), sep =""))
    
  } else {
    print(paste(paste("cp" ,copyfrom ),copyto_lr))
    system(paste(paste("cp" ,copyfrom ),copyto_lr),  intern = T)
    print(paste("mv ",paste(copyto_lr,filename, sep=""), paste(copyto_lr,addTimeStamp(filename, gsub(" ","_",x)), sep=""), sep =""))
    system(paste("mv ",paste(copyto_lr,filename, sep=""), paste(copyto_lr,addTimeStamp(filename, gsub(" ","_",x)), sep=""), sep =""))
    
  }
  
  #system("git config --global user.email 'seshadriarti@gmail.com'",  intern = T)
  #system("git config --global user.name 'seshadriarti'",  intern = T)
  setwd("~/threat_sensing_starbucks/threat_sesnsing_AI_R")
  system('pwd')
  system("git status",  intern = T)
  system("git add .",  intern = T)
  system("git commit -m 'test'",  intern = T)
  system("git config --global push.default simple", intern = T)
  system("git config credential.helper store", intern =T)
  system("git config credential.helper cache 157680000", intern =T)
  system(" git push https://github.com/seshadriarti/threat_sesnsing_AI_R.git", intern = T)
  #system("git push https://seshadriarti:Fall2012@github.com/seshadriarti/threat_sesnsing_AI_R.git",  intern = T)
}

#Main Function
#replace knn/lr paths of stage place to below and replace copyfrom
gitpath_knn=""
gitpath_lr =""

#replace this
copyfrom = paste(trim(paste("~/threat_sensing_starbucks/hello_mac", "")),".csv " , sep  ="")
filename ="hello_mac.csv"

copyto_knn =" ~/threat_sensing_starbucks/threat_sesnsing_AI_R/KNN/"
copyto_lr=" ~/threat_sensing_starbucks/threat_sesnsing_AI_R/Regression/"

connect()
system("echo 'This is Test'>hello_mac.csv")
push(copyfrom, FALSE, filename)
  #get most recent model



