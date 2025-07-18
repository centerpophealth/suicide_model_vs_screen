####################################################################
###                     Model v Screen                           ###
###                 Load functions and packages                  ### 
####################################################################

##########################aggregation###############################

#data.table
if(require("data.table") == F){
  
  install.packages("data.table")
  require("data.table")
  
}

#keep unique values
keepUnique = function(x){
  
  paste0(unique(x), collapse = ", ")
  
}

#replace values
if(require("stringr") == F){
  
  install.packages("stringr")
  require("stringr")
  
}

########################formatting functions########################

#kable extra
if(require("kableExtra") == F){
  
  install.packages("kableExtra")
  require("kableExtra")
  
}

#fancy percentages
fancyP = function(x) format(round(x*100, 1), nsmall = 1)

#fancy n
fancyN = function(x) formatC(x, format = "d", big.mark = ",")

#fancy p value
fancyPval = function(x){
  
  x[as.numeric(x) >= .01] = 
    format(round(as.numeric(x[as.numeric(x) >= .01]), 2), nsmall = 2)
  x[as.numeric(x) < .01 & as.numeric(x) >= .001] = 
    format(round(as.numeric(x[as.numeric(x) < .01 & as.numeric(x) >= .001]), 3), nsmall = 3)  
  x[as.numeric(x) < .001] = "<0.001" 
  x[x == "1.00"] = ">0.99"
  
  x
  
}

#combine two stats
combo = function(x, y) paste0(x, " (", y, ")")

#combine two stats with CI
comboCI = function(x, y, z) paste0(x, " (", y, ", ", z, ")")

getMedianIQR <- function(x){
  
  x = round(quantile(x, c(.25, .50, .75)))
  
  paste0(x[2],
         " (",
         x[1],
         "-",
         x[3],
         ")"
  )
  
}


#ICD-10 names
formatICD <- function(x){
  
  if(require('icd.data') == F){
    install.packages("icd.data")
  }else{
    require('icd.data')
  }
  
  icd_names = data.frame(icd10cm2016[[5]])
  icd_desc  = data.frame(icd10cm2016[[6]])
  icd_dat = data.frame(name=icd_names$icd10cm2016..5.., des=icd_desc$icd10cm2016..6..)
  
  for(i in 1:length(x)){
    
    if(grepl("icd", x[i]) == T){
      
      if(x[i] == "icd10_R45851"){
        
        x[i] = "R45851: Suicidal ideation"
        
      }else if(x[i] == "icd10_SA"){
        
        x[i] = "Prior suicide attempt"
        
      }else if(x[i] == "icd10_T1491"){
        
        x[i] = "Suicide attempt"
        
      }else if(x[i] == "icd10_Z9152"){
        
        x[i] = "Z9152: History of nonsuicidal self-harm"
        
      }else if(x[i] == "icd10_Z9151"){
        
        x[i] = "Z9151: History of suicidal behaviors"
        
      }else if(x[i] == "icd10_Z915"){
        
        x[i] = "Z915: History of self-harm"
        
      }else if(x[i] == "icd10_R4588"){
        
        x[i] = "R4588: Nonsuicidal self-harm"
        
      }else if(x[i] == "age"){
        
        x[i] = "Age group (1 = 14-18 years old)"
        
      }else if(x[i] == "gender"){
        
        x[i] = "Gender (1 = Male)"
      
      }else{
        
        icd_i = substr(x[i],7,9)
        
        x[i] = 
          paste0(icd_i, ": ", icd_dat$des[which(icd_dat$name == icd_i)][1])
        
      }
      
    }
    
  }
  
  return(x)
  
}

###########################Modelling#################################
#prc
if(require("MLmetrics") == F){
  
  install.packages("MLmetrics")
  require("MLmetrics")
  
}

#sparse matrices
if(require("Matrix") == F){
  
  install.packages("Matrix")
  require("Matrix")
  
}

#clean up parallel processing
unregisterDoPar <- function () {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

#parallel processing package
if(require("doParallel") == F){
  
  install.packages("doParallel")
  require("doParallel")
  
}

#glmnet package for models
if(require("glmnet") == F){
  
  install.packages("glmnet")
  require("glmnet")
  
}
  
#pROC package for AUC
if(require("pROC") == F){
  
  install.packages("pROC")
  require("pROC")
  
}

#survival
if(require("survminer") == F){
  install.packages("survminer")
  require("survminer")
}

#survival
if(require("survival") == F){
  install.packages("survival")
  require("survival")
}

if(require("FSA") == F){
  
  install.packages('FSA')
  require("FSA")
  
}

########################Suicide codes##########################

#ICD-10 suicide code package
if(require("SuicideAttemptCodes") == F){
  #load (and install if needed) devtools package
  if(require('devtools') == F){
    
    install.packages('devtools')
    require('devtools')
    
  }
  
  #install SuicideAttemptCodes package from github
  install_github("saccosj/SuicideAttemptCodes")
  
  #load SuicideAttemptCodes package
  require("SuicideAttemptCodes")

}

