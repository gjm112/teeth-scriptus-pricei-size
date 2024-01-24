library(jpeg)
library(tidyverse)
library(dplyr)
library(Momocs)
library(fdasrvf)

#test


data <- list()
for (i in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  data[[i]] <- list()
  path <- paste0("/Users/nastaranghorbani/Documents/teeth-scriptus-pricei2/data/pricei_bw/images/Fossil/Tragelaphini/Tragelaphus/pricei/",i,"/bw")
  file_list_BW_extant <- list.files(path, recursive = TRUE, full.names = TRUE)
  
  #Import the BW image files.
  start <- Sys.time()
  import_BW <- function(x){import_jpg(x)[[1]]}
  teeth_BW_train <- lapply(as.list(file_list_BW_extant), import_BW)
  names(teeth_BW_train) <- unlist(lapply(strsplit(file_list_BW_extant,"/"), function(x){x[[length(x)]]}))
  names(teeth_BW_train) <- substring(names(teeth_BW_train),1,nchar(names(teeth_BW_train))-4)
  end <- Sys.time()
  end - start
  
  data[[i]][["pricei"]] <- teeth_BW_train
  
  
  path <- paste0("/Users/nastaranghorbani/Documents/teeth-scriptus-pricei2/data/scriptus_bw/images/Extant/Tragelaphini/Tragelaphus/scriptus/",i,"/bw")
  file_list_BW_extant <- list.files(path, recursive = TRUE, full.names = TRUE)
  
  #Import the BW image files.
  start <- Sys.time()
  import_BW <- function(x){import_jpg(x)[[1]]}
  teeth_BW_train <- lapply(as.list(file_list_BW_extant), import_BW)
  names(teeth_BW_train) <- unlist(lapply(strsplit(file_list_BW_extant,"/"), function(x){x[[length(x)]]}))
  names(teeth_BW_train) <- substring(names(teeth_BW_train),1,nchar(names(teeth_BW_train))-4)
    #substring(file_list_BW_extant, unlist(gregexpr( "JPG",file_list_BW_extant)) - 9, unlist(gregexpr( "JPG",file_list_BW_extant)) - 2)
  end <- Sys.time()
  end - start
  
  data[[i]][["scriptus"]] <- teeth_BW_train
  
  
  
  
}


#Save the list
save(data, file = "/Users/nastaranghorbani/Documents/teeth-scriptus-pricei2/data/teethdata_scriptus_pricei.RData")



#Now do data prep for matlab
#Makes all the teeth have the same number of points
make_same_num_points <- function(x, N = 500){
  out <- resamplecurve(t(x),N)
  return(out)
}

for (i in c("LM1", "LM2", "LM3", "UM1", "UM2", "UM3")) {
  data[[i]][["scriptus"]] <-
    lapply(data[[i]][["scriptus"]], make_same_num_points)
  data[[i]][["pricei"]] <-
    lapply(data[[i]][["pricei"]], make_same_num_points)
}


data_for_matlab <- list()
for (i in c("LM1", "LM2", "LM3", "UM1", "UM2", "UM3")) {print(i)
data_for_matlab[[i]] <- list()
data_for_matlab[[i]][["scriptus"]] <- do.call(rbind,data[[i]][["scriptus"]])
data_for_matlab[[i]][["pricei"]] <- do.call(rbind,data[[i]][["pricei"]])
}



for (i in c("LM1", "LM2", "LM3", "UM1", "UM2", "UM3")) {print(i)
  write.csv(data_for_matlab[[i]][["scriptus"]],file = paste0("/Users/nastaranghorbani/Documents/teeth-scriptus-pricei2/data/matlab/data_",i,"_scriptus.csv"), row.names = FALSE)
  write.csv(data_for_matlab[[i]][["pricei"]],file = paste0("/Users/nastaranghorbani/Documents/teeth-scriptus-pricei2/data/matlab/data_",i,"_pricei.csv"), row.names = FALSE)
}

# run teeth_scriptus_pricei_find-mean_combined.m in matlab




