set.seed(20240521)
library(jpeg)
library(tidyverse)
library(dplyr)
library(Momocs)
library(fdasrvf)

path <- paste0("/Users/gregorymatthews/Dropbox/teeth-scriptus-pricei/data/bushbuck/")
file_list_BW_bushbuck <- list.files(path, recursive = TRUE, full.names = TRUE)

start <- Sys.time()
import_BW <- function(x){import_jpg(x)[[1]]}
teeth_BW_bushbuck <- lapply(as.list(file_list_BW_bushbuck), import_BW)
names(teeth_BW_bushbuck) <- unlist(lapply(strsplit(file_list_BW_bushbuck,"/"), function(x){x[[length(x)]]}))
names(teeth_BW_bushbuck) <- substring(names(teeth_BW_bushbuck),1,nchar(names(teeth_BW_bushbuck))-4)
end <- Sys.time()
end - start

save(teeth_BW_bushbuck, file = "./data/teeth_BW_bushbuck.RData")

#Export data to matlab

#Now do data prep for matlab
#Makes all the teeth have the same number of points
make_same_num_points <- function(x, N = 500){
  out <- resamplecurve(t(x),N)
  return(out)
}

#Make all teeth have 500 points
teeth_BW_bushbuck <- lapply(teeth_BW_bushbuck, make_same_num_points)
write.csv(do.call(rbind,teeth_BW_bushbuck),file = paste0("./data/matlab/data_bushbuck.csv"), row.names = FALSE)

#####Compute distances in matlab with bushbuck.m script
set.seed(20240521)
load("/Users/gregorymatthews/Dropbox/teeth-scriptus-pricei/data/teethdata_scriptus_pricei.RData")

toothtype <- c("LM3")
  
  n_scriptus <- length(data[[toothtype]][["scriptus"]])
  n_pricei <- length(data[[toothtype]][["pricei"]])
  
  class <- c(rep("scriptus",n_scriptus),rep("pricei",n_pricei))
  id <- c(names(data[[toothtype]][["scriptus"]]),names(data[[toothtype]][["pricei"]]),"SK4261","UW88519a")
  
  #Run this script first in matlab: bushbuck.m
  #Pariwise distances
  #First rows are scriptus and last rows are pricei
  #Row 67 is SK4261
  #Row 68 is UW 885-19a
  ddd <- read.csv(paste0("./data/matlab/pairwise_distances_bushbuck.csv"), header = FALSE)
  ddd <- as.matrix(ddd)
  ddd[67:68, ]
  image(ddd)
  
  
  mdsdat <- data.frame(x = cmdscale(ddd)[,1], 
                       y = cmdscale(ddd)[,2],
                       species = c(class,"SK4261","UW88519a"), 
                       id = id)
  
  ggplot(aes(x = x, y = y, col = species), data = mdsdat) + geom_point(size = 0.5) + theme_bw() + geom_text(label = id, size = 2, nudge_x = 0.02)
  
######Distances for partial teeth
  library(fdasrvf)
  library(parallel)
  
  #setwd("/home/gmatthews1/shapeAnalysis")
  source("../shape_completion_Matthews_et_al/R/utility.R")
  source("../shape_completion_Matthews_et_al/R/curve_functions.R")
  source("../shape_completion_Matthews_et_al/R/calc_shape_dist_partial.R")
  source("../shape_completion_Matthews_et_al/R/calc_shape_dist_complete.R")
  source("../shape_completion_Matthews_et_al/R/complete_partial_shape.R")
  source("../shape_completion_Matthews_et_al/R/impute_partial_shape.R")
  source("../shape_completion_Matthews_et_al/R/tooth_cutter.R")

  
  partial_shape  <- read.csv("../teeth-scriptus-pricei/data/bushbuck/UW 88 518a.csv", header = FALSE)
  partial_shape <- as.matrix(partial_shape)
  
  start_stop <- read.csv("../teeth-scriptus-pricei/data/bushbuck/UW 88 518astart_stop.csv", header = FALSE)
  #Visually check
  #plot(partial_shape)
  #points(start_stop[1,1],start_stop[1,2],pch = 16, col = "green")
  #points(start_stop[2,1],start_stop[2,2],pch = 16, col = "red")
  start_stop <- as.matrix(start_stop)
  
  #Ok now cut off the part i don't need.  
  start <- start_stop[1,]
  stop <- start_stop[2,]
  
  #Measure distance between start and all points
  d_start <- (partial_shape[,1] - start[1])^2 + (partial_shape[,2] - start[2])^2 
  
  d_end <- (partial_shape[,1] - stop[1])^2 + (partial_shape[,2] - stop[2])^2 
  
  
  
  if(which.min(d_start) < which.min(d_end)){
    partial_shape <- partial_shape[which.min(d_start):which.min(d_end),]
  } else {
    partial_shape <- partial_shape[c(which.min(d_start):nrow(partial_shape),1:which.min(d_end)),]
  }
  #check partial shape
  plot(t(partial_shape))
  points(start_stop[1,1],start_stop[1,2], col = "green", pch = 16)
  points(start_stop[2,1],start_stop[2,2], col = "red", pch = 16)

  partial_shape <- t(partial_shape)
  
  #Now resample it to n points 
  partial_shape <- resamplecurve(partial_shape, 100, mode = "O")
  
  plot(t(partial_shape))
  points(start_stop[1,1],start_stop[1,2], col = "green", pch = 16)
  points(start_stop[2,1],start_stop[2,2], col = "red", pch = 16)
  
  
  #Complete shape list
  load("/Users/gregorymatthews/Dropbox/teeth-scriptus-pricei/data/teethdata_scriptus_pricei.RData")
  
  complete_shape_list_scriptus <- lapply(data[["LM1"]][["scriptus"]], t)
  complete_shape_list_pricei <- lapply(data[["LM1"]][["pricei"]], t)
  
  #Dist both with and without scaling:
  dist_scriptus_size <- dist_scriptus_size_and_shape  <- c()
  for (i in 1:length(complete_shape_list_scriptus)){print(i)
  dist_scriptus_size_and_shape[i] <- calc_shape_dist_partial(complete_shape_list_scriptus[[i]], partial_shape, FALSE)
  dist_scriptus_size[i] <- calc_shape_dist_partial(complete_shape_list_scriptus[[i]], partial_shape, TRUE)
  }
  
  dist_pricei_size <- dist_pricei_size_and_shape  <- c()
  for (i in 7:length(complete_shape_list_pricei)){print(i)
    dist_pricei_size_and_shape[i] <- calc_shape_dist_partial(complete_shape_list_pricei[[i]], partial_shape, FALSE)
    dist_pricei_size[i] <- calc_shape_dist_partial(complete_shape_list_pricei[[i]], partial_shape, TRUE)
  }
  
#Put distances into data.frames
results_UW88_518a <- data.frame(
  ID = c(names(complete_shape_list_scriptus),names(complete_shape_list_pricei)),
  species = c(rep("scriptus",length(complete_shape_list_scriptus)),rep("pricei",length(complete_shape_list_pricei))),
  dist_shape = c(dist_scriptus_size,dist_pricei_size),
  dist_size_and_shape = c(dist_scriptus_size_and_shape,dist_pricei_size_and_shape)
)

plot(t(complete_shape_list_pricei[["M19C"]]))
points(t(complete_shape_list_pricei[["M19C"]])[1:500,], col = "red")
points(t(complete_shape_list_pricei[["M19C"]])[501:700,], col = "green")

plot(t(complete_shape_list_pricei[[1]]))
points(t(complete_shape_list_pricei[[1]])[1:500,], col = "red")
points(t(complete_shape_list_pricei[[1]])[501:700,], col = "green")



#results 
save(results_UW88_518a, file = "./results_UW88_518a.RData")
write.csv(results_UW88_518a, file = "./results_UW88_518a.csv", row.names = TRUE)
results_UW88_518a %>% arrange(dist_shape)

ggplot(aes(x = dist_shape, color = species), data = results_UW88_518a) + geom_density() + geom_rug() + xlim(0,0.3) + theme_bw()
ggplot(aes(x = dist_shape, color = species), data = results_UW88_518a) + geom_density() + geom_rug() + theme_bw()
ggplot(aes(x = dist_shape, y = as.factor(species),color = as.factor(species)), data = results_UW88_518a) + geom_boxplot() + geom_rug()  + theme_bw() + xlim(0,.3)
ggplot(aes(x = dist_size_and_shape, color = species), data = results_UW88_518a) + geom_density() + geom_rug() + xlim(0,250) + theme_bw()
ggplot(aes(x = dist_size_and_shape, y = as.factor(species),color = as.factor(species)), data = results_UW88_518a) + geom_boxplot() + geom_rug()  + theme_bw() + xlim(0,250)

#Visualize the best alignment
complete_shape <- complete_shape_list_scriptus[["DSCN5079"]]
complete_shape <- complete_shape_list_pricei[["M19C"]]
scale <- TRUE

d <- dim(complete_shape)[1]
#Number of points for complete_shape and partial_shape
N_complete <- dim(complete_shape)[2]
N_partial <- dim(partial_shape)[2]

t <- seq(0,1,length = 100)
x0 <- matrix(NA,ncol = 100,nrow = 2)
for (j in 1:d){
  x0[j,] <- (1-t)*partial_shape[j,N_partial] + t*partial_shape[j,1]
}

partial_shape_closed <- cbind(partial_shape,x0[,2:100])

N_complete_new <- 500
t <- seq(0,1,length = N_complete_new)

olddel <- get_cumdel(partial_shape_closed)

N <- 100
#Note: resamplecurve is using splines 
#Does this sampl ing need to 
# partial_shape_closed <- resamplecurve(partial_shape_closed,N_complete_new)
# 
# newpt <- which(t < olddel[N_partial])
# newpt <- newpt[length(newpt)]
# 
# #Resample the complete and missing parts
# partial_shape_closed_obs <- resamplecurve(partial_shape_closed[,1:newpt],N)
# partial_shape_closed_mis <- resamplecurve(partial_shape_closed[,newpt:dim(partial_shape_closed)[2]],N)

partial_shape_closed_obs <- resamplecurve(partial_shape_closed[,1:(dim(partial_shape_closed)[2] - (dim(x0)[2] - 1))],N)
partial_shape_closed_mis <- resamplecurve(partial_shape_closed[,(dim(partial_shape_closed)[2] - (dim(x0)[2] - 1)):dim(partial_shape_closed)[2]],N)

#Find the centroid of the observed part
cent1 <- apply(partial_shape_closed_obs,1, mean)

#Centering
partial_shape_closed_obs <- partial_shape_closed_obs - cent1
partial_shape_closed_mis <- partial_shape_closed_mis - cent1

if (scale == TRUE){
  #scale factor
  sc1 <- norm(partial_shape_closed_obs, type = "F")
  
  #Scaling the shape
  partial_shape_closed_obs <- partial_shape_closed_obs/sc1
  partial_shape_closed_mis <- partial_shape_closed_mis/sc1
}

minE <- Inf
#I think we are looking across all strting points around the curve?
for (j in 0:(N_complete-1)){
  #What does shiftF do??
  #Why N_complete - 1 and not just N_complete??????
  mu <- ShiftF(complete_shape[,1:(N_complete-1)],j) 
  mu <- cbind(mu,mu[,1])
  
  olddel1 <- get_cumdel(mu)
  
  N <- 100
  N_complete_new <- 100
  #library(fdasrvf)
  mu <- resamplecurve(mu,N_complete_new)
  
  #N_complete needs to be larger than N_partial for this code to work!
  newpt1 <- which(t < olddel1[N_partial])
  newpt1 <- newpt1[length(newpt1)]
  
  mu1 <- resamplecurve(mu[,1:newpt1],N) 
  mu2 <- resamplecurve(mu[,newpt1:dim(mu)[2]],N) 
  
  cent2 <- apply(mu1,1,mean)
  mu1 <- mu1 - cent2
  mu2 <- mu2 - cent2
  
  if (scale == TRUE){
    sc2=norm(mu1, type = "F")
    mu1=mu1/sc2
    mu2=mu2/sc2
  }
  
  #Finding the best rotation
  out <- find_best_rotation(partial_shape_closed_obs,mu1)
  R <- out$R
  q2new <- out$q2new
  
  mu1n <- R%*%mu1
  
  Ec <- InnerProd_Q(partial_shape_closed_obs-mu1n,partial_shape_closed_obs-mu1n)
  if (Ec < minE){
    Rbest <- R
    complete_shape_obs <- Rbest%*%mu1
    complete_shape_mis <- Rbest%*%mu2
    minE <- Ec
  }
  
}

calc_shape_dist(complete_shape_obs,partial_shape_closed_obs, mode = "O")  

plot(t(complete_shape_obs), xlim= c(-300,500))
points(t(complete_shape_mis), col = "gray" )
points(t(partial_shape_closed_obs), col = "red")

plot(t(complete_shape_obs))
points(t(complete_shape_mis), col = "gray" )
points(t(partial_shape_closed_obs), col = "red")



  