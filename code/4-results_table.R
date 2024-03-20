
# run teeth_scriptus_pricei_find-mean_combined.m in matlab


load("/Users/nastaranghorbani/Documents/teeth-scriptus-pricei-size/data/teethdata_scriptus_pricei-size.RData")
#Scriptus is the first n rows, pricei is the last some rows. 
length(data[["LM1"]][["scriptus"]])
length(data[["LM1"]][["pricei"]])


pvals_hotelling = list()
for (i in c("LM1", "LM2", "LM3", "UM1", "UM2", "UM3")){
  
  
  
  PC_combined <- read.csv(paste0("/Users/nastaranghorbani/Documents/teeth-scriptus-pricei-size/data/matlab/PC_feat_",i,"_combined.csv"), header = FALSE)
  
  library(Hotelling)
  PC_combined$g <- c(rep(1,length(data[[i]][["scriptus"]])),
                     rep(2,length(data[[i]][["pricei"]])))
  
  results <-  hotelling.test(.~g, data = PC_combined)
  results$stats$statistic
  #results <-  hotelling.stat(PC_LM1_combined[PC_LM1_combined$g ==1,-11],PC_LM1_combined[PC_LM1_combined$g ==2,-11])
  #results$statistic
  
  #Permutation
  nsim <- 1000
  null <- c()
  for (j in 1:nsim){
    temp <- PC_combined
    temp$g <- sample(temp$g, length(temp$g),replace = FALSE)
    null[j] <-  hotelling.test(.~g, data = temp)$stat$statistic
  }
  
  
  pvals_hotelling[[i]] <- mean(null >= results$stats$statistic)
}









####################
load("/Users/nastaranghorbani/Documents/teeth-scriptus-pricei-size/data/teethdata_scriptus_pricei-size.RData")
pvals <- list()
for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){print(toothtype)
  
  n_scriptus <- length(data[[toothtype]][["scriptus"]])
  n_pricei <- length(data[[toothtype]][["pricei"]])
  
  class <- c(rep("scriptus",n_scriptus),rep("pricei",n_pricei))
  
  
  #Run this script first in matlab: pairwise_dist_scriptus_pricei.m
  #Pariwise distances
  #First rows are scriptus and last rows are pricei
  ddd <- read.csv(paste0("/Users/nastaranghorbani/Documents/teeth-scriptus-pricei-size/data/matlab/pairwise_distances_",toothtype,".csv"), header = FALSE)
  ddd <- as.matrix(ddd)
  
  #Distrance based permutation testing.  
  #Based on the test defined in Soto et al 2021
  #1. Use distances based on the shapes projected into the tangent space.  
  #to 2. Use distances in the size-shape space.  (I just need a function that computes distance between shapes tha tpreserves size.)
  
  
  Dbar11 <- sum(ddd[class == "scriptus",class == "scriptus"])/(n_scriptus^2)
  Dbar22 <- sum(ddd[class == "pricei",class == "pricei"])/(n_pricei^2)
  Dbar12 <- sum(ddd[class == "pricei",class == "scriptus"])/(n_pricei*n_pricei)
  
  S <- ((n_scriptus*n_pricei)/((n_scriptus+n_pricei)))*(2*Dbar12 - (Dbar11 + Dbar22))
  
  #Now permute
  Sperm <- c()
  nsim <- 100000
  for (i in 1:nsim){print(i)
    class_perm <- sample(class,length(class),replace = FALSE)
    Dbar11 <- sum(ddd[class_perm == "scriptus",class_perm == "scriptus"])/(n_scriptus^2)
    Dbar22 <- sum(ddd[class_perm == "pricei",class_perm == "pricei"])/(n_pricei^2)
    Dbar12 <- sum(ddd[class_perm == "pricei",class_perm == "scriptus"])/(n_pricei*n_pricei)
    
    Sperm[i] <- ((n_scriptus*n_pricei)/((n_scriptus+n_pricei)))*(2*Dbar12 - (Dbar11 + Dbar22))
  }
  
  pvals[[toothtype]] <- mean(Sperm >= S)
  
}


hist(Sperm)
abline(v = S, col = "red")




##############


hotelling_results=results$pval
pvals_hotelling
pvals



results_table <- data.frame(
  Toothtype = rep(c("LM1", "LM2", "LM3", "UM1", "UM2", "UM3"), each = 3),
  Method = c("Hotelling", "Permutation", "Distance",
             "Hotelling", "Permutation", "Distance",
             "Hotelling", "Permutation", "Distance",
             "Hotelling", "Permutation", "Distance",
             "Hotelling", "Permutation", "Distance",
             "Hotelling", "Permutation", "Distance"),
  P_Value = numeric(18), # Placeholder vector for p-values
  stringsAsFactors = FALSE
)



results_table$P_Value <- c(
  hotelling_results, pvals_hotelling[['LM1']], pvals[['LM1']],
  hotelling_results, pvals_hotelling[['LM2']], pvals[['LM2']],
  hotelling_results, pvals_hotelling[['LM3']], pvals[['LM3']],
  hotelling_results, pvals_hotelling[['UM1']], pvals[['UM1']],
  hotelling_results, pvals_hotelling[['UM2']], pvals[['UM2']],
  hotelling_results, pvals_hotelling[['UM3']], pvals[['UM3']]
)


View(results_table)


write.csv(results_table, "/Users/nastaranghorbani/Documents/teeth-scriptus-pricei-size/data/results_table.csv", row.names = FALSE, quote = FALSE)


